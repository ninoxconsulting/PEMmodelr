#' Calculate model accuracy metrics
#'
#' Calculates internal ML metrics and PEM specific spatial and proportional metrics.
#' This currently includes: accuracy, mcc, spatial metrics for primary, primary + alternate,
#' and primary + alternate + fuzzy for theta adjusts set at 0, 0.5, 1. Also aspatial proportions
#' for primary+alternate+fuzzy with the same 3 theta settings
#'
#' @param pred_data a data.frame with mapunit1, mapunit2, and .pred columns
#' @param fuzz_matrix is the fuzzy values matrix from the map key giving partial correct points
#' for near misses
#' @param theta the function always returns values for theta 0 and theta 1.
#' The theta setting sets an intermediate theta setting to report efault set to 0.5
#' @export
#' @examples
#' \dontrun{
#' acc_metrics(pred_data, fuzz_matrix, theta = 0.5)
#' }
acc_metrics <- function(pred_data, fuzz_matrix, theta = NULL) {
  # # testing lines
  # pred_data <- pred_all
  # fuzz_matrix <- fuzz_matrix # note these are characters
  # theta <- theta
  # # # end testing line


  # generate a range of thetas if NULL or specified.
  if (is.null(theta)) {
    theta_seq <- seq(0, 1, by = 0.5)
  } else {
    theta_seq <- c(0, theta, 1)
  }

  # convert to character from factor
  pred_data <- pred_data |>
    dplyr::mutate_if(is.factor, as.character)

  # add the fuzzy value for mapunit 1 and predicted
  data1 <- dplyr::left_join(pred_data, fuzz_matrix, by = c("mapunit1" = "target", ".pred_class" = "Pred")) |>
    dplyr::rename("p_fuzzval" = .data$fVal)

  if (any(is.na(data1$p_fuzzval))) {
    cli::cli_alert_warning("Some combination of units are not included in fuzzy matrix table. Please review these and update the table")
    # convert NA to a zero value
    data1 <- data1 |>
      dplyr::mutate(p_fuzzval = ifelse(is.na(.data$p_fuzzval), 0, .data$p_fuzzval))
  }

  fuzzy_lookup <- fuzz_matrix |>
    dplyr::filter(.data$fVal > 0) |>
    dplyr::mutate(key = paste(Pred, target, sep = "_")) |>
    dplyr::select(.data$key, .data$fVal)


  # add the fuzzy value for mapunit 2 and predicted
  data2 <- dplyr::left_join(data1, fuzz_matrix, by = c("mapunit2" = "target", ".pred_class" = "Pred")) |>
    dplyr::rename("alt_fuzzval" = .data$fVal)


  ## 2. selects the neighbour with max value (based on primary and seconday),
  ## then group by the highest vale neighbour option.
  pdata <- data2 |>
    dplyr::rowwise() |>
    dplyr::mutate(pa_fuzzval = max(.data$p_fuzzval, .data$alt_fuzzval, na.rm = TRUE)) |>
    dplyr::group_by(.data$id) |>
    dplyr::slice_max(abs(.data$pa_fuzzval), n = 1, with_ties = FALSE) |>
    dplyr::distinct(.data$id, .keep_all = TRUE) |>
    dplyr::ungroup() |>
    data.frame()

  # calculate the number of records where predcition = mapunit values
  # full length of table
  pdata <- pdata |>
    dplyr::mutate(
      p_Val = ifelse(.data$mapunit1 == .data$.pred_class, 1, 0),
      alt_Val = ifelse(.data$mapunit2 == .data$.pred_class, 1, 0)
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(pa_Val = max(.data$p_Val, .data$alt_Val, na.rm = TRUE)) |>
    dplyr::select(-.data$alt_Val) |>
    data.frame() |>
    dplyr::add_count(.data$mapunit1, name = "trans.tot") |>
    dplyr::add_count(.data$.pred_class, name = "pred.tot") |>
    dplyr::mutate_if(is.character, as.factor)


  aspat_pdata <- pdata

  # convert the characters to vector and ensure levels are the same to use ml acc metrics
  # get all unique mapunit classes
  targ.lev <- as.data.frame(levels(pdata$mapunit1)) |>
    dplyr::rename(levels = 1) |>
    droplevels()

  # get all unique predicted classes
  pred.lev <- as.data.frame(levels(pdata$.pred_class)) |>
    dplyr::rename(levels = 1) |>
    droplevels()

  # check for levels predicted that were not in the transect
  add.pred.lev <- dplyr::anti_join(pred.lev, targ.lev, by = "levels")
  # add.pred.lev <- as.factor("ICHmc1_99")

  # if all levels were contained in the
  if (length(add.pred.lev > 0)) {
    pdata <- pdata |>
      dplyr::mutate(
        mapunit.new = ifelse(pdata$.pred_class %in% add.pred.lev, as.character(.data$.pred_class), as.character(.data$mapunit1)),
        trans.tot.new = ifelse(.data$.pred_class %in% add.pred.lev, 0, .data$trans.tot)
      ) |>
      dplyr::mutate_if(is.character, as.factor) |>
      dplyr::mutate(trans.tot = .data$trans.tot.new) |>
      dplyr::select(-.data$trans.tot.new, -mapunit1) |>
      dplyr::rename("mapunit1" = mapunit.new)
    #   mutate(pred.new = ifelse(mapunit.new %in% add.pred.lev, as.character(mapunit), as.character(.pred_class))) |>
    #     mutate(mapunit = mapunit.new, .pred_class = pred.new)
  }
  # } else {
  #   pdata <- pdata |>
  #     dplyr::mutate(mapunit.new = as.character(.data$mapunit1)) |>
  #     dplyr::mutate_if(is.character, as.factor)
  # }

  # ### harmonize factor levels
  targ.lev <- levels(pdata$mapunit1)
  pred.lev <- levels(pdata$.pred_class)
  levs <- c(targ.lev, pred.lev) |> unique()

  # generate a new mpaunit with all levs
  pdata$mapunit1 <- factor(pdata$mapunit1, levels = levs)
  pdata$.pred_class <- factor(pdata$.pred_class, levels = levs)

  pdata <- pdata |>
    dplyr::mutate(no.classes = length(levs)) |>
    dplyr::select(-pred.tot)

  # perhaps need predicted tot still in here

  ### 1)machine learning stats
  if (length(levels(pdata$mapunit1)) == 1) {
    allunits <- levels(unique(pdata$mapunit1, pdata$mapunit2))

    if (length(allunits) == 1) {
      allunits <- c(allunits, "NA")
    }

    levels(pdata$mapunit1) <- allunits
    levels(pdata$mapunit2) <- allunits
    levels(pdata$.pred_class) <- allunits

    cli::cli_alert_warning("Only one factor level in this slice, review metrics with caution")
  }

  acc <- pdata |>
    yardstick::accuracy(.data$mapunit1, .data$.pred_class, na_rm = TRUE) |>
    dplyr::select(.data$.estimate) |>
    as.numeric()

  mcc <- pdata |>
    yardstick::mcc(.data$mapunit1, .data$.pred_class, na_rm = TRUE) |>
    dplyr::select(.data$.estimate) |>
    as.numeric()

  kap <- pdata |>
    yardstick::kap(.data$mapunit1, .data$.pred_class, na.rm = TRUE) |>
    dplyr::select(.data$.estimate) |>
    as.numeric()


  ### 2) spatial stats

  # generate a new data summary frame with one line per mapunit
  # sum the number of correct values for each type
  # remove some cols.

  spatial_acc <- pdata |>
    dplyr::mutate(trans.sum = dplyr::n(), acc = acc, kap = kap) |>
    ### here the problem is differing number of mapunit vs .pred_class
    dplyr::group_by(.data$mapunit1) |>
    dplyr::mutate(spat_p_correct = sum(.data$p_Val)) |>
    dplyr::mutate(spat_pa_correct = sum(.data$pa_Val)) |>
    dplyr::mutate(spat_paf_correct = sum(.data$pa_fuzzval)) |>
    # dplyr::ungroup() |>
    dplyr::select(
      -.data$id, -.data$mapunit2, -.data$.pred_class, -.data$p_fuzzval,
      -.data$pa_fuzzval, -.data$p_Val, -.data$pa_Val, -.data$alt_fuzzval
    ) |>
    dplyr::distinct()

  # calculate the proportion of correct spatial values / the total number that occur (per mapunit)

  spatial_acc <- spatial_acc |>
    dplyr::mutate(spat_p = .data$spat_p_correct / .data$trans.tot) |>
    dplyr::mutate(spat_pa = .data$spat_pa_correct / .data$trans.tot) |>
    dplyr::mutate(spat_paf = .data$spat_paf_correct / .data$trans.tot) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ replace(., is.nan(.), 0))) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ replace(., is.infinite(.), 0)))


  # cycle through the theta values and generate out puts for each theta
  for (th in theta_seq) {
    # th <- theta_seq[1]

    spatial_acc <- spatial_acc |>
      dplyr::rowwise() |>
      dplyr::mutate(
        !!paste0("spat_p_theta_wt_", th) := th * (1 / no.classes) + (1 - th) * (trans.tot / trans.sum),
        !!paste0("spat_p_theta_work_", th) := get(paste0("spat_p_theta_wt_", th)) * spat_p,
        !!paste0("spat_pa_theta_work_", th) := get(paste0("spat_p_theta_wt_", th)) * spat_pa,
        !!paste0("spat_paf_theta_work_", th) := get(paste0("spat_p_theta_wt_", th)) * spat_paf
      ) |>
      dplyr::ungroup()
  }

  # Then summarize across all theta values
  for (th in theta_seq) {
    # th <- theta_seq[1]
    spatial_acc <- spatial_acc |>
      dplyr::mutate(
        !!paste0("spat_p_theta_", th) := (sum(get(paste0("spat_p_theta_work_", th)))),
        !!paste0("spat_pa_theta_", th) := (sum(get(paste0("spat_pa_theta_work_", th)))),
        !!paste0("spat_paf_theta_", th) := (sum(get(paste0("spat_paf_theta_work_", th))))
      )
  }

  spatial_acc <- spatial_acc |>
    dplyr::select(-dplyr::contains("wt")) |>
    dplyr::select(-dplyr::contains("work")) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("spat"), round, 3))


  # 3) calculate aspatial metrics (overall and mapunit % correct)

  aspatial_mapunit <- aspat_pdata |>
    dplyr::add_count(.data$mapunit1, name = "trans.tot") |>
    dplyr::select(.data$mapunit1, .data$trans.tot) |>
    dplyr::distinct() #|>
  # dplyr::mutate(trans.sum = sum(.data$trans.tot))

  aspatial_pred <- aspat_pdata |>
    dplyr::select(.data$.pred_class) |>
    dplyr::add_count(.data$.pred_class, name = "pred.tot") |>
    dplyr::distinct()


  # could potentially be left_join? or full_join
  aspatial_acc <- dplyr::full_join(aspatial_mapunit, aspatial_pred, by = c("mapunit1" = ".pred_class")) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ tidyr::replace_na(., 0))) |>
    dplyr::mutate(trans.sum = sum(.data$trans.tot, na.rm = TRUE)) |>
    dplyr::mutate(no.classes = length(unique(.data$mapunit1))) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      aspat_p_correct = min(.data$trans.tot, .data$pred.tot),
      aspat_p = .data$aspat_p_correct / .data$trans.tot,
      diff_p = .data$pred.tot - .data$trans.tot
    ) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ tidyr::replace_na(., 0))) |>
    dplyr::ungroup()


  #### --- primary plus alternate
  # There is still a problem here - not sure why this is not working

  aspat_pred2 <- aspat_pdata |>
    dplyr::mutate_if(is.factor, as.character) |>
    dplyr::mutate(.pred_class = ifelse(.data$.pred_class != .data$mapunit1, as.character(.data$mapunit2), as.character(.data$.pred_class))) |>
    dplyr::add_count(.pred_class, name = "pred.tot2") |>
    dplyr::select(.pred_class, pred.tot2) |>
    dplyr::distinct()

  aspatial_pa_acc <- dplyr::left_join(aspatial_acc, aspat_pred2, by = c("mapunit1" = ".pred_class")) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ tidyr::replace_na(., 0))) |>
    dplyr::rowwise() |>
    dplyr::mutate(aspat_pa_correct = min(.data$trans.tot, .data$pred.tot2)) |>
    dplyr::mutate(
      aspat_pa = .data$aspat_pa_correct / .data$trans.tot,
      diff_pa = .data$pred.tot2 - .data$trans.tot
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ tidyr::replace_na(., 0)))



  ### optimize rebalancing of points using deficit and fuzzy matrix

  donors <- aspatial_pa_acc |> dplyr::filter(.data$diff_pa > 0)
  recipients <- aspatial_pa_acc |> dplyr::filter(.data$diff_pa < 0)

  # Create copies of the original values
  aspatial_paf_acc <- aspatial_pa_acc |>
    dplyr::mutate(
      pred.tot.paf = .data$pred.tot2,
      diff_paf = .data$diff_pa
    )

  # Redistribution loop
  max_transfer <- 0
  # i=1
  for (i in seq_len(nrow(donors))) {
    # i = 1
    donor <- donors$mapunit1[i]
    donor_excess <- donors$diff_pa[i]
    # j=4
    for (j in seq_len(nrow(recipients))) {
      # j = i
      recipient <- recipients$mapunit1[j]
      recipient_deficit <- abs(recipients$diff_pa[j])

      key <- paste(donor, recipient, sep = "_")
      fuzzy_val <- fuzzy_lookup$fVal[fuzzy_lookup$key == key]

      if (length(fuzzy_val) > 0 && fuzzy_val > 0) {
        recipient_max_allowed <- recipient_deficit * fuzzy_val
        max_transfer <- min(
          donor_excess * fuzzy_val,
          recipient_max_allowed
        )

        if (max_transfer > 0) {
          # Update donor and recipient
          aspatial_paf_acc$pred.tot.paf[aspatial_paf_acc$mapunit1 == donor] <-
            aspatial_paf_acc$pred.tot.paf[aspatial_paf_acc$mapunit1 == donor] - max_transfer

          aspatial_paf_acc$pred.tot.paf[aspatial_paf_acc$mapunit1 == recipient] <-
            aspatial_paf_acc$pred.tot.paf[aspatial_paf_acc$mapunit1 == recipient] + max_transfer

          aspatial_paf_acc$diff_pa[aspatial_paf_acc$mapunit1 == donor] <-
            aspatial_paf_acc$diff_pa[aspatial_paf_acc$mapunit1 == donor] - max_transfer

          aspatial_paf_acc$diff_pa[aspatial_paf_acc$mapunit1 == recipient] <-
            aspatial_paf_acc$diff_pa[aspatial_paf_acc$mapunit1 == recipient] + max_transfer

          donor_excess <- donor_excess - max_transfer
          if (donor_excess <= 0) break
        }
      }
    }
  }

  aspatial_acc_paf <- aspatial_paf_acc |>
    dplyr::mutate(
      aspat_paf_correct = pmin(.data$trans.tot, .data$pred.tot.paf),
      aspat_paf = .data$aspat_paf_correct / .data$trans.tot,
      aspat_paf_theta1 = mean(.data$aspat_paf, na.rm = TRUE),
      aspat_paf_theta0 = sum(.data$aspat_paf_correct) / sum(.data$trans.tot),
      no.classes = length(unique(.data$mapunit1))
    )


  for (th in theta_seq) {
    aspatial_acc_paf <- aspatial_acc_paf |>
      dplyr::rowwise() |>
      dplyr::mutate(
        !!paste0("aspat_theta_wt_", th) := th * (1 / no.classes) + (1 - th) * (.data$trans.tot / .data$trans.sum),
        !!paste0("aspat_p_theta_work_", th) := get(paste0("aspat_theta_wt_", th)) * .data$aspat_p,
        !!paste0("aspat_pa_theta_work_", th) := get(paste0("aspat_theta_wt_", th)) * .data$aspat_pa,
        !!paste0("aspat_paf_theta_work_", th) := get(paste0("aspat_theta_wt_", th)) * .data$aspat_paf
      ) |>
      dplyr::ungroup()
  }


  # Then summarize across all theta values
  for (th in theta_seq) {
    aspatial_acc_paf <- aspatial_acc_paf |>
      dplyr::mutate(
        !!paste0("aspat_p_theta_", th) := sum(get(paste0("aspat_p_theta_work_", th))),
        !!paste0("aspat_pa_theta_", th) := sum(get(paste0("aspat_pa_theta_work_", th))),
        !!paste0("aspat_paf_theta_", th) := sum(get(paste0("aspat_paf_theta_work_", th)))
      )
  }

  aspatial_acc <- aspatial_acc_paf |>
    dplyr::select(-contains("work")) |>
    dplyr::select(-contains("wt"), -.data$trans.tot, -.data$no.classes, -.data$trans.sum) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("aspat"), round, 3))

  accuracy_stats <- dplyr::left_join(spatial_acc, aspatial_acc, by = "mapunit1") |>
    as.data.frame() |>
    dplyr::select(-contains("wt")) |>
    dplyr::select(-contains("work")) |>
    dplyr::select(.data$mapunit1, .data$trans.sum, .data$trans.tot, .data$pred.tot, .data$no.classes, dplyr::everything())

  return(accuracy_stats)
}











# acc_metrics <- function(pred_data, fuzz_matrix, theta = 0.5) {
#   # # testing lines
#   #   pred_data = pred_all
#   #     fuzz_matrix = fmat
#   #     theta = 0.5
#   # # # end testing line
#
#   preds <- c("id", "mapunit1", "mapunit2", ".pred_class")
#   pred_data <- pred_data |>
#     dplyr::select(dplyr::any_of(preds)) |>
#     dplyr::mutate_if(is.factor, as.character)
#   pred_data <- replace(pred_data, is.na(pred_data), 0)
#
#   # add the fuzzy value for mapunit 1 and predicted
#   data1 <- dplyr::left_join(pred_data, fuzz_matrix, by = c("mapunit1" = "target", ".pred_class" = "Pred")) |>
#     dplyr::mutate_if(is.character, as.factor) |>
#     dplyr::mutate(fVal = ifelse(is.na(.data$fVal), 0, .data$fVal)) |>
#     dplyr::rename("p_fuzzval" = .data$fVal)
#
#   # add the fuzzy value for mapunit 2 and predicted
#   data2 <- dplyr::left_join(data1, fuzz_matrix, by = c("mapunit2" = "target", ".pred_class" = "Pred")) |>
#     dplyr::mutate_if(is.character, as.factor) |>
#     dplyr::mutate(fVal = ifelse(is.na(.data$fVal), 0, .data$fVal)) |> # replace(is.na(data1$fVal), 0) |>
#     dplyr::rename("alt_fuzzval" = .data$fVal)
#
#
#   ## 2. selects the neighbour with max value (based on primary and seconday),,
#   ## then group by the highest neighbour option.
#   pdata <- data2 |>
#     dplyr::rowwise() |>
#     dplyr::mutate(pa_fuzzval = max(.data$p_fuzzval, .data$alt_fuzzval)) |>
#     dplyr::group_by(.data$id) |>
#     dplyr::top_n(1, abs(.data$pa_fuzzval)) |>
#     dplyr::distinct(.data$id, .keep_all = TRUE) |>
#     data.frame()
#
#   pdata <- pdata |>
#     dplyr::mutate_if(is.factor, as.character) |>
#     dplyr::mutate(
#       p_Val = ifelse(.data$mapunit1 == .data$.pred_class, 1, 0),
#       alt_Val = ifelse(.data$mapunit2 == .data$.pred_class, 1, 0)
#     ) |>
#     dplyr::mutate(
#       alt_Val = ifelse(is.na(.data$alt_Val), 0, .data$alt_Val)
#     ) |>
#     dplyr::rowwise() |>
#     dplyr::mutate(pa_Val = max(.data$p_Val, .data$alt_Val)) |>
#     dplyr::select(-.data$alt_Val) |>
#     dplyr::mutate_if(is.character, as.factor) |>
#     data.frame() |>
#     dplyr::add_count(.data$mapunit1, name = "trans.tot") |>
#     dplyr::add_count(.data$.pred_class, name = "pred.tot")
#
#   # get all unique mapunit classes
#   targ.lev <- as.data.frame(levels(pdata$mapunit1)) |>
#     dplyr::rename(levels = 1) |>
#     droplevels()
#
#   # get all unique predicted classes
#   pred.lev <- as.data.frame(levels(pdata$.pred_class)) |>
#     dplyr::rename(levels = 1) |>
#     droplevels()
#
#   # check for levels predicted that were not in the transect
#   add.pred.lev <- dplyr::anti_join(pred.lev, targ.lev, by = "levels")
#
#   if (length(add.pred.lev > 0)) {
#     pdata <- pdata |>
#       dplyr::mutate(
#         mapunit.new = ifelse(pdata$.pred_class %in% add.pred.lev, as.character(.data$.pred_class), as.character(.data$mapunit1)),
#         trans.tot.new = ifelse(.data$.pred_class %in% add.pred.lev, 0, .data$trans.tot)
#       ) |>
#       dplyr::mutate_if(is.character, as.factor) |>
#       dplyr::mutate(trans.tot = .data$trans.tot.new) |>
#       dplyr::select(-.data$trans.tot.new)
#     #   mutate(pred.new = ifelse(mapunit.new %in% add.pred.lev, as.character(mapunit), as.character(.pred_class))) |>
#     #     mutate(mapunit = mapunit.new, .pred_class = pred.new)
#   } else {
#     pdata <- pdata |>
#       dplyr::mutate(mapunit.new = as.character(.data$mapunit1)) |>
#       dplyr::mutate_if(is.character, as.factor)
#   }
#
#   # ### harmonize factor levels
#   targ.lev <- levels(pdata$mapunit1)
#   pred.lev <- levels(pdata$.pred_class)
#   levs <- c(targ.lev, pred.lev) |> unique()
#   # generate a new mpaunit with all levs
#   pdata$mapunit1 <- factor(pdata$mapunit1, levels = levs)
#   pdata$.pred_class <- factor(pdata$.pred_class, levels = levs)
#
#   # pdata <- .harmonize_factors(pdata)
#
#   pdata <- pdata |>
#     tidyr::drop_na(.data$mapunit1) |>
#     dplyr::mutate(no.classes = length(levs)) |>
#     dplyr::select(-.data$pred.tot)
#
#   # perhaps need predicted tot still in here
#
#   ### 1)machine learning stats
#   if (length(levels(pdata$mapunit1)) == 1) {
#     allunits <- levels(unique(pdata$mapunit1, pdata$mapunit2))
#     if (length(allunits) == 1) {
#       allunits <- c(allunits, "NA")
#     }
#
#     levels(pdata$mapunit1) <- allunits
#     levels(pdata$mapunit2) <- allunits
#     levels(pdata$.pred_class) <- allunits
#
#     cli::cli_alert_warning("Only one factor level in this slice, review metrics with caution")
#   }
#
#   acc <- pdata |>
#     yardstick::accuracy(.data$mapunit1, .data$.pred_class, na_rm = TRUE) |>
#     dplyr::select(.data$.estimate) |>
#     as.numeric() |>
#     round(3)
#
#   mcc <- pdata |>
#     yardstick::mcc(.data$mapunit1, .data$.pred_class, na_rm = TRUE) |>
#     dplyr::select(.data$.estimate) |>
#     as.numeric() |>
#     round(3)
#
#   # sens <- data |> sens(mapunit, .pred_class, na_rm = TRUE)
#   # spec <- data |> yardstick::spec(mapunit, .pred_class, na_rm = TRUE)
#   # prec <- data |> precision(mapunit, .pred_class, na.rm = TRUE)
#   # recall <- data |> recall(mapunit, .pred_class, na.rm = TRUE)
#   # fmeas <- data |> f_meas(mapunit, .pred_class, na.rm = TRUE)
#   kap <- pdata |>
#     yardstick::kap(.data$mapunit1, .data$.pred_class, na.rm = TRUE) |>
#     dplyr::select(.data$.estimate) |>
#     as.numeric() |>
#     round(3)
#
#   ### 2) spatial stats
#   spatial_acc <- pdata |>
#     dplyr::mutate(trans.sum = dplyr::n(), acc = acc, kap = kap) |>
#     ### here the problem is differing number of mapunit vs .pred_class
#     dplyr::group_by(.data$mapunit.new) |>
#     dplyr::mutate(spat_p_correct = sum(.data$p_Val)) |>
#     dplyr::mutate(spat_pa_correct = sum(.data$pa_Val)) |>
#     dplyr::mutate(spat_pf_correct = sum(.data$p_fuzzval)) |>
#     dplyr::mutate(spat_paf_correct = sum(.data$pa_fuzzval)) |>
#     dplyr::ungroup() |>
#     dplyr::select(
#       -.data$id, -.data$mapunit1, -.data$mapunit2, -.data$.pred_class, -.data$p_fuzzval,
#       -.data$pa_fuzzval, -.data$p_Val, -.data$pa_Val, -.data$alt_fuzzval
#     ) |>
#     dplyr::distinct()
#
#   spatial_acc <- spatial_acc |>
#     dplyr::mutate(spat_p = .data$spat_p_correct / .data$trans.tot) |>
#     dplyr::mutate(spat_pa = .data$spat_pa_correct / .data$trans.tot) |>
#     dplyr::mutate(spat_pf = .data$spat_pf_correct / .data$trans.tot) |>
#     dplyr::mutate(spat_paf = .data$spat_paf_correct / .data$trans.tot) |>
#     dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ replace(., is.nan(.), 0))) |>
#     dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ replace(., is.infinite(.), 0)))
#
#   spatial_acc <- spatial_acc |>
#     dplyr::mutate(
#       spat_p_theta1 = mean(.data$spat_p),
#       spat_p_theta0 = sum(.data$spat_p_correct) / .data$trans.sum,
#       spat_pa_theta1 = mean(.data$spat_pa),
#       spat_pa_theta0 = sum(.data$spat_pa_correct) / .data$trans.sum,
#       spat_paf_theta1 = mean(.data$spat_paf),
#       spat_paf_theta0 = sum(.data$spat_paf_correct) / .data$trans.sum
#     ) |>
#     dplyr::rowwise() |>
#     dplyr::mutate(
#       spat_p_theta_wt = theta * (1 / .data$no.classes) + (1 - theta) * (.data$trans.tot / .data$trans.sum),
#       spat_pa_theta_wt = theta * (1 / .data$no.classes) + (1 - theta) * (.data$trans.tot / .data$trans.sum),
#       spat_paf_theta_wt = theta * (1 / .data$no.classes) + (1 - theta) * (.data$trans.tot / .data$trans.sum),
#       spat_p_theta_work = .data$spat_p_theta_wt * .data$spat_p,
#       spat_pa_theta_work = .data$spat_pa_theta_wt * .data$spat_pa,
#       spat_paf_theta_work = .data$spat_paf_theta_wt * .data$spat_paf
#     ) |>
#     dplyr::ungroup() |>
#     dplyr::mutate(
#       spat_p_theta.5 = sum(.data$spat_p_theta_work),
#       spat_pa_theta.5 = sum(.data$spat_pa_theta_work),
#       spat_paf_theta.5 = sum(.data$spat_paf_theta_work)
#     ) |>
#     dplyr::select(
#       -.data$spat_p_theta_wt, -.data$spat_p_theta_work, -.data$spat_pa_theta_wt,
#       -.data$spat_pa_theta_work, -.data$spat_paf_theta_wt, -.data$spat_paf_theta_work
#     ) |>
#     dplyr::distinct() |>
#     dplyr::rename(mapunit1 = .data$mapunit.new)
#
#   # 3) calculate aspatial metrics (overall and mapunit % correct)
#   aspatial_mapunit <- pdata |>
#     dplyr::select(.data$mapunit1) |>
#     dplyr::add_count(.data$mapunit1, name = "trans.tot") |>
#     dplyr::distinct() |>
#     dplyr::mutate(trans.sum = sum(.data$trans.tot))
#   #
#   aspatial_pred <- pdata |>
#     dplyr::select(.data$.pred_class) |>
#     dplyr::add_count(.data$.pred_class, name = "pred.tot") |>
#     dplyr::distinct()
#
#   aspatial_acc <- dplyr::full_join(aspatial_mapunit, aspatial_pred, by = c("mapunit1" = ".pred_class")) |>
#     dplyr::select(.data$mapunit1, .data$trans.tot, .data$pred.tot) |>
#     dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ tidyr::replace_na(., 0))) |>
#     dplyr::mutate(trans.sum = sum(.data$trans.tot)) |>
#     dplyr::mutate(no.classes = length(unique(.data$mapunit1))) |>
#     dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ tidyr::replace_na(., 0))) |>
#     dplyr::rowwise() |>
#     dplyr::mutate(
#       aspat_p = min((.data$trans.tot / .data$trans.tot), (.data$pred.tot / .data$trans.tot)),
#       aspat_p_wtd = min((.data$trans.tot / .data$trans.sum), (.data$pred.tot / .data$trans.sum))
#     ) |>
#     dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ replace(., is.nan(.), 0))) |>
#     dplyr::ungroup() |>
#     dplyr::mutate(
#       aspat_p_theta0 = sum(.data$aspat_p_wtd),
#       aspat_p_theta1 = mean(.data$aspat_p)
#     ) |>
#     dplyr::rowwise() |>
#     dplyr::mutate(aspat_p_theta_wt = theta * (1 / .data$no.classes) + (1 - theta) * (.data$trans.tot / .data$trans.sum)) |> #
#     dplyr::mutate(aspat_p_theta_work = .data$aspat_p_theta_wt * .data$aspat_p_wtd) |>
#     dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ replace(., is.nan(.), 0))) |>
#     dplyr::ungroup() |>
#     dplyr::mutate(aspat_p_theta.5 = sum(.data$aspat_p_theta_wt * .data$aspat_p)) |>
#     dplyr::select(-.data$aspat_p_theta_wt, -.data$aspat_p_theta_work, -.data$no.classes, -.data$trans.sum, -.data$trans.tot) |>
#     dplyr::ungroup() |>
#     dplyr::distinct()
#
#   #### --- primary plus alternate
#
#   data_pa <- pdata |>
#     dplyr::mutate_if(is.factor, as.character) |>
#     dplyr::mutate(mapunit1 = ifelse(.data$mapunit2 == .data$.pred_class, as.character(.data$mapunit2), as.character(.data$mapunit1))) |>
#     dplyr::mutate_if(is.character, factor)
#
#   aspatial_mapunit <- data_pa |>
#     dplyr::select(.data$mapunit1) |> # |> group_by(mapunit) |>
#     dplyr::add_count(.data$mapunit1, name = "trans.tot") |>
#     dplyr::distinct()
#   #
#   aspatial_pred <- data_pa |>
#     dplyr::select(.data$.pred_class) |>
#     dplyr::add_count(.data$.pred_class, name = "pred.tot") |>
#     dplyr::distinct()
#
#   aspatial_acc_pa <- dplyr::full_join(aspatial_mapunit, aspatial_pred, by = c("mapunit1" = ".pred_class")) |>
#     dplyr::select(.data$mapunit1, .data$trans.tot, .data$pred.tot) |>
#     dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ tidyr::replace_na(., 0))) |>
#     dplyr::mutate(trans.sum = sum(.data$trans.tot)) |>
#     dplyr::mutate(no.classes = length(unique(.data$mapunit1))) |>
#     dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ tidyr::replace_na(., 0))) |>
#     dplyr::rowwise() |>
#     dplyr::mutate(
#       aspat_pa = min((.data$trans.tot / .data$trans.tot), (.data$pred.tot / .data$trans.tot)),
#       aspat_pa_wtd = min((.data$trans.tot / .data$trans.sum), (.data$pred.tot / .data$trans.sum))
#     ) |>
#     dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ replace(., is.nan(.), 0))) |>
#     dplyr::ungroup() |>
#     dplyr::mutate(
#       aspat_pa_theta0 = sum(.data$aspat_pa_wtd),
#       aspat_pa_theta1 = mean(.data$aspat_pa)
#     ) |>
#     dplyr::rowwise() |>
#     dplyr::mutate(aspat_pa_theta_wt = theta * (1 / .data$no.classes) + (1 - theta) * (.data$trans.tot / .data$trans.sum)) |> #
#     dplyr::mutate(aspat_pa_theta_work = .data$aspat_pa_theta_wt * .data$aspat_pa_wtd) |>
#     dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ replace(., is.nan(.), 0))) |>
#     dplyr::ungroup() |>
#     dplyr::mutate(aspat_pa_theta.5 = sum(.data$aspat_pa_theta_wt * .data$aspat_pa)) |>
#     dplyr::select(-.data$aspat_pa_theta_wt, -.data$aspat_pa_theta_work, -.data$no.classes, -.data$trans.sum, -.data$trans.tot) |>
#     dplyr::ungroup() |>
#     dplyr::distinct() |>
#     dplyr::select(-.data$pred.tot)
#
#   aspatial_acc2 <- dplyr::left_join(aspatial_acc, aspatial_acc_pa, by = "mapunit1")
#
#   accuracy_stats <- dplyr::left_join(spatial_acc, aspatial_acc2, by = "mapunit1")
#
#   ### calculate paf aspatial statistics
#   aspat_fpa_df <- accuracy_stats |>
#     dplyr::select(
#       .data$mapunit1, .data$trans.sum, .data$no.classes, .data$trans.tot,
#       .data$pred.tot, .data$spat_p_correct, .data$spat_paf_correct
#     ) |>
#     dplyr::rowwise() |>
#     dplyr::mutate(
#       aspat_paf_min_correct = min(.data$trans.tot, .data$pred.tot),
#       aspat_paf_extra = .data$spat_paf_correct - .data$spat_p_correct,
#       aspat_paf_total = .data$aspat_paf_min_correct + .data$aspat_paf_extra,
#       aspat_paf_pred = min((.data$aspat_paf_total / .data$trans.tot), (.data$trans.tot / .data$trans.tot)),
#       aspat_paf_pred2 = min((.data$aspat_paf_total / .data$trans.sum), (.data$trans.tot / .data$trans.sum)),
#       aspat_paf_unit_pos = min(.data$trans.tot, .data$aspat_paf_total)
#     ) |>
#     dplyr::ungroup() |>
#     dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ tidyr::replace_na(., 0))) |>
#     dplyr::mutate(
#       aspat_paf_theta0 = sum(.data$aspat_paf_unit_pos / .data$trans.sum),
#       aspat_paf_theta1 = mean(.data$aspat_paf_pred)
#     ) |>
#     dplyr::rowwise() |>
#     dplyr::mutate(aspat_paf_theta_wt = theta * (1 / .data$no.classes) + (1 - theta) * (.data$trans.tot / .data$trans.sum)) |> #
#     dplyr::mutate(aspat_paf_theta_work = .data$aspat_paf_theta_wt * .data$aspat_paf_pred2) |>
#     dplyr::ungroup() |>
#     dplyr::mutate(aspat_paf_theta.5 = sum(.data$aspat_paf_theta_wt * .data$aspat_paf_pred)) |>
#     dplyr::select(-.data$aspat_paf_theta_wt, -.data$aspat_paf_theta_work) |>
#     dplyr::ungroup() |>
#     dplyr::distinct() |>
#     dplyr::select(.data$mapunit1, .data$aspat_paf_theta0, .data$aspat_paf_theta.5, .data$aspat_paf_theta1)
#
#   accuracy_stats <- dplyr::left_join(accuracy_stats, aspat_fpa_df, by = "mapunit1") |>
#     # accuracy_stats <- accuracy_stats |>
#     dplyr::select(
#       .data$mapunit1, .data$trans.sum, .data$trans.tot, .data$pred.tot, .data$no.classes,
#       .data$acc, .data$kap, .data$spat_p_correct, .data$spat_pa_correct, .data$spat_pf_correct,
#       .data$spat_paf_correct, .data$spat_p, .data$spat_pa, .data$spat_pf, .data$spat_paf,
#       .data$spat_p_theta0, .data$spat_p_theta.5, .data$spat_p_theta1,
#       .data$spat_pa_theta0, .data$spat_pa_theta.5, .data$spat_pa_theta1,
#       .data$spat_paf_theta0, .data$spat_paf_theta.5, .data$spat_paf_theta1,
#       .data$aspat_p, .data$aspat_pa, .data$aspat_p_wtd, .data$aspat_pa_wtd,
#       .data$aspat_p_theta0, .data$aspat_p_theta.5, .data$aspat_p_theta1,
#       .data$aspat_pa_theta0, .data$aspat_pa_theta.5, .data$aspat_pa_theta1,
#       .data$aspat_paf_theta0, .data$aspat_paf_theta.5, .data$aspat_paf_theta1
#     )
#
#   return(accuracy_stats)
# }
