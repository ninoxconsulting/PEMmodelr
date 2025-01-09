#' Calculate model accuracy metrics
#'
#' Calculates internal ML metrics and PEM specific spatial and proportional metrics.
#' This currently includes: accuracy, mcc, spatial metrics for primary, primary + alternate,
#' and primary + alternate + fuzzy for theta adjusts set at 0, 0.5, 1. Also aspatial proportions
#' for primary+alternate+fuzzy with the same 3 theta settings
#'
#' @param pred_data a data.frame with mapunit1, mapunit2, and .pred columns
#' @param fuzzmatrx is the fuzzy values matrix from the map key giving partial correct points
#' for near misses
#' @param theta the function always returns values for theta 0 and theta 1.
#' The theta setting sets an intermediate theta setting to report efault set to 0.5
#' @export
#' @examples
#' \dontrun{
#' acc_metrics(pred_data, fuzz, theta = 0.5)
#' }
acc_metrics <- function(pred_data, fuzzmatrx, theta = 0.5) {
  # # testing lines
  #   pred_data = pred_all
  #     fuzzmatrx = fmat
  #     theta = 0.5
  # # # end testing line

  preds <- c("id", "mapunit1", "mapunit2", ".pred_class")
  pred_data <- pred_data |>
    dplyr::select(dplyr::any_of(preds)) |>
    dplyr::mutate_if(is.factor, as.character)
  pred_data <- replace(pred_data, is.na(pred_data), 0)

  # add the fuzzy value for mapunit 1 and predicted
  data1 <- dplyr::left_join(pred_data, fuzzmatrx, by = c("mapunit1" = "target", ".pred_class" = "Pred")) |>
    dplyr::mutate_if(is.character, as.factor) |>
    dplyr::mutate(fVal = ifelse(is.na(.data$fVal), 0, .data$fVal)) |>
    dplyr::rename("p_fuzzval" = .data$fVal)

  # add the fuzzy value for mapunit 2 and predicted
  data2 <- dplyr::left_join(data1, fuzzmatrx, by = c("mapunit2" = "target", ".pred_class" = "Pred")) |>
    dplyr::mutate_if(is.character, as.factor) |>
    dplyr::mutate(fVal = ifelse(is.na(.data$fVal), 0, .data$fVal)) |> # replace(is.na(data1$fVal), 0) |>
    dplyr::rename("alt_fuzzval" = .data$fVal)


  ## 2. selects the neighbour with max value (based on primary and seconday),,
  ## then group by the highest neighbour option.
  pdata <- data2 |>
    dplyr::rowwise() |>
    dplyr::mutate(pa_fuzzval = max(.data$p_fuzzval, .data$alt_fuzzval)) |>
    dplyr::group_by(.data$id) |>
    dplyr::top_n(1, abs(.data$pa_fuzzval)) |>
    dplyr::distinct(.data$id, .keep_all = TRUE) |>
    data.frame()

  pdata <- pdata |>
    dplyr::mutate_if(is.factor, as.character) |>
    dplyr::mutate(
      p_Val = ifelse(.data$mapunit1 == .data$.pred_class, 1, 0),
      alt_Val = ifelse(.data$mapunit2 == .data$.pred_class, 1, 0)
    ) |>
    dplyr::mutate(
      alt_Val = ifelse(is.na(.data$alt_Val), 0, .data$alt_Val)
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(pa_Val = max(.data$p_Val, .data$alt_Val)) |>
    dplyr::select(-.data$alt_Val) |>
    dplyr::mutate_if(is.character, as.factor) |>
    data.frame() |>
    dplyr::add_count(.data$mapunit1, name = "trans.tot") |>
    dplyr::add_count(.data$.pred_class, name = "pred.tot")

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

  if (length(add.pred.lev > 0)) {
    pdata <- pdata |>
      dplyr::mutate(
        mapunit.new = ifelse(pdata$.pred_class %in% add.pred.lev, as.character(.data$.pred_class), as.character(.data$mapunit1)),
        trans.tot.new = ifelse(.data$.pred_class %in% add.pred.lev, 0, .data$trans.tot)
      ) |>
      dplyr::mutate_if(is.character, as.factor) |>
      dplyr::mutate(trans.tot = .data$trans.tot.new) |>
      dplyr::select(-.data$trans.tot.new)
    #   mutate(pred.new = ifelse(mapunit.new %in% add.pred.lev, as.character(mapunit), as.character(.pred_class))) |>
    #     mutate(mapunit = mapunit.new, .pred_class = pred.new)
  } else {
    pdata <- pdata |>
      dplyr::mutate(mapunit.new = as.character(.data$mapunit1)) |>
      dplyr::mutate_if(is.character, as.factor)
  }

  # ### harmonize factor levels
  targ.lev <- levels(pdata$mapunit1)
  pred.lev <- levels(pdata$.pred_class)
  levs <- c(targ.lev, pred.lev) |> unique()
  # generate a new mpaunit with all levs
  pdata$mapunit1 <- factor(pdata$mapunit1, levels = levs)
  pdata$.pred_class <- factor(pdata$.pred_class, levels = levs)

  # pdata <- .harmonize_factors(pdata)

  pdata <- pdata |>
    tidyr::drop_na(.data$mapunit1) |>
    dplyr::mutate(no.classes = length(levs)) |>
    dplyr::select(-.data$pred.tot)

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
    as.numeric() |>
    round(3)

  mcc <- pdata |>
    yardstick::mcc(.data$mapunit1, .data$.pred_class, na_rm = TRUE) |>
    dplyr::select(.data$.estimate) |>
    as.numeric() |>
    round(3)

  # sens <- data |> sens(mapunit, .pred_class, na_rm = TRUE)
  # spec <- data |> yardstick::spec(mapunit, .pred_class, na_rm = TRUE)
  # prec <- data |> precision(mapunit, .pred_class, na.rm = TRUE)
  # recall <- data |> recall(mapunit, .pred_class, na.rm = TRUE)
  # fmeas <- data |> f_meas(mapunit, .pred_class, na.rm = TRUE)
  kap <- pdata |>
    yardstick::kap(.data$mapunit1, .data$.pred_class, na.rm = TRUE) |>
    dplyr::select(.data$.estimate) |>
    as.numeric() |>
    round(3)

  ### 2) spatial stats
  spatial_acc <- pdata |>
    dplyr::mutate(trans.sum = dplyr::n(), acc = acc, kap = kap) |>
    ### here the problem is differing number of mapunit vs .pred_class
    dplyr::group_by(.data$mapunit.new) |>
    dplyr::mutate(spat_p_correct = sum(.data$p_Val)) |>
    dplyr::mutate(spat_pa_correct = sum(.data$pa_Val)) |>
    dplyr::mutate(spat_pf_correct = sum(.data$p_fuzzval)) |>
    dplyr::mutate(spat_paf_correct = sum(.data$pa_fuzzval)) |>
    dplyr::ungroup() |>
    dplyr::select(
      -.data$id, -.data$mapunit1, -.data$mapunit2, -.data$.pred_class, -.data$p_fuzzval,
      -.data$pa_fuzzval, -.data$p_Val, -.data$pa_Val, -.data$alt_fuzzval
    ) |>
    dplyr::distinct()

  spatial_acc <- spatial_acc |>
    dplyr::mutate(spat_p = .data$spat_p_correct / .data$trans.tot) |>
    dplyr::mutate(spat_pa = .data$spat_pa_correct / .data$trans.tot) |>
    dplyr::mutate(spat_pf = .data$spat_pf_correct / .data$trans.tot) |>
    dplyr::mutate(spat_paf = .data$spat_paf_correct / .data$trans.tot) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ replace(., is.nan(.), 0))) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ replace(., is.infinite(.), 0)))

  spatial_acc <- spatial_acc |>
    dplyr::mutate(
      spat_p_theta1 = mean(.data$spat_p),
      spat_p_theta0 = sum(.data$spat_p_correct) / .data$trans.sum,
      spat_pa_theta1 = mean(.data$spat_pa),
      spat_pa_theta0 = sum(.data$spat_pa_correct) / .data$trans.sum,
      spat_paf_theta1 = mean(.data$spat_paf),
      spat_paf_theta0 = sum(.data$spat_paf_correct) / .data$trans.sum
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      spat_p_theta_wt = theta * (1 / .data$no.classes) + (1 - theta) * (.data$trans.tot / .data$trans.sum),
      spat_pa_theta_wt = theta * (1 / .data$no.classes) + (1 - theta) * (.data$trans.tot / .data$trans.sum),
      spat_paf_theta_wt = theta * (1 / .data$no.classes) + (1 - theta) * (.data$trans.tot / .data$trans.sum),
      spat_p_theta_work = .data$spat_p_theta_wt * .data$spat_p,
      spat_pa_theta_work = .data$spat_pa_theta_wt * .data$spat_pa,
      spat_paf_theta_work = .data$spat_paf_theta_wt * .data$spat_paf
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      spat_p_theta.5 = sum(.data$spat_p_theta_work),
      spat_pa_theta.5 = sum(.data$spat_pa_theta_work),
      spat_paf_theta.5 = sum(.data$spat_paf_theta_work)
    ) |>
    dplyr::select(
      -.data$spat_p_theta_wt, -.data$spat_p_theta_work, -.data$spat_pa_theta_wt,
      -.data$spat_pa_theta_work, -.data$spat_paf_theta_wt, -.data$spat_paf_theta_work
    ) |>
    dplyr::distinct() |>
    dplyr::rename(mapunit1 = .data$mapunit.new)

  # 3) calculate aspatial metrics (overall and mapunit % correct)
  aspatial_mapunit <- pdata |>
    dplyr::select(.data$mapunit1) |>
    dplyr::add_count(.data$mapunit1, name = "trans.tot") |>
    dplyr::distinct() |>
    dplyr::mutate(trans.sum = sum(.data$trans.tot))
  #
  aspatial_pred <- pdata |>
    dplyr::select(.data$.pred_class) |>
    dplyr::add_count(.data$.pred_class, name = "pred.tot") |>
    dplyr::distinct()

  aspatial_acc <- dplyr::full_join(aspatial_mapunit, aspatial_pred, by = c("mapunit1" = ".pred_class")) |>
    dplyr::select(.data$mapunit1, .data$trans.tot, .data$pred.tot) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ tidyr::replace_na(., 0))) |>
    dplyr::mutate(trans.sum = sum(.data$trans.tot)) |>
    dplyr::mutate(no.classes = length(unique(.data$mapunit1))) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ tidyr::replace_na(., 0))) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      aspat_p = min((.data$trans.tot / .data$trans.tot), (.data$pred.tot / .data$trans.tot)),
      aspat_p_wtd = min((.data$trans.tot / .data$trans.sum), (.data$pred.tot / .data$trans.sum))
    ) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ replace(., is.nan(.), 0))) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      aspat_p_theta0 = sum(.data$aspat_p_wtd),
      aspat_p_theta1 = mean(.data$aspat_p)
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(aspat_p_theta_wt = theta * (1 / .data$no.classes) + (1 - theta) * (.data$trans.tot / .data$trans.sum)) |> #
    dplyr::mutate(aspat_p_theta_work = .data$aspat_p_theta_wt * .data$aspat_p_wtd) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ replace(., is.nan(.), 0))) |>
    dplyr::ungroup() |>
    dplyr::mutate(aspat_p_theta.5 = sum(.data$aspat_p_theta_wt * .data$aspat_p)) |>
    dplyr::select(-.data$aspat_p_theta_wt, -.data$aspat_p_theta_work, -.data$no.classes, -.data$trans.sum, -.data$trans.tot) |>
    dplyr::ungroup() |>
    dplyr::distinct()

  #### --- primary plus alternate

  data_pa <- pdata |>
    dplyr::mutate_if(is.factor, as.character) |>
    dplyr::mutate(mapunit1 = ifelse(.data$mapunit2 == .data$.pred_class, as.character(.data$mapunit2), as.character(.data$mapunit1))) |>
    dplyr::mutate_if(is.character, factor)

  aspatial_mapunit <- data_pa |>
    dplyr::select(.data$mapunit1) |> # |> group_by(mapunit) |>
    dplyr::add_count(.data$mapunit1, name = "trans.tot") |>
    dplyr::distinct()
  #
  aspatial_pred <- data_pa |>
    dplyr::select(.data$.pred_class) |>
    dplyr::add_count(.data$.pred_class, name = "pred.tot") |>
    dplyr::distinct()

  aspatial_acc_pa <- dplyr::full_join(aspatial_mapunit, aspatial_pred, by = c("mapunit1" = ".pred_class")) |>
    dplyr::select(.data$mapunit1, .data$trans.tot, .data$pred.tot) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ tidyr::replace_na(., 0))) |>
    dplyr::mutate(trans.sum = sum(.data$trans.tot)) |>
    dplyr::mutate(no.classes = length(unique(.data$mapunit1))) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ tidyr::replace_na(., 0))) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      aspat_pa = min((.data$trans.tot / .data$trans.tot), (.data$pred.tot / .data$trans.tot)),
      aspat_pa_wtd = min((.data$trans.tot / .data$trans.sum), (.data$pred.tot / .data$trans.sum))
    ) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ replace(., is.nan(.), 0))) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      aspat_pa_theta0 = sum(.data$aspat_pa_wtd),
      aspat_pa_theta1 = mean(.data$aspat_pa)
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(aspat_pa_theta_wt = theta * (1 / .data$no.classes) + (1 - theta) * (.data$trans.tot / .data$trans.sum)) |> #
    dplyr::mutate(aspat_pa_theta_work = .data$aspat_pa_theta_wt * .data$aspat_pa_wtd) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ replace(., is.nan(.), 0))) |>
    dplyr::ungroup() |>
    dplyr::mutate(aspat_pa_theta.5 = sum(.data$aspat_pa_theta_wt * .data$aspat_pa)) |>
    dplyr::select(-.data$aspat_pa_theta_wt, -.data$aspat_pa_theta_work, -.data$no.classes, -.data$trans.sum, -.data$trans.tot) |>
    dplyr::ungroup() |>
    dplyr::distinct() |>
    dplyr::select(-.data$pred.tot)

  aspatial_acc2 <- dplyr::left_join(aspatial_acc, aspatial_acc_pa, by = "mapunit1")

  accuracy_stats <- dplyr::left_join(spatial_acc, aspatial_acc2, by = "mapunit1")

  ### calculate paf aspatial statistics
  aspat_fpa_df <- accuracy_stats |>
    dplyr::select(
      .data$mapunit1, .data$trans.sum, .data$no.classes, .data$trans.tot,
      .data$pred.tot, .data$spat_p_correct, .data$spat_paf_correct
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      aspat_paf_min_correct = min(.data$trans.tot, .data$pred.tot),
      aspat_paf_extra = .data$spat_paf_correct - .data$spat_p_correct,
      aspat_paf_total = .data$aspat_paf_min_correct + .data$aspat_paf_extra,
      aspat_paf_pred = min((.data$aspat_paf_total / .data$trans.tot), (.data$trans.tot / .data$trans.tot)),
      aspat_paf_pred2 = min((.data$aspat_paf_total / .data$trans.sum), (.data$trans.tot / .data$trans.sum)),
      aspat_paf_unit_pos = min(.data$trans.tot, .data$aspat_paf_total)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ tidyr::replace_na(., 0))) |>
    dplyr::mutate(
      aspat_paf_theta0 = sum(.data$aspat_paf_unit_pos / .data$trans.sum),
      aspat_paf_theta1 = mean(.data$aspat_paf_pred)
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(aspat_paf_theta_wt = theta * (1 / .data$no.classes) + (1 - theta) * (.data$trans.tot / .data$trans.sum)) |> #
    dplyr::mutate(aspat_paf_theta_work = .data$aspat_paf_theta_wt * .data$aspat_paf_pred2) |>
    dplyr::ungroup() |>
    dplyr::mutate(aspat_paf_theta.5 = sum(.data$aspat_paf_theta_wt * .data$aspat_paf_pred)) |>
    dplyr::select(-.data$aspat_paf_theta_wt, -.data$aspat_paf_theta_work) |>
    dplyr::ungroup() |>
    dplyr::distinct() |>
    dplyr::select(.data$mapunit1, .data$aspat_paf_theta0, .data$aspat_paf_theta.5, .data$aspat_paf_theta1)

  accuracy_stats <- dplyr::left_join(accuracy_stats, aspat_fpa_df, by = "mapunit1") |>
    # accuracy_stats <- accuracy_stats |>
    dplyr::select(
      .data$mapunit1, .data$trans.sum, .data$trans.tot, .data$pred.tot, .data$no.classes,
      .data$acc, .data$kap, .data$spat_p_correct, .data$spat_pa_correct, .data$spat_pf_correct,
      .data$spat_paf_correct, .data$spat_p, .data$spat_pa, .data$spat_pf, .data$spat_paf,
      .data$spat_p_theta0, .data$spat_p_theta.5, .data$spat_p_theta1,
      .data$spat_pa_theta0, .data$spat_pa_theta.5, .data$spat_pa_theta1,
      .data$spat_paf_theta0, .data$spat_paf_theta.5, .data$spat_paf_theta1,
      .data$aspat_p, .data$aspat_pa, .data$aspat_p_wtd, .data$aspat_pa_wtd,
      .data$aspat_p_theta0, .data$aspat_p_theta.5, .data$aspat_p_theta1,
      .data$aspat_pa_theta0, .data$aspat_pa_theta.5, .data$aspat_pa_theta1,
      .data$aspat_paf_theta0, .data$aspat_paf_theta.5, .data$aspat_paf_theta1
    )

  return(accuracy_stats)
}
