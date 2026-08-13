#' Calculate accuracy metrics
#'
#' Calculates internal ML metrics and PEM specific spatial and proportional metrics.
#' This currently includes: accuracy, mcc, spatial metrics for primary, primary + alternate,
#' and primary + alternate + fuzzy for theta adjusts set at 0, 0.5, 1. Also aspatial proportions
#' for primary+alternate+fuzzy with the same 3 theta settings. This is the updated version of acc_metrics function
#'
#'
#' @param pred_data a data.frame with mapunit1, mapunit2, and .pred columns
#' @param fuzz_matrix is the fuzzy values matrix from the map key giving partial correct points
#' for near misses
#' @param theta the function always returns values for theta 0 and theta 1.
#' The theta setting sets an intermediate theta setting to report efault set to 0.5
#'
#' @returns tibble with all calc values
#' @export
#' @examples
#' \dontrun{
#' calc_acc(pred_data, fuzz_matrix, theta = 0.5)
#' }
#
calc_acc <- function(pred_data, fuzz_matrix = fuzz_matrix, theta = theta) {

  # testing line
  #pred_data = pred_all

  # get a list of thetas to use
  if (is.null(theta)) {
    theta_seq <- seq(0, 1, by = 0.5)
  } else {
    theta_seq <- c(0, theta, 1)
  }

  # part 1: standard summary of transect

  trans.sum <- length(pred_data$mapunit1)
  trans.tot <- data.frame("trans.tot" = rowSums(with(pred_data, table(mapunit1, .pred_class))))
  pred.tot <- data.frame("pred.tot" = colSums(with(pred_data, table(mapunit1, .pred_class))))
  #Confusion matrices and agreement tallies
  prim.agree.tally <- diag(with(pred_data, table(mapunit1, .pred_class)))
  no.classes <- length(levels(pred_data$mapunit1))
  a <- rowSums(with(pred_data, table(mapunit1, .pred_class)))
  b <- colSums(with(pred_data, table(mapunit1, .pred_class)))
  p.e <- sum(a * b) / nrow(pred_data)^2
  p.o <- sum(diag(with(pred_data, table(mapunit1, .pred_class)))) / nrow(pred_data)
  acc <- p.o
  kap <- (p.o - p.e) / (1 - p.e)

  ## checked kappa using tidyverse
  #kap_tv <- pred_data |>
  #  yardstick::kap(.data$mapunit1, .data$.pred_class, na.rm = TRUE) |>
  #  dplyr::select(.data$.estimate) |>
  #  as.numeric()

  # format and output the spatial columns
  out <- cbind(trans.tot, pred.tot)
  out <- out |>
    dplyr::mutate(
      trans.sum = trans.sum,
      no.classes = no.classes,
      acc = round(acc, 3),
      kap = round(kap, 3)
    ) |>
    tibble::rownames_to_column("mapunit1")


  # part 2: spatial correct (p and pa)

  spat_pa_cor <- purrr::map(levels(pred_data$mapunit1), function(i) {
    # i <- levels(pred_data$mapunit1)[1]
    match1 <- pred_data[pred_data$mapunit1 == i & pred_data$.pred_class == i, ]
    pred_data.2 <- pred_data[!is.na(pred_data$mapunit2), ]
    match2 <- pred_data.2[pred_data.2$mapunit1 == i & pred_data.2$.pred_class != i, ]
    # secondary matches among non-matches
    extras <- sum(as.character(match2$mapunit2) == as.character(match2$.pred_class))
    tibble::tibble(mapunit1 = i, spat_p_correct = nrow(match1), spat_pa_correct = nrow(match1) + extras)
  }) |> dplyr::bind_rows()

  out <- dplyr::left_join(out, spat_pa_cor, by = "mapunit1")

  #########################################
  # part 3: aspatial overlap. primary only

  a.1 <- with(pred_data, table(mapunit1))
  a.2 <- with(pred_data, table(.pred_class))

  asapt_p_correct <- as.data.frame(pmin(a.1, a.2)) |> dplyr::rename("aspat_p_correct" = .data$Freq)
  p.overlap <- apply(cbind(a.1, a.2), 1, min) / a.1

  ## NOTE - perhaps this can be a 0 not NA... leavign as NA for now
  asapt_p <- as.data.frame(p.overlap) |> dplyr::rename("aspat_p" = .data$Freq)

  # TODO : decided if you want to conver these to 0 or leave as NA.
  asapt_p <- asapt_p |>
    dplyr::mutate(aspat_p = ifelse(is.nan(aspat_p), 0, aspat_p))

  asat_out <- dplyr::left_join(asapt_p_correct, asapt_p, by = "mapunit1")

  #----------------------------------;
  # aspatial overlap with alternates
  # optimizing the choice of which to use

  # breakdown for mapunit1 when no secondary choice available
  m.1 <- with(pred_data[is.na(pred_data$mapunit2), ], table(mapunit1))
  # breakdown for predictions when no secondary choice available
  p.1 <- with(pred_data[is.na(pred_data$mapunit2), ], table(.pred_class))

  # df when secondary choice is available
  choice <- pred_data[!is.na(pred_data$mapunit2), ]
  # breakdown for predictions when secondary choice is available
  p.f <- with(choice, table(.pred_class))

  # function to be maximized, x is binary var deciding whether to choose primary or secondary call
  opt.fnc <- function(x) {
    m.f.c <- ifelse(x == 0, levels(choice$mapunit1)[choice$mapunit1], levels(choice$mapunit2)[choice$mapunit2])
    m.f <- table(factor(m.f.c, levels = levels(choice$mapunit1)))
    maps <- m.1 + m.f
    preds <- p.1 + p.f
    #-mean((maps - as.vector(preds))**2)
    sum(pmin(maps, as.vector(preds))) / sum(maps)
  }

  # call the optimizer
  testy <- GA::ga(type = "binary", fitness = opt.fnc, nBits = nrow(choice), maxiter = 500, seed = 5)
  # summary(testy)
  # testy@fitnessValue

  choice$map.best <- ifelse(t(testy@solution)[, 1] == 0, levels(choice$mapunit1)[choice$mapunit1], levels(choice$mapunit2)[choice$mapunit2])
  map.opt <- with(choice, table(factor(map.best, levels = levels(choice$mapunit1)))) + m.1
  # aspat_pa_correct
  # map.opt

  asapt_pa_correct <- as.data.frame(map.opt) |> dplyr::rename("aspat_pa_correct" = .data$Freq)

  # Overall overlap
  aspat_pa <- as.data.frame(apply(cbind(map.opt, p.1 + p.f), 1, min) / (map.opt)) |>
    dplyr::rename("aspat_pa" = .data$Freq)

  # TODO : decided if you want to conver these to 0 or leave as NA.
  aspat_pa <- aspat_pa |>
    dplyr::mutate(aspat_pa = ifelse(is.nan(aspat_pa), 0, aspat_pa))

  asapt_pa_correct <- dplyr::left_join(asapt_pa_correct, aspat_pa) |>
    dplyr::rename("mapunit1" = .data$Var1)

  asapt_out <- dplyr::left_join(asat_out, asapt_pa_correct, by = "mapunit1")

  #----------------------------------;
  # aspatial overlap with fuzz

  # Make a matrix out of fuzz_matrix df
  fuzz.matrix <- as.matrix(stats::xtabs(fVal ~ as.factor(target) + as.factor(Pred), data = fuzz_matrix, sparse = TRUE))

  # test funcion: find fuzzy score from first call (include matches). not used below.
  pred_data$fuzz.1 <- apply(pred_data[, c(2, 4)], 1, FUN = function(x) fuzz.matrix[x[1], x[2]])

  # find fuzzy score from first call (non-matches only)
  non.matches <- pred_data[pred_data$mapunit1 != pred_data$.pred_class, ]
  non.matches$fuzz.1 <- apply(non.matches[, c(2, 4)], 1, FUN = function(x) fuzz.matrix[x[1], x[2]])
  fuzz.1 <- aggregate(fuzz.1 ~ mapunit1, data = non.matches, sum)
  # fuzz.1

  # combine together the primary and secondary fuzzy scores
  wtf <- data.frame(mapunit1 = names(prim.agree.tally), matchy = prim.agree.tally)
  z.0 <- merge(wtf, fuzz.1, by = "mapunit1", all = TRUE)

  # find fuzzy score from second call (non-matches only)
  non.matches.2 <- non.matches[!is.na(non.matches$mapunit2), ]

  if(nrow(non.matches.2) > 0) { ### IF THERE ARE NO NON_MATCHES
    non.matches.2$fuzz.2 <- apply(non.matches.2[, c(3, 4)], 1, FUN = function(x) fuzz.matrix[x[1], x[2]])
    fuzz.2 <- stats::aggregate(fuzz.2 ~ mapunit1, data = non.matches.2, sum)
    # fuzz.2
    z <- merge(z.0, fuzz.2, by = "mapunit1", all = TRUE)
  } else {
    z <- z.0
  }

  # spat_paf matches
  spat_paf_correct <- data.frame(cbind(z, "spat_paf_correct" = rowSums(z[, -1], na.rm = TRUE))) |>
    dplyr::select(mapunit1, spat_paf_correct)

  # Merge all the data components together

  out <- dplyr::left_join(out, spat_paf_correct, by = "mapunit1")

  out <- out |>
    dplyr::rowwise() |>
    dplyr::mutate(
      spat_p = .data$spat_p_correct / .data$trans.tot,
      spat_pa = .data$spat_pa_correct / .data$trans.tot,
      spat_paf = .data$spat_paf_correct / .data$trans.tot
    )
  # TODO : decided if you want to conver these to 0 or leave as NA.
  out <- out |>
    dplyr::mutate(spat_p = ifelse(is.nan(.data$spat_p), 0, .data$spat_p),
           spat_pa = ifelse(is.nan(.data$spat_pa), 0, .data$spat_pa),
           spat_paf = ifelse(is.nan(.data$spat_paf), 0, .data$spat_paf))

  out <- out |>
    dplyr::ungroup() |>
    dplyr::mutate(
      spat_p_theta_0 = round(sum(.data$spat_p_correct) / .data$trans.sum, 3),
      spat_pa_theta_0 = round(sum(.data$spat_pa_correct) / .data$trans.sum, 3),
      spat_paf_theta_0 = round(sum(.data$spat_paf_correct) / .data$trans.sum, 3)
    )

  # Close Inspection on ICHmc1_01 as test case to see if above is solid
  # fuzz_matrix[fuzz_matrix$target=="ICHmc1_01" & fuzz_matrix$fVal > 0,]
  # with(pred_data[pred_data$mapunit1=="ICHmc1_01",], table(mapunit1, .pred_class))
  # with(pred_data[pred_data$mapunit1=="ICHmc1_01" & pred_data$.pred_class !="ICHmc1_01",], table(mapunit2, .pred_class))
  # fuzz_matrix[fuzz_matrix$target=="ICHmc1_03" & fuzz_matrix$fVal > 0,]
  # fuzz_matrix[fuzz_matrix$target=="ICHmc1_04" & fuzz_matrix$fVal > 0,]
  # non.matches.2[non.matches.2$mapunit1=="ICHmc1_01",]

  #--------------------------------------------;
  # Below we find the optimal amount of fuzz wrt aspatial accuracy
  # i.e.optimizing the amount of fuzz to use

  subset.rows <- which(rownames(fuzz.matrix) %in% levels(pred_data$mapunit1))
  fuzz.subset <- fuzz.matrix[subset.rows, ]
  # drop columns of all zeroes
  fuzz.subset <- fuzz.subset[, colSums(fuzz.subset == 0) != nrow(fuzz.subset)]

  c.names <- intersect(rownames(fuzz.subset), colnames(fuzz.subset))
  # Get integer indices for these names
  # row.inds <- match(c.names, rownames(fuzz.subset))
  col.inds <- match(c.names, colnames(fuzz.subset))

  # boiling down to core SS
  fuzz.subset.core <- fuzz.subset[, col.inds]
  # Tempting to add a final column of "the rest" but it will not help improve predictions
  # any levels outside of .pred_class are superfluous
  # fuzz.subset.core <- cbind(fuzz.subset.core, rowSums(fuzz.subset)-rowSums(fuzz.subset.core))

  # function to count how many non-'diagonal' elements are nonzero and to be tracked
  fuzz.count <- function(x) {
    sum(x > 0) - 1
  }
  # test it. These are the parameters to be estimated
  # sum(apply(fuzz.subset.core, 1, fuzz.count))

  # Must ensure sum of off-diag elements in a row do not exceed unity. Needed to preserve n after matrix multiplication
  # Important: All of these values should be less than 1 - scale non-diagonal elements in each row if needed!!!!!
  #rowSums(fuzz.subset.core) - 1
  fuzz.matrix.adj <- fuzz.subset.core

  # Actual and predicted aspatial tallies for the site series
  act <- a.1
  pred <- a.2

  # Need to first determine which indices are zero (no fuzz allowed) or on the diagonal
  # note diagonals do not need to float: they ensure each row sums to unity and clean-up other row entries
  # could have picked any row element but this seems natural since diagonal elements are actually minimums, not upper limits
  # important note: for the elements of fuzz.matrix.adj, non-diagonals represent upper limit and diagonals represent lower limit

  # these are the indxs that do not float
  indxs <- which((fuzz.matrix.adj == 0) | row(fuzz.matrix.adj) == col(fuzz.matrix.adj))

  # initializing free parameters to 50% of fuzz limit
  Xmat.0 <- fuzz.matrix.adj[-indxs] / 2

  # initializing/creating a dummy matrix of all zeros
  # this is the shell to hold to optimized fuzz values
  Z.mat <- matrix(0, dim(fuzz.matrix.adj)[1], dim(fuzz.matrix.adj)[2])

  # bounds are between zero to upper fuzz limit
  low <- rep(0, length(Xmat.0))
  upp <- fuzz.matrix.adj[-indxs]

  # Function to be minimized
  # using (negative) overlap score but could also use mean squared loss
  loss.fnc <- function(x) {
    Z.mat[-indxs] <- x
    diag(Z.mat) <- 1 - rowSums(Z.mat, na.rm = TRUE)
    # updated predictions after applying some fuzz
    updated <- t(Z.mat) %*% pred
    # mean((act - as.vector(updated))**2)
    -sum(pmin(act, as.vector(updated))) / sum(act)
  }

  # call the optimizer
  testy <- stats::optim(par = Xmat.0, fn = loss.fnc, lower = low, upper = upp, method = "L-BFGS-B", control = list(maxit = 1000))
  # testy

  # Lets look closer at how we did
  Z.0 <- matrix(0, dim(fuzz.matrix.adj)[1], dim(fuzz.matrix.adj)[2])
  Z.0[-indxs] <- testy$par
  diag(Z.0) <- 1 - rowSums(Z.0, na.rm = TRUE)

  # Optimal fuzz matrix
  # round(Z.0, 3)

  # Original fuzz matrix
  # fuzz.matrix.adj

  # Adjusted fuzz optimized predictions vs actual and original predictions
  updated.0 <- t(Z.0) %*% pred
  # t(round(updated.0, 2))
  # act
  # pred

  # Adjusted loss
  # mean(abs(act - as.vector(updated.0))**2)

  # Original loss
  # mean((act - as.vector(pred))**2)

  ## Adjusted overlap
  #sum(pmin(act, as.vector(updated.0))) / sum(act)
  #apply(cbind(act, as.vector(updated.0)), 1, min) / act

  ## Original overlap
  #sum(pmin(act, as.vector(pred))) / sum(act)
  #apply(cbind(act, as.vector(pred)), 1, min) / act

  # crude but combining the two optimal solutions
  # Both fuzzy plus opt choice
  # Could also (possibly) optimize simultaneously

  aspat_paf_correct <- data.frame(pmin(map.opt, as.vector(updated.0))) |>
    dplyr::rename("mapunit1" = .data$Var1, "aspat_paf_correct" = .data$Freq)

  aspat_paf <- data.frame(apply(cbind(map.opt, as.vector(updated.0)), 1, min) / map.opt) |>
    dplyr::rename("mapunit1" = .data$Var1, "aspat_paf" = .data$Freq)


  # TODO : decided if you want to conver these to 0 or leave as NA.
  aspat_paf <- aspat_paf |>
    dplyr::mutate(aspat_paf = ifelse(is.nan(aspat_paf), 0, aspat_paf))

  asapt_out <- dplyr::left_join(asapt_out, aspat_paf_correct, join_by("mapunit1")) |>
    dplyr::left_join(aspat_paf, join_by("mapunit1"))


  # join the spatial and aspatial outputs together

  out <- dplyr::left_join(out, asapt_out, join_by("mapunit1"))

  #########################################################
  # calculate the theta values

  for (th in theta_seq) {
    #th <- theta_seq[1]
    out <- out |>
      dplyr::rowwise() |>
      dplyr::mutate(
        !!paste0("spat_p_theta_wt_", th) := th * (1 / .data$no.classes) + (1 - th) * (.data$trans.tot / .data$trans.sum),
        !!paste0("spat_p_theta_work_", th) := get(paste0("spat_p_theta_wt_", th)) * .data$spat_p,
        !!paste0("spat_pa_theta_work_", th) := get(paste0("spat_p_theta_wt_", th)) * .data$spat_pa,
        !!paste0("spat_paf_theta_work_", th) := get(paste0("spat_p_theta_wt_", th)) * .data$spat_paf,
        !!paste0("aspat_theta_wt_", th) := th * (1 / .data$no.classes) + (1 - th) * (.data$trans.tot / .data$trans.sum),
        !!paste0("aspat_p_theta_work_", th) := get(paste0("aspat_theta_wt_", th)) * .data$aspat_p,
        !!paste0("aspat_pa_theta_work_", th) := get(paste0("aspat_theta_wt_", th)) * .data$aspat_pa,
        !!paste0("aspat_paf_theta_work_", th) := get(paste0("aspat_theta_wt_", th)) * .data$aspat_paf
      ) |>
      dplyr::ungroup()
  }

  # Then summarize across all theta values
  for (th in theta_seq) {
    #th <- theta_seq[1]

    out <- out |>
      dplyr::mutate(
        !!paste0("spat_p_theta_", th) := (sum(get(paste0("spat_p_theta_work_", th)))),
        !!paste0("spat_pa_theta_", th) := (sum(get(paste0("spat_pa_theta_work_", th)))),
        !!paste0("spat_paf_theta_", th) := (sum(get(paste0("spat_paf_theta_work_", th)))),
        !!paste0("aspat_p_theta_", th) := sum(get(paste0("aspat_p_theta_work_", th))),
        !!paste0("aspat_pa_theta_", th) := sum(get(paste0("aspat_pa_theta_work_", th))),
        !!paste0("aspat_paf_theta_", th) := sum(get(paste0("aspat_paf_theta_work_", th)))
      )
  }

  out <- out |>
    dplyr::select(-dplyr::contains("wt")) |>
    dplyr::select(-dplyr::contains("work")) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("spat"), round, 3),
                  dplyr::across(dplyr::starts_with("aspat"), round, 3))

  return(out)
}

