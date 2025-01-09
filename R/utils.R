# series of functions to assist with procesing and modelling PEM data

select_pure_training <- function(tps) {
  pure_tran_dat <- tps |>
    dplyr::filter(!"mapunit1" == "") |>
    dplyr::filter(!is.na("mapunit1")) |>
    dplyr::filter(is.na(.data$mapunit2))

  return(pure_tran_dat)
}


.filter_min_mapunits <- function(tpts, min_no) {
  if ("position" %in% names(tpts)) {
    tpts <- tpts |> dplyr::filter(.data$position == "Orig")
  } else {
    cli::cli_alert_warning("No position column found, assuming all points are original")
  }

  MU_count <- tpts |> dplyr::count(.data$mapunit1)

  todrop <- MU_count |> dplyr::filter(.data$n < min_no)
  cli::cli_alert_warning("Dropping the following mapunits from the training points: {todrop$mapunit1}")

  tokeep <- MU_count |> dplyr::filter(.data$n >= min_no)

  mdat <- tpts |>
    dplyr::filter(.data$mapunit1 %in% tokeep$mapunit1)

  return(mdat)
}


.harmonize_factors <- function(target_vs_pred) {
  targ.lev <- levels(as.factor(target_vs_pred$mapunit1))
  pred.lev <- levels(as.factor(target_vs_pred$.pred_class))
  levs <- c(targ.lev, pred.lev) |> unique()
  target_vs_pred$mapunit1 <- factor(target_vs_pred$mapunit1, levels = levs)
  target_vs_pred$.pred_class <- factor(target_vs_pred$.pred_class, levels = levs)
  return(target_vs_pred)
}
