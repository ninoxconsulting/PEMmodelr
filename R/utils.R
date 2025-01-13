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



# internal function for model run preparations

.create_training_set <- function(ref_dat, k) {
  ref_train <- ref_dat |>
    dplyr::filter(!.data$slice %in% k) |>
    dplyr::filter(is.na(.data$mapunit2)) |> # train only on pure calls
    dplyr::filter(.data$position == "Orig") |>
    dplyr::select(-.data$id, -.data$slice, -.data$mapunit2, -.data$position, -.data$transect_id) |>
    droplevels()

  return(ref_train)
}


.create_test_set <- function(ref_dat, k, use_neighbours, MU_count) {
  if (use_neighbours) {
    # test set
    ref_test <- ref_dat |>
      dplyr::filter(.data$slice %in% k) |>
      dplyr::filter(.data$mapunit1 %in% MU_count$mapunit1) |>
      droplevels()
  } else {
    ref_test <- ref_dat |>
      dplyr::filter(.data$slice %in% k) |>
      dplyr::filter(.data$mapunit1 %in% MU_count$mapunit1) |>
      dplyr::filter(.data$position == "Orig") |>
      droplevels()
  }

  return(ref_test)
}


# internal function to prep model output

.prep_model_output <- function(pred_all, nf_mapunits) {
  pred_all <- pred_all |> dplyr::mutate(
    mapunit1 = as.character(.data$mapunit1),
    mapunit2 = as.character(.data$mapunit2),
    .pred_class = as.character(.data$.pred_class)
  )

  # switch out the predicted Nf units for "nonfor" catergory.
  pred_all <- pred_all |>
    dplyr::mutate(
      mapunit1 = ifelse(.data$mapunit1 %in% nf_mapunits, "nonfor", .data$mapunit1),
      mapunit2 = ifelse(.data$mapunit2 %in% nf_mapunits, "nonfor", .data$mapunit2),
      .pred_class = ifelse(.data$.pred_class %in% nf_mapunits, "nonfor", .data$.pred_class)
    )

  # harmonize factor levels
  pred_all <- .harmonize_factors(pred_all)
  pred_all$mapunit2 <- as.factor(pred_all$mapunit2)

  return(pred_all)
}


# combine into a single file:

combine_balance_outputs <- function(out_bgc_dir) {
  bal_dir <- fs::path(out_bgc_dir, "balance")

  alldata_list <- list.files(file.path(bal_dir), full.names = TRUE, pattern = "acc_", recursive = TRUE)
  # remove files with no information
  data_list <- alldata_list[file.info(alldata_list)$size > 10]

  aresults <- purrr::map(data_list, function(k) {
    temp <- utils::read.csv(k)
    temp <- temp |> dplyr::mutate(filename = paste(basename(k)))
    temp
  }) |> dplyr::bind_rows()


  aresults <- aresults |> dplyr::mutate(balance = gsub(".csv", "", .data$filename))

  return(aresults)
}














get_tiles <- function(tile_dir, template, tile_size) {
  if (!dir.exists(file.path(tile_dir))) {
    dir.create(file.path(tile_dir))

    ntiles <- terra::makeTiles(template, tile_size, filename = file.path(tile_dir, "tile_.tif"), na.rm = FALSE, overwrite = TRUE)

    cli::cli_alert_info("Creating tiles")
  } else if (dir.exists(file.path(tile_dir))) {
    ntiles <- list.files(tile_dir, full.names = T)
  }

  return(ntiles)
}
