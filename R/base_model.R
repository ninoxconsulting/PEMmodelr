#' Run the basic model
#'
#' @param train_data A list of prepped data. Output of `prep_model_tps()`
#' @param fuzz_matrix data table with fuzzy metrics
#' @param mtry numeric. This is the output based on output of hyperparamter model tuning (default = ??)
#' @param min_n numeric. This is the output based on output of hyperparamter model tuning (default = ??)
#' @param use_neighbours if you want to incluse all neighbours in the calculation
#' @param detailed_output OPTIONAL:TRUE/FALSE if you want to output all raw values this is used to determine optimum theta values
#' @param out_dir OPTIONAL: only needed if detailed_output = TRUE. location of filepath there detailed outputs to be stored
#' @return datatable of accuracy metric
#' @export
#' @examples
#' \dontrun{
#' base_model(train_pts, fuzz_matrix,
#'   mtry = 14, min_n = 7, use_neighbours = TRUE,
#'   detailed_output = TRUE, outdir
#' )
#' }
base_model <- function(train_data,
                       fuzz_matrix,
                       mtry = 14,
                       min_n = 7,
                       use_neighbours = TRUE,
                       detailed_output = TRUE,
                       out_dir) {
  # training set - train only on pure calls
  ref_dat <- train_data |>
    dplyr::filter(!is.na(.data$slice)) |>
    dplyr::mutate(
      mapunit1 = as.factor(.data$mapunit1),
      slice = as.factor(.data$slice)
    )

  cli::cli_alert_info("Training raw data models...")

  munits <- unique(ref_dat$mapunit1)

  # place holder to catch non-forest within a forest model

  if ("forest" %in% munits) {
    nf_mapunits <- NA
  } else {
    nf_mapunits <- grep(munits, pattern = "_\\d", value = TRUE, invert = TRUE)
  }


  slices <- unique(ref_dat$slice) |> droplevels()

  # check the no of slices
  if (length(slices) < 2) { # switching to transect iteration instead of slices

    ref_dat_key <- ref_dat |>
      dplyr::select(c(.data$tid)) |>
      dplyr::distinct() |>
      dplyr::mutate(slice = as.factor(seq(1, length(.data$tid), 1)))

    ref_dat <- ref_dat |>
      dplyr::select(-.data$slice) |>
      dplyr::left_join(ref_dat_key)

    slices <- unique(ref_dat$slice) |> droplevels()
  }

  ref_acc <- purrr::map(levels(slices), function(k) {
    # k = levels(slices)[3]

    # create training set
    ref_train <- .create_training_set(ref_dat, k)

    MU_count <- ref_train |>
      dplyr::count(.data$mapunit1) |>
      dplyr::filter(.data$n > 10)

    ref_train <- ref_train |>
      dplyr::filter(.data$mapunit1 %in% MU_count$mapunit1) |>
      droplevels()

    ref_test <- .create_test_set(ref_dat, k, use_neighbours, MU_count)

    ref_id <- ref_test |> dplyr::select(.data$id, .data$mapunit1, .data$mapunit2)

    null_recipe <- recipes::recipe(mapunit1 ~ ., data = ref_train) |>
      recipes::update_role(.data$tid, new_role = "id variable")

    randf_spec <- parsnip::rand_forest(mtry = mtry, min_n = min_n, trees = 151) |>
      parsnip::set_mode("classification") |>
      parsnip::set_engine("ranger", importance = "permutation", verbose = FALSE)

    pem_workflow <- workflows::workflow() |>
      workflows::add_recipe(null_recipe) |>
      workflows::add_model(randf_spec)

    ref_mod <- parsnip::fit(pem_workflow, ref_train)

    final_fit <- tune::extract_fit_parsnip(ref_mod)

    oob <- round(ref_mod$fit$fit$fit$prediction.error, 3)

    preds <- terra::predict(ref_mod, ref_test)

    pred_all <- cbind(ref_id, .pred_class = preds$.pred_class)

    pred_all <- .prep_model_output(pred_all, nf_mapunits)

    cli::cli_alert_info("generating accuracy metrics for slice:{ k }")

    if (detailed_output == TRUE) {
      saveRDS(pred_all, fs::path(out_dir, paste0("predictions_", k)))
    }

    acc <- acc_metrics(pred_all, fuzz_matrix = fuzz_matrix) |>
      dplyr::mutate(
        slice = k,
        oob = oob
      )
  }) |> dplyr::bind_rows()

  return(ref_acc)
}
