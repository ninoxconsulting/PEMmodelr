#' Run Balance optimisation process
#'
#' This process will generate accuracty metrics for designated downsample (ds) and
#' upsample (smote) iterations. All results are saved into a balance subfolder which
#' is then summarised and the best metrics is exported as a separate csv.
#'
#' @param bgc_pts_subzone A list of prepped data. Output of `prep_model_tps()`
#' @param fmat data table with fuzzy metrics
#' @param covars a character vector of covariates to include in the model
#' @param out_dir A character string of the output directory
#' @param ds_iterations A vector of numeric downsampling values (10 - 100), NA if not using
#' @param smote_iterations A vector of numeric downsampling values (0.1 - 0.9), NA if not using
#' @param use_neighbours Logical to assign neighbours of not. Default is FALSE
#'
#' @returns A list of data tables with the best balance metrics
#' @export
#'
#' @examples
#' \dontrun{
#' run_balance_optimisation(bgc_pts_subzone,
#'   fmat, covars, out_dir,
#'   ds_iterations = c(10, 20, 30, 40, 50, 60, 70, 80, 90),
#'   smote_iterations = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), use_neighbours = FALSE
#' )
#' }
run_balance_optimisation <- function(bgc_pts_subzone,
                                     fmat,
                                     covars,
                                     out_dir,
                                     ds_iterations = c(10, 20, 30, 40, 50, 60, 70, 80, 90),
                                     smote_iterations = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
                                     use_neighbours = FALSE) {
  bal_bgc <- lapply(names(bgc_pts_subzone), function(i) {
    # i <- names(bgc_pts_subzone[1])

    tdat <- bgc_pts_subzone[[i]]

    out_bgc_dir <- fs::path(out_dir, i)

    tdat <- tdat |>
      dplyr::select(
        .data$id, .data$mapunit1, .data$mapunit2, .data$position,
        .data$transect_id, .data$tid, .data$slice, dplyr::any_of(covars)
      )

    tdat <- tdat[stats::complete.cases(tdat[, 8:length(tdat)]), ]

    train_data <- droplevels(tdat)

    # check if best balance exist
    if (!fs::file_exists(fs::path(out_dir, i, "best_tuning.csv"))) {
      cli::cli_abort("No best tuning file found, please run `tune_model_params()` before running `run_balance_optimisation()`")
    }

    best_tune <- utils::read.csv(fs::path(out_dir, i, "best_tuning.csv"))
    mtry <- best_tune$mtry
    min_n <- best_tune$min_n

    balance_optimisation(
      train_data = train_data,
      fuzz_matrix = fmat,
      ds_iterations = ds_iterations,
      smote_iterations = smote_iterations,
      mtry = mtry,
      min_n = min_n,
      use_neighbours = FALSE,
      out_dir = out_bgc_dir
    )

    # combine all metrics and select best balance option
    allbals <- combine_balance_outputs(out_bgc_dir)

    best_metrics <- select_best_acc(allbals)

    utils::write.csv(best_metrics, file.path(out_bgc_dir, "best_balancing.csv"))
  })
  return(TRUE)
}
