#' Run the basic model
#'
#' @param bgc_pts_subzone A list of prepped data. Output of `prep_model_tps()`
#' @param fuzz_matrix a data table with fuzzy metrics, generated using the
#'  `generate_fuzzy_matrix()` function
#' @param covars a character vector of covariates to include in the model
#' @param use_neighbours if you want to incluse all neighbours in the calculation
#' @param report logical. If TRUE, a report will be generated
#' @param detailed_output OPTIONAL:TRUE/FALSE if you want to output all raw values
#' this is used to determine optimum theta values
#' @param out_dir OPTIONAL: only needed if detailed_output = TRUE. location of
#' filepath there detailed outputs to be stored
#'
#' @returns a list of dataframes with the accuracy metrics for each model
#' @export
#'
#' @examples
#' \dontrun{
#' run_base_model(bgc_pts_subzone, fuzz_matrix, covars, use_neighbours,
#'   detailed_output = FALSE, out_dir
#' )
#' }
run_base_model <- function(
    bgc_pts_subzone,
    fuzz_matrix = NA,
    covars = covars,
    use_neighbours = FALSE,
    report = FALSE,
    detailed_output = TRUE,
    out_dir = NA) {

  model_bgc <- lapply(names(bgc_pts_subzone), function(i) {
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

    # always use best tune per bgc model
    best_tune <- utils::read.csv(fs::path(out_bgc_dir, "best_tuning.csv"))
    mtry <- best_tune$mtry
    min_n <- best_tune$min_n

    baseout <- base_model(
      train_data,
      fuzz_matrix = fuzz_matrix,
      mtry = mtry,
      min_n = min_n,
      use_neighbours = use_neighbours,
      detailed_output = detailed_output,
      out_dir = out_bgc_dir
    )

    utils::write.csv(baseout, fs::path(out_bgc_dir, "acc_base_results.csv"))

    if(report){
    # generate model accuracy report
    model_report(train_data, fuzz_matrix, use_neighbours,
                 mtry, min_n, baseout, out_bgc_dir)
    }
  })

  return(out_dir)
}
