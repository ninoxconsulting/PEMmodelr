#' Run the final model fit
#'
#' @param train_data A list of prepped data. Output of `prep_model_tps()`
#' @param covars a character vector of covariates to include in the model
#' @param ds_ratio numeric
#' @param sm_ratio numeric
#' @param mname character with name to be used in output
#' @param report logical. If TRUE, a report will be generated.{PLACEHOLDER}
#' @param out_dir root level out directory. A sub-folder will be created per
#' model type (i.e "fnf" or "bgc level)
#'
#' @returns a list of dataframes with final models
#' @export
#'
#' @examples
#' \dontrun{
#' run_final_model(train_data, covars, ds_ratio = NA, sm_ratio = NA,
#' mname = "base", report = FALSE, out_dir)
#' }
run_final_model <- function (
    train_data,
    covars = covars,
    ds_ratio = NA,
    sm_ratio = NA,
    mname  =  "base",
    report = FALSE,
    out_dir = NA
){

  final_bgc <- lapply(names(train_data), function(i) {

    #i <- names(train_data[1])

    alldat <- train_data[[i]]

    #create a subfolder for each BGC unit
    out_bgc_dir = fs::path(out_dir, i)

    best_tune <- utils::read.csv(fs::path(out_bgc_dir, "best_tuning.csv"))
    mtry <- best_tune$mtry
    min_n <- best_tune$min_n


    final_data <- alldat |>
      dplyr::filter(.data$position == "Orig") |>
      dplyr::select(.data$mapunit1, dplyr::any_of(covars))

    final_data <- final_data[stats::complete.cases(final_data[, 2:length(final_data)]), ]

    final_model <- final_model(
      final_data,
      mtry = mtry,
      min_n = min_n,
      ds_ratio = ds_ratio,
      sm_ratio = sm_ratio
    )

    # Output model
    cli::cli_alert_success("model fit complete and written to {out_bgc_dir}")
    saveRDS(final_model, fs::path(out_bgc_dir, paste0("final_model_", mname, ".rds")))

    # generate a report if requested

    if(report){
      # final_model_report(bgc_bal, final_data, final_model, outDir)
    }

  })
  return(final_bgc)
}




