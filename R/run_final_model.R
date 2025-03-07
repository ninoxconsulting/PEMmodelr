#' Run the final model fit
#'
#' @param train_data A list of prepped data. Output of `prep_model_tps()`
#' @param covars a character vector of covariates to include in the model
#' @param ds_ratio numeric
#' @param sm_ratio numeric
#' @param model_bal the type of model to be run. This can be the base model or a balance model
#' based on the best_balancing csv. Options include:"aspat_paf_theta.5" ,"aspat_paf_theta0" ,
#' "aspat_paf_theta1" , "aspatial_sum", "spat_paf_theta.5" , "spat_paf_theta0", "spat_paf_theta1",
#' "spatial_sum", "overall"
#' @param extra_pts logical. If TRUE, extra points will be included. Default is FALSE.
#' @param report logical. If TRUE, a report will be generated.
#' @param out_dir root level out directory. A sub-folder will be created per
#' model type (i.e "fnf" or "bgc level)
#'
#' @returns a list of dataframes with final models
#' @export
#'
#' @examples
#' \dontrun{
#' run_final_model(train_data, covars, model_bal = "base", report = FALSE, out_dir)
#' }
run_final_model <- function (
    train_data,
    covars = covars,
    model_bal  =  "base",
    extra_pts = FALSE,
    report = FALSE,
    out_dir = NA,
    ds_ratio = NA,
    sm_ratio = NA
){

  final_bgc <- lapply(names(train_data), function(i) {

    #i <- names(train_data[1])

    alldat <- train_data[[i]]

    #create a subfolder for each BGC unit
    out_bgc_dir = fs::path(out_dir, i)

    # read in tuning
    best_tune <- utils::read.csv(fs::path(out_bgc_dir, "best_tuning.csv"))
    mtry <- best_tune$mtry
    min_n <- best_tune$min_n


    if(model_bal == "base") {

      mbaldf <- data.frame(balance = "base", ds_ratio = NA_integer_, sm_ratio = NA_integer_)

    } else {

      # read in balance
      best_balance <- utils::read.csv(fs::path(out_bgc_dir, "best_balancing.csv"))

      mbaldf <- best_balance |>
        dplyr::filter(.data$maxmetric == model_bal) |>
        dplyr::select( .data$balance, .data$ds_ratio, .data$sm_ratio)
    }

    final_data <- alldat |>
      dplyr::filter(.data$position == "Orig") |>
      dplyr::select(.data$mapunit1, dplyr::any_of(covars))

    if(extra_pts){
      extras <- alldat |>
        dplyr::filter(.data$data_type == "incidental") |>
        dplyr::filter(is.na(.data$mapunit2))|>
        dplyr::select(.data$mapunit1, dplyr::any_of(covars))

      final_data <- rbind(final_data, extras)
    }

    final_data <- final_data[stats::complete.cases(final_data[, 2:length(final_data)]), ]

    final_model <- final_model(
      final_data,
      mtry = mtry,
      min_n = min_n,
      ds_ratio = mbaldf$ds_ratio,
      sm_ratio = mbaldf$sm_ratio
    )

    # Output model
    cli::cli_alert_success("model fit complete and written to {out_bgc_dir}")
    saveRDS(final_model, fs::path(out_bgc_dir, paste0("final_model_", model_bal, ".rds")))

    # generate a report if requests

    if(report){
       final_model_report(mbaldf, final_data, final_model, out_bgc_dir)
    }

  })
  return(TRUE)
}




