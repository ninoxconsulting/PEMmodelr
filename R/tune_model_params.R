#' Tune model parameters for random forest model per model type or BGC unit
#'
#' @param prepped_points A list of prepped points for each BGC unit or model type. The output of `prep_model_tps()` function
#' @param covars A character string with the names of the covariates to be used in the model.
#' @param min_no A numeric value of the minimum number of points required for each map unit. Default is 10.
#' @param output_type A character string of the output type. Options are "best" or "full". Default is "best"
#' @param accuracy_type A character string of the accuracy type. Options are "roc_auc" or "accuracy". Default is "roc_auc"
#' @param out_dir A character string of the path to the output directory.
#'
#' @returns a csv file with the best tuning parameters for each model type or BGC unit
#' @export
#'
#' @examples
#' \dontrun{
#' tune_model_params = function(
#'    prepped_points = NA,
#'    covars = NA,
#'    min_no = 10,
#'    output_type = "best",
#'    accuracy_type = "roc_auc",
#'    out_dir = NA)
#' }
tune_model_params <- function(
    prepped_points = NA,
    covars = NA,
    min_no = 10,
    output_type = "best",
    accuracy_type = "roc_auc",
    out_dir = NA) {

  bgc_pts_subzone <- lapply(names(prepped_points), function(i) {
    # i =  names(prepped_points)[1]

    out_bgc_dir <- fs::path(out_dir, i)

    if (!dir.exists(out_bgc_dir)) {
      fs::dir_create(out_bgc_dir)
    }

    trDat <- prepped_points[[i]] |>
      dplyr::filter(.data$position %in% "Orig") |>
      select_pure_training() |>
      dplyr::select(.data$mapunit1, .data$slice, dplyr::any_of(covars)) |>
      dplyr::mutate(slice = as.factor(.data$slice)) |>
      tidyr::drop_na() |>
      dplyr::mutate(mapunit1 = factor(.data$mapunit1)) |>
      droplevels()

    # remove points with less than min_no points
    trDat <- .filter_min_mapunits(trDat, min_no) |>
      droplevels()

    # tune the parameters
    tune_res <- tune_rf(trDat)

    if (output_type == "full") {
      print("returning full tuning outputs")

      out <- tune_res

    } else if (output_type == "best") {
      print("returning best option only")

      if (accuracy_type == "accuracy") {
        out <- tune::select_best(tune_res, metric = accuracy_type)
      }

      if (accuracy_type == "roc_auc") {
        out <- tune::select_best(tune_res, metric = "roc_auc")
      }
    }

    utils::write.csv(out, fs::path(out_bgc_dir, "best_tuning.csv"))
  })

  return(out)
}
