#' Generate Final Model summary
#'
#' This function takes in all the data needed to produce machine learning model.
#' Inputs are handed to a RMD report/ script.
#' Outputs include the markdown report, the cross validation object,
#' and a binary model (RDS) that can then be used to predict on new data.
#' @param final_data  final model training point
#' @param final_model  final model object
#' @param mbaldf the balance options applied to model
#' @param out_bgc_dir  output directory  This defaults to the project's root directory OR where the RMD script is saved.
#' @param extra_pts  extra points to be included in the model
#' @export
#' @examples
#' \dontrun{
#' final_model_report(final_data, final_model, out_bgc_dir)
#' }

final_model_report <- function(mbaldf, final_data, final_model, out_bgc_dir, extra_pts){

  ## create destination folder
  ifelse(!dir.exists(file.path(out_bgc_dir)),
         dir.create(file.path(out_bgc_dir)), FALSE)

  #RMD <- system.file("rmd_template", "final_model_report.rmd", package = "PEMmodelr")
  RMD <- fs::path_package("PEMmodelr", "extdata/final_model_report.rmd")

  rmarkdown::render(RMD,
                    params = list(mbaldf = mbaldf,
                                  final_data = final_data,
                                  final_model = final_model,
                                  out_bgc_dir = out_bgc_dir,
                                  extra_pts = extra_pts),
                    output_dir = out_bgc_dir)                ## where to save the report

  ## open the report
  #browseURL(paste0(paste0(out_bgc_dir,"/","final_model_report.html")))
}




model_report <- function(model_name, bec, train_data, fuzz_matrix, covars,
                         use_neighbours,extra_pts,
                         mtry, min_n, ntrees,
                         nf_f_filter,
                         smote_ratio,
                         downsample_ratio,
                         ref_acc,out_dir){


 # ifelse(!dir.exists(file.path(out_bgc_dir)),
#         dir.create(file.path(out_bgc_dir)), FALSE)

  RMD <- fs::path_package("PEMmodelr", "extdata/model_report.rmd")

  # convert nf_f_filter to a true false value
  if(is.null(nf_f_filter)){
    nf_f_filter = FALSE
  }else{
    nf_f_filter = TRUE
  }

  rmarkdown::render(RMD,
                    params = list(model_name = model_name,
                                  bec = bec,
                                  train_data = train_data,
                                  fuzz_matrix = fuzz_matrix,
                                  covars = covars,
                                  use_neighbours = use_neighbours,
                                  extra_pts = extra_pts,
                                  mtry =mtry,
                                  min_n = min_n,
                                  ntrees = ntrees,
                                  nf_f_filter = nf_f_filter,
                                  smote_ratio = smote_ratio,
                                  downsample_ratio = downsample_ratio,
                                  ref_acc= ref_acc,
                                  out_dir = out_dir),
                    output_dir = out_dir)## where to save the report


  ## open the report
  #browseURL(paste0(paste0(out_bgc_dir,"/","final_model_report.html")))
}
