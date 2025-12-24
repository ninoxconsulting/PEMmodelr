#' Generate Final Model summary
#'
#' This function takes in all the data needed to produce machine learning model.
#' Inputs are handed to a RMD report/ script.
#' Outputs include the markdown report, the cross validation object,
#' and a binary model (RDS) that can then be used to predict on new data.
#'
#'
#' @param model_name A character string to define the model. Default will use balance combination
#' @param final_data  final model training point
#' @param bec A character with the BEC label to use. FOr example "ICHmc1".
#' @param covars A vector with the names of covariates to use. These match raster names.
#' @param extra_pts logical. If TRUE, extra points will be included. Default is FALSE.
#' @param mtry numeric. This is the output based on output of hyperparamter model tuning (default = ??)
#' @param min_n numeric. This is the output based on output of hyperparamter model tuning (default = ??)
#' @param ntrees numeric. Number of trees to use in random forest model. Default is 151.
#' @param downsample_ratio A vector of numeric downsampling values (10 - 100), NA if not using
#' @param smote_ratio A vector of numeric downsampling values (0.1 - 0.9), NA if not using
#' @param out_dir OPTIONAL: only needed if detailed_output = TRUE. location of filepath there detailed outputs to be stored
#' @param final_model  final model object
#'
#' @export
#' @examples
#' \dontrun{
#' final_model_report(final_data, final_model, out_bgc_dir)
#' }

final_model_report <- function(model_name,
                               final_data,
                               bec,
                               covars,
                               extra_pts,
                               mtry,
                               min_n,
                               ntrees,
                               downsample_ratio,
                               smote_ratio,
                               out_dir,
                               final_model
                               ){

  ## create destination folder
  ifelse(!dir.exists(file.path(out_dir)),
         dir.create(file.path(out_dir)), FALSE)

  #RMD <- system.file("rmd_template", "final_model_report.rmd", package = "PEMmodelr")
  RMD <- fs::path_package("PEMmodelr", "extdata/final_model_report.rmd")

  rmarkdown::render(RMD,
                    params = list(model_name = model_name,
                                  final_data = final_data,
                                  bec= bec,
                                  covars=  covars,
                                  extra_pts=  extra_pts,
                                  mtry=  mtry,
                                  min_n=  min_n,
                                  ntrees=  ntrees,
                                  downsample_ratio=  downsample_ratio,
                                  smote_ratio=  smote_ratio,
                                  out_dir=  out_dir,
                                  final_model= final_model),
                    output_dir = out_dir)                ## where to save the report

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
