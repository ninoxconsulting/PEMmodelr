#' Run the final model fit
#'
#' @param model_name A character string to define the model. Default will use balance combination
#' @param bgc_pts_subzone Datasets list with all formatted training data. Output of prep_model_tps()
#' @param bec A character with the BEC label to use. FOr example "ICHmc1".
#' @param covars A vector with the names of covariates to use. These match raster names.
#' @param extra_pts logical. If TRUE, extra points will be included. Default is FALSE.
#' @param mtry numeric. This is the output based on output of hyperparamter model tuning (default = ??)
#' @param min_n numeric. This is the output based on output of hyperparamter model tuning (default = ??)
#' @param ntrees numeric. Number of trees to use in random forest model. Default is 151.
#' @param downsample_ratio A vector of numeric downsampling values (10 - 100), NA if not using
#' @param smote_ratio A vector of numeric downsampling values (0.1 - 0.9), NA if not using
#' @param report TRUE/FALSE: if you want to produce a pdf report summarising model inputs and model outputs
#' this is used to determine optimum theta values
#' @param out_dir OPTIONAL: only needed if detailed_output = TRUE. location of filepath there detailed outputs to be stored
#' @returns a list of dataframes with final models
#' @export
#'
#' @examples
#' \dontrun{
#' run_final_model(train_data, covars, model_bal = "base", report = FALSE, out_dir)
#' }
run_final_model <- function (
    model_name = "final",
    bgc_pts_subzone,
    bec = bec,
    covars = covars,
    extra_pts = TRUE,
    mtry = mtry,
    min_n = min_n,
    ntrees = 151,
    downsample_ratio = FALSE,
    smote_ratio = FALSE,
    report = FALSE,
    out_dir = NA
){

  # # testing lines
  #
  # bec <- "ICHmc1" #"ESSFwv" "ICHmc2"
  # out_bgc_dir <- fs::path(out_dir, bec)
  # # read best tune
  # best_tune <- utils::read.csv(fs::path(out_bgc_dir, "best_tuning.csv"))
  # mtry <- best_tune$mtry
  # min_n <- best_tune$min_n
  # model_name = "final"
  # bgc_pts_subzone = bgc_pts_subzone
  # covars = covars
  # extra_pts = TRUE
  # ntrees = 151
  # downsample_ratio = FALSE
  # smote_ratio = FALSE
  # report = FALSE
  # out_dir = out_bgc_dir
  # # end testing
  #

  # select the bec zone of interest
  tdat <- bgc_pts_subzone[[bec]]

  tdat <- tdat |>
    dplyr::select(
      .data$id, .data$X , .data$Y, .data$mapunit1, .data$mapunit2, .data$position, .data$data_type,
      .data$transect_id, .data$tid, .data$slice, dplyr::any_of(covars)
    )

  final_data <- tdat |>
    dplyr::filter(.data$position == "Orig") |>
    dplyr::select(.data$mapunit1, dplyr::any_of(covars))

  if(extra_pts){
    extras <- tdat |>
      dplyr::filter(.data$data_type == "incidental") |>
      #dplyr::filter(is.na(.data$mapunit2))|>
      dplyr::select(.data$mapunit1, dplyr::any_of(covars))

    final_data <- rbind(final_data, extras)
  }


  final_data <- final_data[stats::complete.cases(final_data[, 2:length(final_data)]), ]

  # smote data if specified
  #smote_ratio = FALSE

  if (!smote_ratio == FALSE) {
    cli::cli_alert_success("smoting data")
    smote_recipe <- recipes::recipe(mapunit1 ~ ., data = final_data) |>
      # recipes::update_role(tid, new_role = "id variable") |>
      themis::step_upsample(mapunit1, over_ratio = smote_ratio) |>
      recipes::prep()
    final_data <- recipes::juice(smote_recipe)
  }

  final_data <- final_data[stats::complete.cases(final_data[, 2:length(final_data)]), ]
  #
  #     MU_count <- final_data |> dplyr::count(.data$mapunit1) |> dplyr::filter(.data$n > 10)
  #
  #     final_data <- final_data |> dplyr::filter(.data$mapunit1 %in% MU_count$mapunit1)  |>
  #       droplevels()
  #

  # Use corresponding downscale ratio
  if (downsample_ratio == FALSE) {
    null_recipe <- recipes::recipe(mapunit1 ~ ., data = final_data) #|>
    # recipes::update_role(.data$tid, new_role = "id variable")
    print("no downsampling")
  } else {
    null_recipe <- recipes::recipe(mapunit1 ~ ., data = final_data) |>
      #recipes::update_role(.data$tid, new_role = "id variable") |>
      themis::step_downsample(mapunit1, under_ratio = downsample_ratio)
    print("yes downsampling")
  }


  #set up model params
  randf_spec <- parsnip::rand_forest(mtry = mtry, min_n = min_n, trees = ntrees) |>
    parsnip::set_mode("classification") |>
    parsnip::set_engine("ranger", importance = "permutation", splitrule = "gini", verbose = FALSE, probability = TRUE)

  # apply model to the workflow
  pem_workflow <- workflows::workflow() |>
    workflows::add_recipe(null_recipe) |>
    workflows::add_model(randf_spec)


  print("running final PEM model")

  final_model <- parsnip::fit(pem_workflow, final_data)


  # Output model
  cli::cli_alert_success("model fit complete and written to {out_dir}")
  saveRDS(final_model, fs::path(out_dir, paste0("final_model_", model_name, ".rds")))

  # generate a report if requested

  # if(report){
  #    final_model_report(mbaldf, final_data, final_model, out_bgc_dir)
  #  }


  return(TRUE)
}
