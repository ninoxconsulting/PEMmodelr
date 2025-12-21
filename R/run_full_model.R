#' Run full model
#'
#' @param model_name A character string to define the model. Default will use balance combination
#' @param bgc_pts_subzone Datasets list with all formatted training data. Output of prep_model_tps()
#' @param bec A character with the BEC label to use. FOr example "ICHmc1".
#' @param fuzz_matrix data table with fuzzy metrics.
#' @param covars A vector with the names of covariates to use. These match raster names.
#' @param use_neighbours TRUE/FALSE. Define if you want to include all neighbours in the calculation
#' @param extra_pts logical. If TRUE, extra points will be included. Default is FALSE.
#' @param mtry numeric. This is the output based on output of hyperparamter model tuning (default = ??)
#' @param min_n numeric. This is the output based on output of hyperparamter model tuning (default = ??)
#' @param ntrees numeric. Number of trees to use in random forest model. Default is 151.
#' @param downsample_ratio A vector of numeric downsampling values (10 - 100), NA if not using
#' @param smote_ratio A vector of numeric downsampling values (0.1 - 0.9), NA if not using
#' @param nf_f_filter A SpatRaster with binary forest (0) and nonforest (1)
#' @param theta numeric to decide what level of theta to report in the outputs. Default will produce theta (0,1)
#' @param report TRUE/FALSE: if you want to produce a pdf report summarising model inputs and model outputs
#' @param detailed_output OPTIONAL:TRUE/FALSE if you want to output all raw values
#' this is used to determine optimum theta values
#' @param out_dir OPTIONAL: only needed if detailed_output = TRUE. location of filepath there detailed outputs to be stored
#' @return datatable of accuracy metric
#'
#' @export
#' @examples
#' \dontrun{
#' run_full_model(
#'  model_name = "base_model",
#'  bgc_pts_subzone = bgc_pts_subzone,
#'  bec = bec,
#'  fuzz_matrix = fuzz_matrix,
#'  covars = covars,
#'  use_neighbours = FALSE,
#'  extra_pts = FALSE,
#'  mtry = mtry,
#'  min_n = min_n,
#'  ntrees = 151,
#'  downsample_ratio = FALSE,
#'  smote_ratio = FALSE,
#'  nf_f_filter = nf_f_filter,
#'  heta = c(0,1),
#'  report = FALSE,
#'  detailed_output = TRUE,
#'  out_dir = out_bgc_dir
#' )
#' }
run_full_model <- function(
    model_name = NULL,
    bgc_pts_subzone,
    bec = bec,
    fuzz_matrix = NA,
    covars = covars,
    use_neighbours = FALSE,
    extra_pts = FALSE,
    mtry = mtry,
    min_n = min_n,
    ntrees = 151,
    downsample_ratio = FALSE,
    smote_ratio = FALSE,
    nf_f_filter = FALSE,
    theta = c(0,0.5,1),
    report = FALSE,
    detailed_output = TRUE,
    out_dir = NA) {


  # # ## testing lines
  # model_name = "testing"
  # bgc_pts_subzone = bgc_pts_subzone
  # bec = bec
  # fuzz_matrix = fuzz_matrix
  # covars = covars
  # use_neighbours = FALSE
  # extra_pts = FALSE
  # mtry = mtry
  # min_n = min_n
  # ntrees = 151
  # downsample_ratio = FALSE
  # smote_ratio = FALSE
  # nf_f_filter = nf_f_filter
  # theta = 0.5
  # report = FALSE
  # detailed_output = TRUE
  # out_dir = out_bgc_dir


  #Generate a model name if missing

  if(is.null(model_name)){
    downsample_no <- ifelse(downsample_ratio == FALSE, 0, downsample_ratio)
    smote_no <- ifelse(smote_ratio == FALSE, 0, smote_ratio)
    model_name <- paste0(bec, "_ds", downsample_no, "_sm", smote_no)
    cli::cli_alert_info("creating model name : {model_name}")
  }

  # select the bec zone of interest
  tdat <- bgc_pts_subzone[[bec]]

  # format colums and check all are complete (as required for model)
  tdat <- tdat |>
    dplyr::select(
      .data$id, .data$X , .data$Y, .data$mapunit1, .data$mapunit2, .data$position, .data$data_type,
      .data$transect_id, .data$tid, .data$slice, dplyr::any_of(covars)
    )

  tdat <- tdat[stats::complete.cases(tdat[, 11:length(tdat)]), ]

  train_data <- droplevels(tdat)

  # Add extra points if specified - extra points set - only on pure calls

  if(extra_pts){
    extras <- train_data |>
      dplyr::filter(.data$data_type == "incidental") |>
      dplyr::filter(is.na(.data$mapunit2))
  }

  # training set - train only on pure calls
  ref_dat <- train_data |>
    #dplyr::filter(!is.na(.data$slice)) |>
    dplyr::mutate(
      mapunit1 = as.factor(.data$mapunit1),
      slice = as.factor(.data$slice)
    )

  cli::cli_alert_info("Training raw data models...")

  slices <- unique(ref_dat$slice) |> droplevels()

  # # check the no of slices
  # if (length(slices) < 2) { # switching to transect iteration instead of slices
  #
  #   ref_dat_key <- ref_dat |>
  #     dplyr::select(c(.data$tid)) |>
  #     dplyr::distinct() |>
  #     dplyr::mutate(slice = as.factor(seq(1, length(.data$tid), 1)))
  #
  #   ref_dat <- ref_dat |>
  #     dplyr::select(-.data$slice) |>
  #     dplyr::left_join(ref_dat_key)
  #
  #   slices <- unique(ref_dat$slice) |> droplevels()
  # }
  #

  ref_acc <- purrr::map(levels(slices), function(k) {
    # test line
    #k = levels(slices)[2]

    # create training set
    ref_train <- ref_dat |>
      dplyr::filter(!.data$slice %in% k) |>
      dplyr::filter(is.na(.data$mapunit2)) |> # train only on pure calls
      dplyr::filter(.data$position == "Orig") |>
      dplyr::select(-.data$id, -.data$slice, -.data$mapunit2, -.data$position, -.data$transect_id,
                    -.data$data_type, -.data$X, -.data$Y) |>
      droplevels()

    #ref_train <- ref_train |>
    #    dplyr::select(-.data$data_type, -.data$X, -.data$Y)

    # drop the nf points for training dataset
    allmunits <- unique(ref_train$mapunit1)
    nfmunits <- grep(allmunits, pattern = "_\\d", value = TRUE, invert = TRUE)

    ref_train <- ref_train |>
      dplyr::filter(!.data$mapunit1 %in% nfmunits)

    if(extra_pts){
      extras <- extras |>
        dplyr::select(dplyr::any_of(names(ref_train)))

      ref_train <- rbind(ref_train, extras)
    }


    if (!smote_ratio == FALSE) {
      cli::cli_alert_success("smoting data")
      smote_recipe <- recipes::recipe(mapunit1 ~ ., data = ref_train) |>
        recipes::update_role(tid, new_role = "id variable") |>
        themis::step_upsample(mapunit1, over_ratio = smote_ratio) |>
        recipes::prep()
      ref_train <- recipes::juice(smote_recipe)

    }

    MU_count <- ref_train |>
      dplyr::count(.data$mapunit1) |>
      dplyr::filter(.data$n > 10)

    ref_train <- ref_train |>
      dplyr::filter(.data$mapunit1 %in% MU_count$mapunit1) |>
      droplevels()



    # create a test set

    ref_test <- ref_dat |>
      dplyr::filter(.data$slice %in% c(k)) |>
      dplyr::filter(.data$mapunit1 %in% MU_count$mapunit1) |>
      droplevels()

    # if not using neighbours then select only Origin data position
    if (use_neighbours == FALSE) {

      print("only using the Orig locations")

      ref_test <- ref_test |>
        dplyr::filter(.data$position == "Orig")
    }

    ref_test_all <- ref_test
    ref_test_transect_no <- length(unique(ref_test$tid))

    ref_id <- ref_test |> dplyr::select(.data$id, .data$X, .data$Y, .data$mapunit1, .data$mapunit2)
    ref_id <- ref_id |>
      dplyr::rename("x" = .data$X, 'y' = .data$Y)

    # Define recipe and model
    if (downsample_ratio == FALSE) {
      null_recipe <- recipes::recipe(mapunit1 ~ ., data = ref_train) |>
        recipes::update_role(.data$tid, new_role = "id variable")
      print("no downsampling")
    } else {
      null_recipe <- recipes::recipe(mapunit1 ~ ., data = ref_train) |>
        recipes::update_role(.data$tid, new_role = "id variable") |>
        themis::step_downsample(mapunit1, under_ratio = downsample_ratio)
      print("yes downsampling")
    }

    # specify the machine learning model parameters
    randf_spec <- parsnip::rand_forest(mtry = mtry, min_n = min_n, trees = ntrees) |>
      parsnip::set_mode("classification") |>
      parsnip::set_engine("ranger", importance = "permutation", verbose = FALSE)

    # apply model to the workflow
    pem_workflow <- workflows::workflow() |>
      workflows::add_recipe(null_recipe) |>
      workflows::add_model(randf_spec)


    # fit model using training data
    ref_mod <- parsnip::fit(pem_workflow, ref_train)

    # extract the model component
    final_fit <- tune::extract_fit_parsnip(ref_mod)

    # calcualte out of bag
    oob <- round(ref_mod$fit$fit$fit$prediction.error, 3)

    preds <- terra::predict(ref_mod, ref_test)
    preds <- cbind(preds, ref_id)

    #preds <- tolower(preds)
    #mutate_if(is.factor,as.character) %>%  distinct()

    # if using the non-forest template filter then update the predictions and
    # mapunits base on binary template

    if (!is.null(nf_f_filter)) {

      # convert to dataframe ans keep XY value
      nf_df <- terra::as.data.frame(nf_f_filter, xy = T)

      # join the forest binary template to predictions by x and y ( # 0 = forest 1 = non-forest)

      test.pred <- preds |>
        dplyr::left_join(nf_df, by = c("x","y")) |>
        dplyr::mutate(.pred_class = as.character(.data$.pred_class),
                      mapunit1 = as.character(.data$mapunit1),
                      mapunit2 = as.character(.data$mapunit2))

      test.pred <- test.pred |>
        dplyr::mutate(.pred_class = ifelse(.data$forest_nonforest ==  1,  "nonfor", .data$.pred_class)) |>
        dplyr::mutate(
          mapunit1 = ifelse(grepl("^[[:alpha:]]+$", .data$mapunit1), "nonfor", .data$mapunit1),
          mapunit2 = ifelse(grepl("^[[:alpha:]]+$", .data$mapunit2), "nonfor", .data$mapunit2),
          .pred_class = ifelse(grepl("^[[:alpha:]]+$", .data$.pred_class)|.data$.pred_class == "nonfor", "nonfor", .data$.pred_class)
        ) |>
        dplyr::select(.data$id,  .data$mapunit1, .data$mapunit2, .data$.pred_class)

    } else {
      # if not using nf filter then just use outputs
      test.preds = preds

    }
    # harmonize factor levels
    pred_all <- .harmonize_factors(test.pred)
    pred_all$mapunit2 <- as.factor(pred_all$mapunit2)


    cli::cli_alert_info("generating accuracy metrics for slice:{ k }")

    if (detailed_output == TRUE) {
      saveRDS(pred_all, fs::path(out_dir, paste0("predictions_", k)))
    }

    # calculate accuracy metrics and add other information for summary
    acc <- acc_metrics(pred_all, fuzz_matrix = fuzz_matrix, theta = theta) |>
      dplyr::mutate(
        slice = k,
        oob = oob,
        model_name = model_name
      )
  }) |> dplyr::bind_rows()

  # write out the combined acccuracy metrics for all slices
  utils::write.csv(ref_acc, fs::path(out_dir, paste0(model_name, "_acc_results.csv")))

  if(report){
    cli::cli_alert_info("generating model report")

    out_dir <- file.path(out_dir)

    # generate model accuracy report
    model_report(model_name, bec, train_data, fuzz_matrix, covars,
                 use_neighbours,extra_pts,
                 mtry, min_n, ntrees, nf_f_filter,
                 smote_ratio, downsample_ratio,
                 ref_acc,
                 out_dir)

  }

  return(out_dir)

}

