#' Balance_optimisation_iteration
#'
#' @param train_data training data set
#' @param ds_iterations downsampling values (vector of numbers:  10 - 100), NA if not using
#' @param smote_iterations smote iterations (vector of numbers: 0.1 - 0.9), NA if not using
#' @param use_neighbours to use all spatial adjoining values, default is FALSE
#' @param fuzz_matrix fuzzy matrix
#' @param mtry mtry from best params
#' @param min_n mtry from best params
#' @param out_dir location where the balance output files will be stored
#' @return outputs files directly to folder
#' @export
#'
#' @examples
#' \dontrun{
#' balance_optimisation_iteration(train_data,
#'   ds_iterations = c(30, 40, 50),
#'   smote_iterations = c(0.5, 0.6, 0.7), use_neighbours = FALSE, fuzz_matrix = fmat,
#'   out_dir = out_dir
#' )
#' }
balance_optimisation <- function(train_data = train_data,
                                 ds_iterations = ds_iterations,
                                 smote_iterations = smote_iterations,
                                 mtry = mtry,
                                 min_n = min_n,
                                 fuzz_matrix = fuzz_matrix,
                                 out_dir = out_dir,
                                 use_neighbours = FALSE) {
  # # create a subfolder to store all balance outputs:

  out_folder <- fs::path(out_dir, "balance")
  if (!dir.exists(out_folder)) {
    dir.create(out_folder)
  }

  # run base model

  print("base model")

  base_acc <- base_model(train_data, fuzz_matrix,
    mtry = mtry, min_n = min_n,
    use_neighbours = FALSE,
    detailed_output = FALSE
  )


  utils::write.csv(base_acc, file = fs::path(out_folder, "acc_base_model.csv"))


  # downsample and smote options

  if (unique(!is.na(ds_iterations) & !is.na(smote_iterations))) {
    print("downsample and smote")

    for (d in ds_iterations) {
      # d = ds_iterations[1]
      print(d)

      for (i in smote_iterations) {
        # i = smote_iterations[2]
        print(i)

        # set up the parameters for balancing
        downsample_ratio <- d # (0 - 100, Null = 1)
        smote_ratio <- i # 0 - 1, 1 = complete smote

        balance_name <- paste0("ds_", downsample_ratio, "_sm_", smote_ratio)

        # training set - train only on pure calls
        ref_dat <- train_data |>
          dplyr::mutate(
            mapunit1 = as.factor(.data$mapunit1),
            slice = as.factor(.data$slice)
          ) |>
          dplyr::select(-.data$tid)

        cli::cli_alert_info("Training raw data models...")

        munits <- unique(ref_dat$mapunit1)

        # place holder to catch non-forest within a forest model

        if ("forest" %in% munits) {
          nf_mapunits <- NA
        } else {
          nf_mapunits <- grep(munits, pattern = "_\\d", value = TRUE, invert = TRUE)
        }

        slices <- unique(ref_dat$slice) |> droplevels()

        # for all slices
        ref_acc <- purrr::map(levels(slices), function(k) {
          # k = levels(slices)[2]
          label <- paste(d, i, k, sep = "-")
          print(label)
          # print(k)

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
            # recipes::update_role(tid, new_role = "id variable") |>
            themis::step_downsample(mapunit1, under_ratio = downsample_ratio) |>
            themis::step_smote(mapunit1, over_ratio = smote_ratio, neighbors = 4, skip = TRUE)

          randf_spec <- parsnip::rand_forest(mtry = mtry, min_n = min_n, trees = 151) |>
            parsnip::set_mode("classification") |>
            parsnip::set_engine("ranger", importance = "permutation", verbose = FALSE)

          pem_workflow <- workflows::workflow() |>
            workflows::add_recipe(null_recipe) |>
            workflows::add_model(randf_spec)

          #######################################################
          possibleError <- tryCatch(
            parsnip::fit(pem_workflow, ref_train),
            error = function(e) e
          )
          if (!inherits(possibleError, "error")) {
            ref_mod <- possibleError

            oob <- round(ref_mod$fit$fit$fit$prediction.error, 3)

            # ref_mod <- fit(pem_workflow, ref_train)

            preds <- terra::predict(ref_mod, ref_test)

            pred_all <- cbind(ref_id, .pred_class = preds$.pred_class)

            pred_all <- .prep_model_output(pred_all, nf_mapunits)

            cli::cli_alert_info("generating accuracy metrics for slice: {k}")

            acc <- acc_metrics(pred_all, fuzz_matrix = fuzz_matrix) |>
              dplyr::mutate(
                slice = k,
                oob = oob
              )
          }
        }) |> dplyr::bind_rows() # end of slice loop (downsample only )

        utils::write.csv(ref_acc, file = fs::path(out_folder, paste0("acc_", balance_name, ".csv")))
      } # end of smote iteration
    } # end of downsample iteration
  }

  if (unique(!is.na(ds_iterations))) {
    print("downsample only")

    for (d in ds_iterations) {
      # d = ds_iterations[1]
      print(d)
      # set up the parameters for balancing
      downsample_ratio <- d # (0 - 100, Null = 1)
      balance_name <- paste0("ds_", downsample_ratio)

      # training set - train only on pure calls
      ref_dat <- train_data |>
        dplyr::mutate(
          mapunit1 = as.factor(.data$mapunit1),
          slice = as.factor(.data$slice)
        ) |>
        dplyr::select(-.data$tid)

      cli::cli_alert_info("Training raw data models...")

      munits <- unique(ref_dat$mapunit1)

      # place holder to catch non-forest within a forest model
      if ("forest" %in% munits) {
        nf_mapunits <- NA
      } else {
        nf_mapunits <- grep(munits, pattern = "_\\d", value = TRUE, invert = TRUE)
      }

      slices <- unique(ref_dat$slice) |> droplevels()

      ref_acc <- purrr::map(levels(slices), function(k) {
        # k = levels(slices)[1]
        label <- paste(d, k, sep = "-")
        print(label)

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
          # recipes::update_role(tid, new_role = "id variable") |>
          themis::step_downsample(mapunit1, under_ratio = downsample_ratio)

        randf_spec <- parsnip::rand_forest(mtry = mtry, min_n = min_n, trees = 151) |>
          parsnip::set_mode("classification") |>
          parsnip::set_engine("ranger", importance = "permutation", verbose = FALSE)

        pem_workflow <- workflows::workflow() |>
          workflows::add_recipe(null_recipe) |>
          workflows::add_model(randf_spec)

        #######################################################
        possibleError <- tryCatch(
          parsnip::fit(pem_workflow, ref_train),
          error = function(e) e
        )
        if (!inherits(possibleError, "error")) {
          ref_mod <- parsnip::fit(pem_workflow, ref_train)
          oob <- round(ref_mod$fit$fit$fit$prediction.error, 3)

          preds <- terra::predict(ref_mod, ref_test)

          pred_all <- cbind(ref_id, .pred_class = preds$.pred_class)

          pred_all <- .prep_model_output(pred_all, nf_mapunits)

          cli::cli_alert_info("generating accuracy metrics for slice:{ k }")

          acc <- acc_metrics(pred_all, fuzz_matrix = fuzz_matrix) |>
            dplyr::mutate(
              slice = k,
              oob = oob
            )
        } # end of error checking loop
      }) |> dplyr::bind_rows() # end of slice loop (downsample only)

      # extract results from sresults

      utils::write.csv(ref_acc, file = fs::path(out_folder, paste0("acc_", balance_name, ".csv")))
    } # end of downsample iteration only loop
  }

  if (unique(!is.na(smote_iterations))) {
    # smote only
    print("smote only")

    for (i in smote_iterations) {
      # i = smote_iterations[1]
      print(i)

      # set up the parameters for balancing
      smote_ratio <- i # 0 - 1, 1 = complete smote
      balance_name <- paste0("sm_", smote_ratio)

      # training set - train only on pure calls
      ref_dat <- train_data |>
        dplyr::mutate(
          mapunit1 = as.factor(.data$mapunit1),
          slice = as.factor(.data$slice)
        ) |>
        dplyr::select(-.data$tid)

      cli::cli_alert_info("Training raw data models...")

      munits <- unique(ref_dat$mapunit1)

      if ("forest" %in% munits) {
        nf_mapunits <- NA
      } else {
        nf_mapunits <- grep(munits, pattern = "_\\d", value = TRUE, invert = TRUE)
      }

      slices <- unique(ref_dat$slice) |> droplevels()

      # for all slices
      ref_acc <- purrr::map(levels(slices), function(k) {
        # k = levels(slices)[2]
        label <- paste(i, k, sep = "-")
        print(label)

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
          themis::step_smote(mapunit1, over_ratio = smote_ratio, neighbors = 4, skip = TRUE)

        randf_spec <- parsnip::rand_forest(mtry = mtry, min_n = min_n, trees = 151) |>
          parsnip::set_mode("classification") |>
          parsnip::set_engine("ranger", importance = "permutation", verbose = FALSE)

        pem_workflow <- workflows::workflow() |>
          workflows::add_recipe(null_recipe) |>
          workflows::add_model(randf_spec)

        #######################################################
        possibleError <- tryCatch(
          parsnip::fit(pem_workflow, ref_train),
          error = function(e) e
        )
        if (!inherits(possibleError, "error")) {
          ref_mod <- possibleError
          oob <- round(ref_mod$fit$fit$fit$prediction.error, 3)
          preds <- terra::predict(ref_mod, ref_test)

          pred_all <- cbind(ref_id, .pred_class = preds$.pred_class)

          pred_all <- .prep_model_output(pred_all, nf_mapunits)

          cli::cli_alert_info("generating accuracy metrics for slice:{ k }")

          acc <- acc_metrics(pred_all, fuzz_matrix = fuzz_matrix) |>
            dplyr::mutate(
              slice = k,
              oob = oob
            )
        }
      }) |> dplyr::bind_rows() # end of slice loop (downsample only )

      utils::write.csv(ref_acc, file = fs::path(out_folder, paste0("acc_", balance_name, ".csv")))
    } # end of smote iteration
  }

  return(TRUE)
} # end of function
