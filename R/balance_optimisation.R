#' Balance Optimisation
#'
#' @param bgc_pts_subzone Datasets list with all formatted training data. Output of prep_model_tps()
#' @param bec A character with the BEC label to use. For example "ICHmc1".
#' @param fuzz_matrix data table with fuzzy metrics.
#' @param nf_f_filter A SpatRaster with binary forest (0) and nonforest (1)
#' @param covars A vector with the names of covariates to use. These match raster names.
#' @param mtry numeric. This is the output based on output of hyperparamter model tuning (default = ??)
#' @param min_n numeric. This is the output based on output of hyperparamter model tuning (default = ??)
#' @param ntrees numeric. Number of trees to use in random forest model. Default is 151.
#' @param downsample_ratio A vector of numeric downsampling values (1-10).
#' @param smote_ratio A vector of numeric downsampling values (0.1 - 0.9).
#' @param use_neighbours TRUE/FALSE. Define if you want to include all neighbours in the calculation
#' @param extra_pts logical. If TRUE, extra points will be included. Default is FALSE.
#' @param extra_pts_ratio numeric. If extra points are being used the ratio compared to the most common unit to which extra points will be added. The default is 0.1 or equivalent to 10% of most common unit
#' @param detailed_output if full output is to be produced. Default is FALSE
#' @param out_dir filepath location of output. A new folder labeled balance will be created as subfolder.
#'
#' @returns a datatable of summary of best accuracy metric
#' @export
#'
#' @examples
#' \dontrun{
#' bal_summary <- balance_optimisation(bgc_pts_subzone, bec, fuzz_matrix, nf_f_filter,
#' covars, mtry, min_n, ntrees,
#' downsample_ratio = c(1,2,3,4,5,6,7 10),
#' smote_ratio = c(0.05, 0.1),
#' use_neighbours = TRUE,
#' extra_pts = TRUE,
#' extra_pts_ratio = 0.1,
#' detailed_output = FALSE,
#' out_dir = outdir)
#' }
balance_optimisation <- function(bgc_pts_subzone, bec, fuzz_matrix, nf_f_filter,
                                 covars, mtry, min_n, ntrees,
                                 downsample_ratio = c(1, 5, 10),
                                 smote_ratio = c(0.05, 0.1),
                                 use_neighbours = TRUE,
                                 extra_pts = TRUE,
                                 extra_pts_ratio = 0.1,
                                 detailed_output = FALSE,
                                 out_dir = NA) {
  # downsample options (30, 20,10,9, 8, 7, 6, 5, - 1)
  # smote ration = 0.05, 0.1
  # extra pts = T/F
  # neighbours = T/F
  #
  # # test lines
  # bgc_pts_subzone <- bgc_pts_subzone
  # bec <- bec
  # fuzz_matrix <- fuzz_matrix
  # covars <- covars
  # mtry <- mtry
  # min_n <- min_n
  # ntrees <- 151
  # nf_f_filter <- nf_f_filter
  # detailed_output <- TRUE
  # out_dir <- out_bgc_dir
  # extra_pts <- TRUE
  # extra_pts_ratio <- 0.1
  # downsample_ratio <- c(1, 2, 3, 4, 5, 6, 7, 8, 9,10)
  # smote_ratio <- c(0.05, 0.1)
  # use_neighbours <- TRUE
  #


  # create a balance folder

  out_dir_balance <- fs::path(out_dir, "balance")

  if (!dir.exists(out_dir_balance)) {
    fs::dir_create(out_dir_balance)
  }


  # generate base results

  base_model <- run_full_model(
    model_name = "base_model", bgc_pts_subzone = bgc_pts_subzone, bec = bec,
    fuzz_matrix = fuzz_matrix, covars = covars, use_neighbours = use_neighbours,
    extra_pts = FALSE, mtry = mtry, min_n = min_n, ntrees = 151,
    downsample_ratio = FALSE, smote_ratio = FALSE, nf_f_filter = nf_f_filter,
    theta = NULL, report = FALSE, detailed_output = FALSE,
    out_dir = out_dir_balance
  )


  # generate smoting outputs (with and without extra pts)

  # smote_ratio = c(0.05, 0.1)

  smotes <- purrr::map(smote_ratio, function(s) {
    # s <- smote_ratio[1]
    # smote models with no extra
    model_name <- paste0("sm", s)

    run_full_model(
      model_name = model_name, bgc_pts_subzone = bgc_pts_subzone, bec = bec,
      fuzz_matrix = fuzz_matrix, covars = covars, use_neighbours = use_neighbours,
      extra_pts = FALSE, mtry = mtry, min_n = min_n, ntrees = 151,
      downsample_ratio = FALSE, smote_ratio = s, nf_f_filter = nf_f_filter,
      theta = NULL, report = FALSE, detailed_output = FALSE,
      out_dir = out_dir_balance
    )

    # s <- smote_ratio[1]
    model_name <- paste0("sm", s, "_extra")

    run_full_model(
      model_name = model_name, bgc_pts_subzone = bgc_pts_subzone, bec = bec,
      fuzz_matrix = fuzz_matrix, covars = covars, use_neighbours = use_neighbours,
      extra_pts = TRUE, mtry = mtry, min_n = min_n, ntrees = 151,
      downsample_ratio = FALSE, smote_ratio = s, nf_f_filter = nf_f_filter,
      theta = NULL, report = FALSE, detailed_output = FALSE,
      out_dir = out_dir_balance
    )
  })


  # generate downsamplin outputs (with and without extra pts)

  # downsample_ratio = c(1,5,10)

  ds <- purrr::map(downsample_ratio, function(d) {
    # d <- downsample_ratio[1]
    # downsample models with no extra
    model_name <- paste0("ds", d)

    run_full_model(
      model_name = model_name, bgc_pts_subzone = bgc_pts_subzone, bec = bec,
      fuzz_matrix = fuzz_matrix, covars = covars, use_neighbours = use_neighbours,
      extra_pts = FALSE, mtry = mtry, min_n = min_n, ntrees = 151,
      downsample_ratio = d, smote_ratio = FALSE, nf_f_filter = nf_f_filter,
      theta = NULL, report = FALSE, detailed_output = FALSE,
      out_dir = out_dir_balance
    )

    # downsample models with extras
    model_name <- paste0("ds", d, "_extra")

    run_full_model(
      model_name = model_name, bgc_pts_subzone = bgc_pts_subzone, bec = bec,
      fuzz_matrix = fuzz_matrix, covars = covars, use_neighbours = use_neighbours,
      extra_pts = TRUE, mtry = mtry, min_n = min_n, ntrees = 151,
      downsample_ratio = d, smote_ratio = FALSE, nf_f_filter = nf_f_filter,
      theta = NULL, report = FALSE, detailed_output = FALSE,
      out_dir = out_dir_balance
    )
  })


  # generate downsamplin outputs (with and without extra pts)
  # sm = smote_ratio
  # ds = downsample_ratio


  for (ds in downsample_ratio) {
    # ds = downsample_ratio[1]
    print(ds)

    for (sm in smote_ratio) {
      # sm = smote_ratio[2]
      print(sm)

      # set up the parameters for balancing
      # dsratio <- d # (0 - 100, Null = 1)
      # smote_ratio <- i # 0 - 1, 1 = complete smote

      model_name <- paste0("ds", ds, "_sm", sm, "_extra")
      print(model_name)

      run_full_model(
        model_name = model_name, bgc_pts_subzone = bgc_pts_subzone, bec = bec,
        fuzz_matrix = fuzz_matrix, covars = covars, use_neighbours = use_neighbours,
        extra_pts = TRUE, mtry = mtry, min_n = min_n, ntrees = 151,
        downsample_ratio = ds, smote_ratio = sm, nf_f_filter = nf_f_filter,
        theta = NULL, report = FALSE, detailed_output = FALSE,
        out_dir = out_dir_balance
      )
    }
  } # end of ds and smote loop



  # combine all metrics and select best balance optio

  alldata_list <- list.files(file.path(out_dir_balance), full.names = TRUE, pattern = "acc_", recursive = TRUE)
  # remove files with no information
  data_list <- alldata_list[file.info(alldata_list)$size > 10]

  aresults <- purrr::map(data_list, function(k) {
    temp <- utils::read.csv(k)
    temp <- temp |> dplyr::mutate(filename = paste(basename(k)))
    temp
  }) |> dplyr::bind_rows()

  aresults <- aresults |> dplyr::mutate(balance = gsub(".csv", "", .data$filename))


  # summaries into a single table with average plus unit accuracy metrics

  # generate the average metrics
  best_metrics <- aresults |>
    dplyr::group_by(.data$balance) |>
    dplyr::select(
      .data$balance,
      .data$spat_pa_theta_0, .data$spat_pa_theta_0.5, .data$spat_pa_theta_1,
      .data$aspat_pa_theta_0, .data$aspat_pa_theta_0.5, .data$aspat_pa_theta_1
    ) |>
    dplyr::distinct() |>
    dplyr::summarise(dplyr::across(dplyr::where(is.numeric), mean)) |>
    dplyr::ungroup() |>
    dplyr::rowwise() |>
    dplyr::mutate(sum_acc = sum(dplyr::c_across(2:7), na.rm = T)) |>
    dplyr::mutate(ave_acc = .data$sum_acc / 6)


  # generate the average result overall slices per mapunit and calculate the
  # number of units which are above 65%

  best_units <- aresults |>
    dplyr::select(.data$balance, .data$mapunit1, .data$aspat_pa, .data$aspat_paf) |>
    dplyr::group_by(.data$balance, .data$mapunit1) |>
    dplyr::summarize(
      unit.acc.pa = mean(.data$aspat_pa, na.rm = TRUE),
      unit.acc.paf = mean(.data$aspat_paf, na.rm = TRUE),
      no.classes = dplyr::n()
    )

  unit.acc <- best_units |>
    dplyr::mutate(
      good_units_pa = sum(.data$unit.acc.pa > 0.65, na.rm = TRUE),
      good_units_paf = sum(.data$unit.acc.paf > 0.65, na.rm = TRUE)
    ) |>
    dplyr::group_by(.data$balance) |>
    dplyr::select(c(-.data$mapunit1, -.data$unit.acc.pa, -.data$unit.acc.paf)) |>
    dplyr::summarise(dplyr::across(dplyr::where(is.numeric), sum))

  best_metrics <- dplyr::left_join(best_metrics, unit.acc)

  utils::write.csv(best_metrics, file.path(out_dir_balance, "best_balancing.csv"))

  return(best_metrics)
}
