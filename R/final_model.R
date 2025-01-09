#' final model fit
#'
#' @param train_data A list of prepped data. Output of `prep_model_tps()`
#' @param mtry numeric. This is the output based on output of hyperparamter model tuning (default = 14)
#' @param min_n numeric. This is the output based on output of hyperparamter model tuning (default = 7)
#' @param ds_ratio numeric
#' @param sm_ratio numeric
#'
#' @returns a parsnip model object
#' @export
#'
#' @examples
#' \dontrun{
#' final_model(train_data, mtry, min_n, ds_ratio = NA, sm_ratio = NA)
#' }
final_model <- function(train_data, mtry, min_n, ds_ratio = NA, sm_ratio = NA){

  # prep data
  ref_dat <- train_data |>
    dplyr::mutate(mapunit1 = as.factor(.data$mapunit1))

  MU_count <- ref_dat |> dplyr::count(.data$mapunit1) |> dplyr::filter(.data$n > 10)

  ref_dat <- ref_dat |> dplyr::filter(.data$mapunit1 %in% MU_count$mapunit1)  |>
    droplevels()

  munits <- unique(ref_dat$mapunit1)
  # nf_mapunits <- grep(munits, pattern = "_\\d", value = TRUE, invert = TRUE)

  ref_dat <- ref_dat[stats::complete.cases(ref_dat[, 2:length(ref_dat)]), ]

  #set up model params
  randf_spec <- parsnip::rand_forest(mtry = mtry, min_n = min_n, trees = 200) |>
    parsnip::set_mode("classification") |>
    parsnip::set_engine("ranger", importance = "permutation", verbose = FALSE)


  # set up downsample and smote options

  if(is.na(ds_ratio) & is.na(sm_ratio)){

    print("no downsample or smoting")

    best_recipe <-  recipes::recipe(.data$mapunit1 ~ ., data = ref_dat)

  }
  if(is.na(ds_ratio) & !is.na(sm_ratio)){

    print("applying smoting")

    best_recipe <-  recipes::recipe(.data$mapunit1 ~ ., data = ref_dat) |>
      themis::step_smote(.data$mapunit1, over_ratio = sm_ratio , neighbors = 5, skip = TRUE)

  }

  if(!is.na(ds_ratio) & is.na(sm_ratio)){

    print("applying downsample")

    best_recipe <-  recipes::recipe(.data$mapunit1 ~ ., data = ref_dat) |>
      themis::step_downsample(.data$mapunit1, under_ratio = ds_ratio)

  }
  if(!is.na(ds_ratio) & !is.na(sm_ratio)){

    print("applying downsample and smoting")

    best_recipe <-  recipes::recipe(.data$mapunit1 ~ ., data = ref_dat) |>
      themis::step_downsample(.data$mapunit1, under_ratio = ds_ratio) |>
      themis::step_smote(.data$mapunit1, over_ratio = sm_ratio , neighbors = 5, skip = TRUE)

  }

  # set up workflow

  pem_workflow <- workflows::workflow() |>
    workflows::add_recipe(best_recipe) |>
    workflows::add_model(randf_spec)

  # run model
  print("running final PEM model")

  PEM_rf <- parsnip::fit(pem_workflow, ref_dat)

  return(PEM_rf)

}
