#' Find optimal tuning for rF model
#'
#' Runs a tuneRF function on all training data to determine optimal mtry and min_n.
#' Saves these hyperparameters for use in model building.
#'
#' @param tran_dat is the transect training points in table format
#' @export
#' @examples
#' \dontrun{
#' tune_rf(tpt, reduced_covs)
#' }
tune_rf <- function(tran_dat) {
  trees_split <- rsample::initial_split(tran_dat, strata = .data$slice, prop = 1 / 10)
  trees_train <- rsample::training(trees_split)
  trees_test <- rsample::testing(trees_split)

  tune_spec <- parsnip::rand_forest(
    mtry = parsnip::tune(),
    trees = 10,
    min_n = parsnip::tune()
  ) |>
    parsnip::set_mode("classification") |>
    parsnip::set_engine("ranger")

  best_recipe <- recipes::recipe(mapunit1 ~ ., data = trees_train) |>
    recipes::update_role(slice, new_role = "id variable")

  tune_wf <- workflows::workflow() |>
    workflows::add_recipe(best_recipe) |>
    workflows::add_model(tune_spec)

  trees_folds <- rsample::vfold_cv(trees_train, v = 10, repeats = 1, strata = .data$slice)

  doParallel::registerDoParallel()

  tune_res <- tune::tune_grid(
    tune_wf,
    resamples = trees_folds,
    grid = 10
  )

  return(tune_res)
}
