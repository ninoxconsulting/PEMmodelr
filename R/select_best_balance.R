#' Select Best accuracy metric
#'
#' @param aresults dataframe with combined outputs
#' @return dataframe of best balance options
#' @export
#'
#' @examples
#' \dontrun{
#' select_best_acc(aresults)
#'}
select_best_acc <- function(aresults){

  # get best output
  best_balance <- aresults |>
    dplyr::group_by(.data$balance) |>
    dplyr::select(.data$balance,.data$aspat_paf_theta1, .data$aspat_paf_theta.5, .data$aspat_paf_theta0,
                  .data$spat_paf_theta1, .data$spat_paf_theta.5, .data$spat_paf_theta0 ) |>
    dplyr::distinct() |>
    dplyr::summarise(dplyr::across(dplyr::where(is.numeric), mean)) |>
    dplyr::ungroup()

  # extract raw values
  raw <- best_balance |>
    dplyr::filter(.data$balance == "acc_base_model")

  # 1) calculate difference between raw and balanced for each metric
  long_raw <- raw |>
    dplyr::select(-.data$balance) |>
    tidyr::gather(key = "column", value = "value_1")

  long_bal <- best_balance |>  tidyr::gather(key = "column", value = "value_2", -.data$balance)

  best_balance_difference_all <- long_bal |>
    dplyr::inner_join(long_raw, by = "column")


  # calculate best overall metric
  best_overall <- best_balance_difference_all |>
    dplyr::mutate(dif = '-'(.data$value_2, .data$value_1))|>
    dplyr::select(.data$balance, .data$column, .data$dif) |>
    tidyr::spread(key = "column", value = "dif") |>
    dplyr::rowwise() |>
    dplyr::mutate(allsum = sum(dplyr::c_across("aspat_paf_theta.5":"spat_paf_theta1")))

  # select best overall model
  max_overall <- best_overall[which.max(best_overall$allsum),'balance']

  max_raw_overall <- best_balance|>
    dplyr::filter(.data$balance %in% c(max_overall,"acc_base_model" ))|>
    dplyr::rowwise() |>
    dplyr::mutate(allsum = sum(dplyr::c_across("aspat_paf_theta1":"spat_paf_theta0")))|>
    dplyr::select(.data$balance, .data$allsum)

  max_overall <- best_balance |>
    dplyr::filter(.data$balance %in% max_overall) |>
    dplyr::rowwise() |>
    dplyr::mutate(value_2 = (sum(dplyr::c_across("aspat_paf_theta1":"spat_paf_theta0")))/6) |>
    dplyr::select(.data$balance, .data$value_2) |>
    dplyr::mutate(column = "overall")

  raw_overall <- best_balance |>
    dplyr::filter(.data$balance == "acc_base_model") |>
    dplyr::rowwise() |>
    dplyr::mutate(value_1 = (sum(dplyr::c_across("aspat_paf_theta1":"spat_paf_theta0")))/6) |>
    dplyr::select(.data$value_1) |>
    dplyr::mutate(column = "overall")

  max_raw_overall <- dplyr::left_join(max_overall, raw_overall, by = "column")


  # 2) select best based on aspat and spat values

  best_balance_as <- best_balance  |>
    dplyr::rowwise()|>
    dplyr::mutate(aspatial_sum =(.data$aspat_paf_theta0 +  .data$aspat_paf_theta.5 +  .data$aspat_paf_theta1)/3,
                  spatial_sum = ( .data$spat_paf_theta0 +  .data$spat_paf_theta.5 +  .data$spat_paf_theta1)/3)

  # compare these to raw values (here)
  raw_best_balance <-  best_balance_as |>
    dplyr::filter(.data$balance == "acc_base_model") |>
    dplyr::select(.data$aspatial_sum, .data$spatial_sum) |>
    tidyr::gather(key = "column", value = "value_1") |>
    rbind(long_raw)


  best_balance_as <- best_balance_as |>
    tidyr::gather(key = "column", value = "value_2", -balance) |>
    dplyr::group_by(.data$column)|>
    dplyr::slice(which.max(value_2))


  # check the difference between raw and best

  best_metrics <- best_balance_as |>
    dplyr::full_join(raw_best_balance , by = "column") |>
    rbind(max_raw_overall) |>
    dplyr::rowwise() |>
    dplyr::mutate(pcdelta = round((value_2 - value_1) *100,1))|>
    dplyr::rename("maxmetric" = .data$column,
                  "max" = .data$value_2,
                  "base" = .data$value_1) |>
    dplyr::mutate(balance = stringr::str_replace_all(.data$balance, "acc_","")) #|>

  # split into easy to use columns
  best_metrics <- best_metrics |>
    dplyr::mutate(ds = dplyr::case_when(
      stringr::str_detect(.data$balance, "ds") ~ 'ds',
      .default = NA)) |>
    dplyr::mutate(sm = dplyr::case_when(
      stringr::str_detect(.data$balance, "sm") ~ 'sm',
      .default = NA)) |>
    dplyr::rowwise() |>
    dplyr::mutate(ds_ratio = ifelse(!is.na(ds), stringr::str_split(balance, "_")[[1]][2],NA),
                  sm_ratio = ifelse(!is.na(sm) & !is.na(ds), stringr::str_split(balance, "_")[[1]][4],NA))|>
    dplyr::mutate(sm_ratio = ifelse(!is.na(sm) & is.na(ds), stringr::str_split(balance, "_")[[1]][2],sm_ratio))

  return(best_metrics)

}
