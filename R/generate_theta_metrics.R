#' Generate theta metrics for given predictions
#'
#' @param datafolder a character or filepath to the
#' @param fmat a dataframe of the fuzzy matrix values
#'
#' @returns a dataframe with accuracy measures
#' @export
#'
#' @examples
#' \dontrun{
#' generate_theta_metrics(datafolder, fmat)
#' }
generate_theta_metrics = function(datafolder, fmat) {

  #datafolder = i

  slices <- as.factor(list.files(datafolder, pattern = "prediction_*"))

  if("compiled_theta_results.csv" %in% slices){
    cli::cli_alert_warning("compiled theta file already exists, this file will be overwriten")
    slices = slices[-1] |>
      droplevels()
  }

  theta_acc <- purrr::map(levels(slices), function(k){
    #k = levels(slices)[1]
    cli::cli_alert_info(paste0("Calculating theta metrics for ", k))

    pred_all <- readRDS(file.path(datafolder, k))
    theta_vals <- as.factor(c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))
    # note if you add 1 and 0 this will interfere with the generate theta threshold calculations

    allthetas <- purrr::map(levels(theta_vals), function(th){

      tacc <- acc_metrics(pred_all, fuzzmatrx = fmat, theta = as.numeric(th))  |>
        dplyr::mutate(theta = th)

    })|> dplyr::bind_rows()

    allthetas <- allthetas |>  dplyr::mutate(slice = k)

  }) |> dplyr::bind_rows()

  return(theta_acc)
}



# select theta threshold

select_theta_threshold <- function(allthetas){

  #allthetas = acc_out

  metrics = as.factor(c("p", "pa", "paf"))

  theta_thresh <- purrr::map(levels(metrics), function(k){
    #k = levels(metrics)[1]
    mnames = paste0(k,"_theta")
    noi <-  names(allthetas)[stringr::str_detect(names(allthetas),mnames)]

    acc <- allthetas |>
      dplyr::select(.data$slice, .data$theta, dplyr::any_of(noi)) |>
      dplyr::distinct()

    acc2 <- acc  |>
      tidyr::pivot_longer(cols = dplyr::where(is.numeric), names_to = "accuracy_type", values_to = "value") |>
      dplyr::distinct()

    acc <- acc2 |>
      dplyr::mutate(type = dplyr::case_when(
        stringr::str_detect(.data$accuracy_type, "aspat") ~ "aspatial",
        stringr::str_detect(.data$accuracy_type, "spat") ~ "spatial"))|>
      dplyr::mutate(theta_base = dplyr::case_when(
        stringr::str_detect(.data$accuracy_type, "theta0") ~ 0,
        stringr::str_detect(.data$accuracy_type, "theta.5") ~ NA,
        stringr::str_detect(.data$accuracy_type, "theta1") ~ 1)) |>
      dplyr::mutate(theta_final = ifelse(is.na(.data$theta_base), .data$theta, .data$theta_base))|>
      dplyr::select(-.data$theta_base)

    bal_out <- acc |>
      dplyr::summarise(mean = mean(.data$value),
                       q25 = stats::quantile(.data$value, probs = 0.25),
                       q75 = stats::quantile(.data$value, probs = 0.75),
                       .by = c(.data$type, .data$theta_final))|>
      dplyr::mutate(above_thresh = ifelse(.data$q25 <= 0.65, F, T))

    # #library(ggplot2)
    # overall_acc <- ggplot(aes(y = value, x = theta_final), data = acc ) +
    #   geom_boxplot() +
    #   scale_fill_brewer(type = "qual") +
    #   facet_wrap(~type, scales = "free_x")+
    #   geom_hline(yintercept = 0.65,linetype ="dashed", color = "black") +
    #   #theme_pem_facet() +
    #   # scale_fill_manual(values=c("grey90", "grey75", "grey50", "grey35","grey10"))+
    #   theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="none") +
    #   xlab("Metric") + ylab("Accuracy") +
    #   ylim(0, 1)
    #
    # overall_acc

    bal_out <- bal_out |>  dplyr::mutate(accuracy_type = k)

  }) |> dplyr::bind_rows()

  return(theta_thresh)
}


run_theta_metrics <- function(bgc_pts_subzone, out_dir, fmat, overwrite = FALSE) {

  bgcs <- names(bgc_pts_subzone)

  for (i in bgcs) {
    # i = bgcs[3]
    cli::cli_alert_info(paste("Running theta metrics for", i))

    datafolder <- fs::path(out_dir, i)
    acc_out <- generate_theta_metrics(datafolder, fmat)
    theta_thresh <- select_theta_threshold(acc_out)

    # if file exists and overwrite is set to TRUE delete the file
    if (fs::file_exists(fs::path(datafolder, "compiled_theta_results.csv")) & overwrite) {
      fs::file_delete(fs::path(datafolder, "compiled_theta_results.csv"))
    }

    utils::write.csv(acc_out, fs::path(datafolder, "compiled_theta_results.csv"))
    utils::write.csv(theta_thresh, file.path(datafolder, "theta_threshold.csv"))
  }

  cli::cli_alert_success("Theta metrics complete")

  return(invisible(out_dir))
}
