#' Prepare prepped training points for model runs
#'
#' @param prepped_points A csv object with cleaned and attribute points.
#' This is the output following `prep_tps()` function.
#' @param covars A character string with the names of the covariates to be used in the model.
#' @param model_type A character string to define the model type.
#' Options are "fnf" (forest-nonforest), "f" (forest) or "nf" (nonoforest)
#' @param out_dir A character string of the path to the output directory.
#' @param outname A character string of the output file name. Default is "model_input_pts.rds"
#'
#' @returns an rds object with list of cleaned prepped points for each subcatergory (bgc or model type)
#' @export
#'
#' @examples
#' \dontrun{
#' model_bgc <- prep_model_tps_bgc(
#'   prepped_points = tpts,
#'   covars = reduced_vars, model_type = "f", out_dir = out_dir, outname = "model_input_pts.rds"
#' )
#' }
prep_model_tps <- function(
    prepped_points = NULL,
    covars = NULL,
    model_type = "fnf", # fnf or f or nf
    out_dir = NULL,
    outname = "model_input_pts.rds") {
  # check inputs

  # check if model type is within the available options
  if (!model_type %in% c("fnf", "f", "nf")) {
    cli::cli_abort("model_type must be one of 'fnf', 'f' or 'nf'")
  }

  # check if prepped_points is a csv object
  if (!is.data.frame(prepped_points)) {
    cli::cli_abort("prepped_points must be a csv object")
  }

  # check if covars is a character string
  if (!is.character(covars)) {
    cli::cli_abort("covars must be a character string")
  }


  # if non bgc model then save as single list

  if (model_type %in% c("fnf", "nf")) {
    if (!dir.exists(out_dir)) {
      fs::dir_create(out_dir)
    }

    bgc_pts_subzone <- lapply(model_type, function(i) {
      tdat <- prepped_points |>
        dplyr::mutate(slice = factor(.data$slice)) |>
        dplyr::select(
          .data$id, .data$mapunit1, .data$mapunit2, .data$position, .data$transect_id,
          .data$tid, .data$slice, .data$bgc_cat, .data$fnf, .data$X, .data$Y, .data$data_type, dplyr::any_of(covars)
        )

      tdat
    })

    # format names
    names(bgc_pts_subzone) <- model_type

    saveRDS(bgc_pts_subzone, fs::path(out_dir, outname))
  }

  # if bgc model then save as list of subzones

  if (model_type == "f") {
    zones <- c(as.character(unique(prepped_points$bgc_cat)))
    zones <- zones[!is.na(zones)]

    bgc_pts_subzone <- lapply(zones, function(i) {
      # i =  zones[2]

      out_bgc_dir <- fs::path(out_dir, i)

      if (!dir.exists(out_bgc_dir)) {
        fs::dir_create(out_bgc_dir)
      }

      # remove any bec sites series that are not in the bec_zone catergory
      pts_subzone <- prepped_points |>
        dplyr::mutate(keep = dplyr::case_when(
          stringr::str_detect(.data$tid, as.character(paste0(tolower(i), "_")))  ~ TRUE,
          stringr::str_detect(tolower(.data$mapunit1), as.character(paste0(tolower(i), "_")))  ~ TRUE,
          TRUE ~ FALSE
        )) |>
        dplyr::filter(keep == TRUE) |>
        dplyr::select(-keep) |>
        droplevels()


      # remove any bec sites series that are not in the bec_zone catergory
      #TODO this may create issue if there is a unit in incidentals and these are not added to model?

      munits <- grep(unique(pts_subzone$mapunit1), pattern = "_\\d", value = TRUE, invert = FALSE)

      diff_bec_mapunits <- grep(munits, pattern = paste0("^", i, "_"), value = TRUE, invert = TRUE)

      pts_subzone <- pts_subzone |>
        dplyr::filter(!.data$mapunit1 %in% diff_bec_mapunits)

      if (nrow(pts_subzone) == 0) {
        pts_subzone <- NULL
      } else {
        ppts_subzone <- pts_subzone
      }

      tdat <- pts_subzone |> dplyr::mutate(slice = factor(.data$slice))

      tdat <- tdat |>
        dplyr::select(
          .data$id, .data$mapunit1, .data$mapunit2, .data$position, .data$transect_id,
          .data$tid, .data$slice, .data$bgc_cat, .data$fnf, .data$X, .data$Y,
          .data$data_type, dplyr::any_of(covars)
        )

      tdat
    })

    names(bgc_pts_subzone) <- zones

    saveRDS(bgc_pts_subzone, fs::path(out_dir, outname))
  }

  return(bgc_pts_subzone)
}
