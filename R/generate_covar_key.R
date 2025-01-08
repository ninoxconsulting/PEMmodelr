
#' Generate a covariate key
#'
#' This function reviews covariates in the attribute points and generates a key to be used in
#' the next steps of modelling.
#' @param att_pts A `sf` object with cleaned and attribute points. This is the output of `PEMsamplr::attribute_pts()`
#' @param overwrite Logical TRUE or FALSE if any existing file is to overwritten. Default is FALSE
#' @param out_dir A character string of path which points to output location. A default
#'    location and name are applied in line with standard workflow.
#' @param out_name A character string of the output file name. Default is "covar_key.csv"
#'
#' @returns a csv file with two columns. Value is the name of each covariate and type is the type of covariate
#' @export
#'
#' @examples
#' \dontrun{
#' covkey <- generate_covar_key(att_pts,overwrite = FALSE,
#' out_dir = PEMprepr::read_fid()$dir_30_model$path_rel, out_name = "covar_key.csv")
#' }
generate_covar_key <- function(
    att_pts,
    overwrite = FALSE,
    out_dir = PEMprepr:::read_fid()$dir_30_model$path_rel,
    out_name = "covar_key.csv") {
  # testing
  #  overwrite = TRUE
  ##  out_dir = PEMprepr::read_fid()$dir_30_model$path_rel
  #  out_name = "covar_key.csv"
  # end testing


  # check if input is an sf object
  if (!inherits(att_pts, "sf")) {
    cli::cli_abort("att_pts must be an sf object")
    return()
  }


  if (!overwrite & fs::file_exists(fs::path(out_dir, out_name))) {
    cli::cli_abort("covariate key file already exists, set overwrite = TRUE to overwrite")
    return()
  }


  # define the names of columns by type
  core_names <- c(
    "id", "fnf", "x", "y", "bgc_cat", "data_type",
    "mapunit1", "mapunit2", "position", "transect_id", "tid",
    "slice", "geom", "geometry"
  )

  extra_names <- c(
    "order", "point_type", "observer", "transition", "struc_stage",
    "struc_mod", "date_ymd", "time_hms", "edatope", "comments", "photos", "ID", "lyr.1"
  )

  dem_names <- c(
    "convergence", "dah", "dem_preproc", "flow_accum_ft",
    "flow_accum_p", "flow_accum_td",
    "flowpathlentd", "max_fp_l", "max_fp_l2",
    "max_fp_l3", "ls_factor", "mrn",
    "mrn_area", "mrn_mheight", "mrrtf",
    "mrvbf", "mrrtf2", "mrvbf2",
    "open_neg", "open_pos", "hdist",
    "vdist", "hdistnob", "vdistnob",
    "rid_level", "val_depth", "scatchment",
    "sinkroute", "sinksfilled", "aspect",
    "gencurve", "slope", "totcurve",
    "slength", "flowlength1", "tca1", "twi",
    "tcatchment", "tpi", "tri", "elevation", "channelsnetwork", "flowpathlenTD"
  )

  structure_name <- c(
    "p10_mosaic_rproj", "p20_mosaic_rproj",
    "p25_mosaic_rproj", "p50_mosaic_rproj",
    "p75_mosaic_rproj", "p80_mosaic_rproj",
    "p85_mosaic_rproj", "p90_mosaic_rproj", "p95_mosaic_rproj",
    "p05_mosaic_rproj", "p15_mosaic_rproj", "p30_mosaic_rproj",
    "cov_gap_mosaic_rproj", "dns_gap_mosaic_rproj",
    "vc3_mosaic", "p98_mosaic_rproj"
  )


  sat_name <- c("red", "green", "blue", "nir", "swir1", "swir2")


  # add covariates that will be generated in the next steps
  covars <- tibble::as_tibble(c(
    names(att_pts), "id", "fnf", "x", "y", "bgc_cat",
    "position", "geom"
  ))

  names(covars) <- "value"
  covars$type <- NULL

  covars <- covars |>
    dplyr::mutate(type = dplyr::case_when(
      value %in% core_names ~ "core",
      value %in% extra_names ~ "extra",
      value %in% dem_names ~ "dem",
      value %in% structure_name ~ "structure",
      value %in% sat_name ~ "satellite",
    ))

  if (anyNA(unique(covars$type))) {
    cli::cli_alert_warning("Some covariates are missing covariate type, pleae review and edit output file: {.path { out_dir}/{ out_name}} before proceeding with modelling")
  }

  covars <- utils::write.csv(covars, fs::path(out_dir, out_name), row.names = FALSE)

  return(covars)
}
