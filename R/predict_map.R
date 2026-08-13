#' Generate maps from predicted model and output map or combined maps
#'
#' @param model A fitted model object
#' @param bec A character defining the MAP_LABEL zone to be modeled, i.e. "ICHmc1"
#' @param covars A character vector of the covariates to use
#' @param cov_dir A character defining the directory of the covariates
#' @param bec_shp OPTIONAL: A sf object of the bec map, only needed for
#' model_type = "f". A sf object of the bec map
#' @param tile_dir A character defining the directory of the tiles
#' @param map_label A character defining name of output, default is "final_map.tif"
#' @param out_dir A character string with the directly where outputs will be saved.
#' @param probability A logical value indicating whether to output probability rasters. Default is FALSE.
#'
#' @returns TRUE
#' @export
#'
#' @examples
#' \dontrun{
#' predict_map(rf_fit, out_dir, tile_dir, rstack, probability = FALSE)
#'}
predict_map <- function(model = model,
                        bec = bec,
                        bec_shp = bec_shp,
                        covars = covars,
                        cov_dir = cov_dir,
                        tile_dir = tile_dir,
                        out_dir = out_dir,
                        map_label = map_label,
                        probability = FALSE){


  # create a map dir if not already existing
  if (!dir.exists(file.path(out_dir))) {
    dir.create(file.path(out_dir))
  }

  # write out best class
  if (!dir.exists(file.path(out_dir, "best"))) {
    dir.create(file.path(out_dir, "best"))
  }

  # set up raster stack
  rast_list <- list.files(cov_dir, pattern = ".sdat$|.tif$", recursive = T, full.names = T)
  rast_list <- rast_list[tolower(gsub(".sdat$|.tif$", "", basename(rast_list))) %in% (covars)]
  rstack <- terra::rast(rast_list)

  # check if any covars have NA values- these will not be predicted in the final map
  na_list <- terra::global(rstack, "anyNA")

  na <- tibble::rownames_to_column(na_list) |>
    dplyr::filter(anyNA == TRUE)
  nas <- na$rowname

  if (length(na$rowname > 0)) {
    cli::cli_alert_warning("The following covariates have NA values and will contain gaps when generating predictions, please check the raw data: { nas }")
  }

  # if template file exists use this otherwise chose first .tif and assign value of 0
  if (fs::file_exists(fs::path(cov_dir, "template.tif"))) {
    template <- terra::rast(fs::path(cov_dir, "template.tif"))
  } else {
    template <- terra::rast(rast_list[1])
    template[] <- 0
  }

  # generate tiles for mapping
  tiles <- get_tiles(tile_dir, template, 500)

  cli::cli_alert_info("Predicting tiles")

  # read in model item
  modeli <- model

  # generate a filter for the bec extent
  aoi_shp <- bec_shp[bec_shp$MAP_LABEL == bec, ]
  aoi_r <- terra::rasterize(terra::vect(aoi_shp), template)
  aoi_df <- terra::as.data.frame(aoi_r, xy = TRUE)

  # predict maps using internal function
  predict_map_internal(modeli, out_dir, tile_dir, rstack, probability = FALSE, aoi_df = aoi_df, map_label = map_label)


  cli::cli_alert_success("Maps have been generated")

  return(TRUE)

}
