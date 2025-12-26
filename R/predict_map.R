#' Generate maps from predicted model and output map or combined maps
#'
#' @param bec A character defining the MAP_LABEL zone to be modeled, i.e. "ICHmc1"
#' @param model A character filepath to the model to be used in prediction
#' @param covars A character vector of the covariates to use
#' @param cov_dir A character defining the directory of the covariates
#' @param bec_shp OPTIONAL: A sf object of the bec map, only needed for
#' model_type = "f". A sf object of the bec map
#' @param tile_dir A character defining the directory of the tiles
#' @param map_label A character defining name of output, default is "final_map.tif"
#' @param out_dir A character string with the directly where outputs will be saved.
#'
#' @returns TRUE
#' @export
#'
#' @examples
#' \dontrun{
#' run_predict_map(
#'   bec = "ICHmc1",
#'   model = fs::path(PEMprepr::read_fid()$dir_3020_draft$path_rel, "20_f",final_model_base.rds),
#'   covars = utils::read.csv(fs::path(model_dir, "reduced_covariate_list.csv")) |> dplyr::pull(),
#'   cov_dir = fs::path(PEMprepr::read_fid()$dir_1020_covariates$path_rel, "5m"),
#'   bec_shp = sf::st_read(fs::path(PEMprepr::read_fid()$dir_1010_vector$path_rel, "bec.gpkg")),
#'   tile_dir = fs::path(PEMprepr::read_fid()$dir_30_model$path_rel, "tiles"),
#'   map_label = "final_map.tif",
#'   out_dir = "temp"
#' )
#' }
predict_map <- function(
    bec = NA,
    model = NA,
    covars = NA,
    cov_dir = fs::path(PEMprepr::read_fid()$dir_1020_covariates$path_rel, "5m"),
    bec_shp = sf::st_read(fs::path(PEMprepr::read_fid()$dir_1010_vector$path_rel, "bec.gpkg")),
    tile_dir = fs::path(PEMprepr::read_fid()$dir_30_model$path_rel, "tiles"),
    map_label = "final_map.tif",
    out_dir = NA) {

   # bec
  # bec_shp
  # covars
  # cov_dir
  # tile_dir
  # out_dir
  # probability
  # map_label = "map.tif"
  # map_label = "map.tif"
  #

  # define the output dir

  if (!dir.exists(fs::path(out_dir))) {
    dir.create(fs::path(out_dir))
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
  modeli <- readRDS(fs::path(model))

  # generate a filter for the bec extent
  aoi_shp <- bec_shp[bec_shp$MAP_LABEL == bec, ]
  aoi_r <- terra::rasterize(terra::vect(aoi_shp), template)
  aoi_df <- terra::as.data.frame(aoi_r, xy = TRUE)


  # predict maps using internal function
  predict_map_internal(modeli, out_dir, tile_dir, rstack, probability = FALSE, aoi_df = aoi_df, map_label = map_label)


  cli::cli_alert_success("Maps have been generated")
  return(TRUE)
}


#
#
# # combine bgc maps for forested areas
# combine_sub_maps <- function(
#     bec_shp,
#     model_dir,
#     model_name_label) {
#   # set a list of all sub models to run (ie. BGC folders)
#   submods <- basename(fs::dir_ls(model_dir, type = "directory"))
#
#   # if "tiles exist remove this as vector
#   submods <- submods[submods != "map"]
#   submods <- submods[submods != "balance"]
#
#   ## Generate final map by joining BGC maps together
#
#   # step 1:  set up a key for the combined map (includes all the units)
#   rkey <- purrr::map(submods, function(f) {
#     keys <- utils::read.csv(fs::path(model_dir, f, "map", "response_names.csv")) |>
#       dplyr::mutate(model = f)
#   }) |> dplyr::bind_rows()
#
#   rkey <- rkey |> dplyr::mutate(map.response = seq_len(nrow(rkey)))
#
#
#   # Step 2: For each bgc, filter and mask the raster map and update key if needed
#
#   combo_map <- lapply(submods, function(f) {
#     # f <- bgcs[[2]]
#
#     rtemp <- terra::rast(file.path(model_dir, f, "map", model_name_label))
#
#     rtemp[is.na(rtemp[])] # <- 0
#     names(rtemp) <- "pred_no"
#
#     # filter to only predict over bgc
#     bec_filter <- bec_shp |> dplyr::filter(.data$MAP_LABEL == f)
#
#     rtemp <- terra::mask(rtemp, terra::vect(bec_filter))
#
#     subkey <- rkey |>
#       dplyr::filter(.data$model == f) |>
#       dplyr::mutate(mosaic = as.numeric((.data$pred_no)))
#
#     # check if the key matches or needs reclassification
#     if (isTRUE(unique(subkey$mosaic == subkey$map.response))) {
#       cli::cli_alert_info("matching key")
#     } else {
#       cli::cli_alert_info("updating key")
#
#       for (i in 1:nrow(subkey)) {
#         subkey_row <- subkey[i, ]
#
#         from <- subkey_row$pred_no
#         to <- subkey_row$map.response
#
#         rtemp <- terra::subst(rtemp, from, to)
#       }
#     }
#
#     rtemp
#   })
#
#   rsrc <- terra::sprc(combo_map)
#   m <- terra::mosaic(rsrc, fun = "max")
#
#   rkey <- rkey |> dplyr::select(.data$.pred_class, .data$pred_no, .data$model, .data$map.response)
#
#   out_folder <- fs::path(model_dir, "map")
#
#   if (!dir.exists(out_folder)) dir.create(out_folder)
#
#   terra::writeRaster(m, fs::path(out_folder, model_name_label), overwrite = TRUE)
#
#   utils::write.csv(rkey, fs::path(out_folder, "response_names.csv"))
#
#   cli::cli_alert_success("forest map merged and created and saved: {out_folder}")
# }
