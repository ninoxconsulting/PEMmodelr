#' Generate maps from predicted model and output map or combined maps
#'
#' @param model_type A character defining the type of model to run (f = forested)
#' @param model_dir A character defining the directory of the model
#' @param model_name A character defining the name of the model. default = "final_model_base.rds"
#' @param covars A character vector of the covariates to use
#' @param cov_dir A character defining the directory of the covariates
#' @param tile_dir A character defining the directory of the tiles
#' @param bec_shp OPTIONAL: A sf object of the bec map, only needed for
#' model_type = "f". A sf object of the bec map
#'
#' @returns TRUE
#' @export
#'
#' @examples
#' \dontrun{
#' run_predict_map(
#'   model_type = "f",
#'   model_dir = fs::path(PEMprepr::read_fid()$dir_3020_draft$path_rel, "20_f"),
#'   model_name = "final_model_base.rds",
#'   covars = utils::read.csv(fs::path(model_dir, "reduced_covariate_list.csv")) |> dplyr::pull(),
#'   cov_dir = fs::path(PEMprepr::read_fid()$dir_1020_covariates$path_rel, "5m"),
#'   tile_dir = fs::path(PEMprepr::read_fid()$dir_30_model$path_rel, "tiles"),
#'   bec_shp = sf::st_read(fs::path(PEMprepr::read_fid()$dir_1010_vector$path_rel, "bec.gpkg"))
#' )
#' }
run_predict_map <- function(
    model_type = NA,
    model_dir = fs::path(PEMprepr::read_fid()$dir_3020_draft$path_rel, "20_f"),
    model_name = "final_model_base.rds",
    covars = utils::read.csv(fs::path(model_dir, "reduced_covariate_list.csv")) |> dplyr::pull(),
    cov_dir = fs::path(PEMprepr::read_fid()$dir_1020_covariates$path_rel, "5m"),
    tile_dir = fs::path(PEMprepr::read_fid()$dir_30_model$path_rel, "tiles"),
    bec_shp = sf::st_read(fs::path(PEMprepr::read_fid()$dir_1010_vector$path_rel, "bec.gpkg"), quiet = TRUE)) {
  # set a list of all sub models to run (ie. BGC folders)
  submods <- basename(fs::dir_ls(model_dir, type = "directory"))

  # if "tiles exist remove this as vector
  submods <- submods[submods != "map"]
  submods <- submods[submods != "balance" ]
  model_name_label = gsub(".rds", ".tif", model_name)

  # set up raster stack
  rast_list <- list.files(cov_dir, pattern = ".sdat$|.tif$", recursive = T, full.names = T)
  rast_list <- rast_list[tolower(gsub(".sdat$|.tif$", "", basename(rast_list))) %in% (covars)]
  rstack <- terra::rast(rast_list)

  # if template file exists use this otherwise chose first .tif and assign value of 0
  if (fs::file_exists(fs::path(cov_dir, "template.tif"))) {
    template <- terra::rast(fs::path(cov_dir, "template.tif"))
  } else {
    template <- terra::rast(rast_list[1])
    template[] <- 0
  }

  # generate tiles for mapping
  tiles <- get_tiles(tile_dir, template, 500)

  map_bgc <- purrr::map(submods, function(b) {
    cli::cli_alert_info("Predicting {b} maps")

    mfit <- fs::dir_ls(file.path(model_dir, b), type = "file", recurse = TRUE, regexp = paste0(model_name, "$"))
    model <- readRDS(fs::path(mfit))

    out_dir_map <- fs::path(model_dir, b, "map")

    if (!dir.exists(fs::path(out_dir_map))) {
      dir.create(fs::path(out_dir_map))
    }

    predict_map(model, out_dir = out_dir_map, rstack = rstack, tile_dir = tile_dir, probability = FALSE, model_name_label = model_name_label)
  })

  if (model_type == "f") {
    combine_sub_maps(bec_shp, model_dir, model_name_label)
  }

  cli::cli_alert_success("Maps have been generated")
  return(TRUE)
}



# combine bgc maps for forested areas
combine_sub_maps <- function(
    bec_shp,
    model_dir,
    model_name_label) {
  # set a list of all sub models to run (ie. BGC folders)
  submods <- basename(fs::dir_ls(model_dir, type = "directory"))

  # if "tiles exist remove this as vector
  submods <- submods[submods != "map"]
  submods <- submods[submods != "balance"]

  ## Generate final map by joining BGC maps together

  # step 1:  set up a key for the combined map (includes all the units)
  rkey <- purrr::map(submods, function(f) {
    keys <- utils::read.csv(fs::path(model_dir, f, "map", "response_names.csv")) |>
      dplyr::mutate(model = f)
  }) |> dplyr::bind_rows()

  rkey <- rkey |> dplyr::mutate(map.response = seq_len(nrow(rkey)))


  # Step 2: For each bgc, filter and mask the raster map and update key if needed

  combo_map <- lapply(submods, function(f) {
    # f <- bgcs[[2]]

    rtemp <- terra::rast(file.path(model_dir, f, "map", model_name_label))

    rtemp[is.na(rtemp[])] # <- 0
    names(rtemp) <- "pred_no"

    # filter to only predict over bgc
    bec_filter <- bec_shp |> dplyr::filter(.data$MAP_LABEL == f)

    rtemp <- terra::mask(rtemp, terra::vect(bec_filter))

    subkey <- rkey |>
      dplyr::filter(.data$model == f) |>
      dplyr::mutate(mosaic = as.numeric((.data$pred_no)))

    # check if the key matches or needs reclassification
    if (isTRUE(unique(subkey$mosaic == subkey$map.response))) {
      cli::cli_alert_info("matching key")
    } else {
      cli::cli_alert_info("updating key")

      for (i in 1:nrow(subkey)) {
        subkey_row <- subkey[i, ]

        from <- subkey_row$pred_no
        to <- subkey_row$map.response

        rtemp <- terra::subst(rtemp, from, to)
      }
    }

    rtemp
  })

  rsrc <- terra::sprc(combo_map)
  m <- terra::mosaic(rsrc, fun = "max")

  rkey <- rkey |> dplyr::select(.data$.pred_class, .data$pred_no, .data$model, .data$map.response)

  out_folder <- fs::path(model_dir, "map")

  if (!dir.exists(out_folder)) dir.create(out_folder)

  terra::writeRaster(m, fs::path(out_folder, model_name_label), overwrite = TRUE)

  utils::write.csv(rkey, fs::path(out_folder, "response_names.csv"))

  cli::cli_alert_success("forest map merged and created and saved: {out_folder}")
}
