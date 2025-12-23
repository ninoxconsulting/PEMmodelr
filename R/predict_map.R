#' Generate maps from predicted model and output map or combined maps
#'
#' @param bec A character defining the MAP_LABEL zone to be modeled, i.e. "ICHmc1"
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
#' predict_map(rf_fit, out_dir, tile_dir, rstack, probability = FALSE)
#'}
predict_map <- function(model,
                        out_dir,
                        tile_dir,
                        rstack,
                        probability = FALSE,
                        model_name_label = "map.tif") {
  # extract fit
  rf_fit <- workflows::extract_fit_engine(model)
  .pred_class <- rf_fit$forest$levels
  respNames <- as.data.frame(.pred_class) |>
    dplyr::mutate(pred_no = seq(1:length(.pred_class)))

  utils::write.csv(respNames, file.path(out_dir, "response_names.csv"), row.names = TRUE)

  ntiles <- list.files(tile_dir, full.names = T)
  a <- 0 ## running total of area complete
  ta <- sum(as.numeric(length(ntiles)))

  for (i in ntiles) {
    # i = ntiles[3]
    out_name <- basename(i)

    # create tracking message
    t <- terra::rast(file.path(i)) ## read in tile
    cli::cli_alert_info("working on {out_name} of {length(ntiles)}")
    cli::cli_alert_info("... loading data ...")

    # check if blank tile
    if (all(is.na(unique(terra::values(t)))) == TRUE) {
      cli::cli_alert_warning("Some variables with all NA values, skipping tile...")
    } else {
      # crop the raster stack to tile extent
      tstack <- terra::crop(rstack, t)
      # convert to dataframe
      rsf <- as.data.frame(tstack)
      # get xy values
      # rsfxy <- terra::crds(tstack)

      # check if all values in columns are NA (ie not in study area)
      na_table <- as.data.frame(sapply(rsf, function(x) all(is.na(x))))

      if (any(na_table[, 1] == TRUE)) {
        cli::cli_alert_warning("Some variables with all NA values, skipping tile...")
      } else {
        # predict
        pred <- terra::predict(tstack, rf_fit, na.rm = TRUE)

        # write out probability layer
        if (probability == TRUE) {
          if (!dir.exists(file.path(out_dir, "probability"))) {
            dir.create(file.path(out_dir, "probability"))
          } else {
            cli::cli_alert_info("probability dir exists")
          }
          terra::writeRaster(pred, file.path(out_dir, "probability", out_name), overwrite = TRUE)
          cli::cli_alert_success("writing probability tile")
        }

        # write out best class
        if (!dir.exists(file.path(out_dir, "best"))) {
          dir.create(file.path(out_dir, "best"))
        }

        pdfxy <- as.data.frame(pred, xy = TRUE, cells = FALSE)
        pdf <- pdfxy |> dplyr::select(-.data$x, -.data$y)
        pdfid <- pdfxy |> dplyr::select(.data$x, .data$y)

        best_class <- colnames(pdf)[apply(pdf[, 1:length(pdf)], 1, which.max)]

        r_out <- cbind(pdfid, as.factor(best_class))
        names(r_out) <- c("x", "y", ".pred_class")

        ## change the text values to numeric values.
        r_out <- dplyr::left_join(r_out, respNames, by = ".pred_class")
        r_out <- r_out |> dplyr::select(-".pred_class")

        cli::cli_alert_info("... exporting raster tiles...")

        out <- tidyterra::as_spatraster(r_out, crs = "epsg:3005")

        terra::writeRaster(out, fs::path(out_dir, "best", out_name), overwrite = TRUE)
      }
    }

    ## * report progress -----
    a <- a + 1
    prog <- round(a / ta * 100, 0)
    cli::cli_alert_info("{prog} % complete")
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
