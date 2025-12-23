#' Predict Map from model internal function
#'
#' @param model rf model file
#' @param out_dir location to export predicted tiles
#' @param tile_dir location of template tiles
#' @param rstack spatRast stack of all covars
#' @param probability TRUE or FALSE if probability rasters are to be exported
#' @param aoi_df dataframe with xy values for each bec zone
#' @param map_label name of the model file
#'
#' @return TRUE
#' @export
#'
#' @examples
#' \dontrun{
#' predict_map_internal(rf_fit, out_dir, tile_dir, rstack, probability = FALSE)
#' }
predict_map_internal <- function(model,
                                 out_dir,
                                 tile_dir,
                                 rstack,
                                 probability = FALSE,
                                 aoi_df,
                                 map_label = "map.tif") {


  # extract model info to create a key
  rf_fit <- workflows::extract_fit_engine(model)
  .pred_class <- rf_fit$forest$levels
  respNames <- as.data.frame(.pred_class) |>
    dplyr::mutate(pred_no = seq(1:length(.pred_class)))

  utils::write.csv(respNames, file.path(out_dir, "response_names.csv"), row.names = TRUE)


  # convert rasterstack to dataframe to subset by tile
  rstackdf <- terra::as.data.frame(rstack, xy = TRUE)

  # get list of tiles
  ntiles <- list.files(tile_dir, full.names = T)

  # loop through tiles
  tile_run <- purrr::map(ntiles, function(i) {
    #i = ntiles[1]
    out_name <- basename(i)

    # read in tile
    t <- terra::rast(file.path(i))

    # check if blank tile
    if (all(is.na(unique(terra::values(t)))) == TRUE) {
      cli::cli_alert_warning("Some variables with all NA values, skipping tile...")
    } else {

      # convert to dataframe and add rast stack values for XYs
      tdf <- as.data.frame(t, xy = TRUE)

      # check if any of the tile is within the BGC
      bec_check <- dplyr::left_join(tdf, aoi_df, by = dplyr::join_by(x, y)) |>
        dplyr::select(-.data$lyr.1)

      if (!all(is.na(bec_check$layer))) {
        # join the values of stacked raster to tile based on x and y
        rsf <- dplyr::left_join(tdf, rstackdf, by = dplyr::join_by(x, y)) |>
          dplyr::select(-.data$lyr.1)

        # create raterstack for tile
        tstack <- terra::rast(rsf, crs = terra::crs(t))
        bec_tile <- terra::rast(bec_check, crs = terra::crs(t))

        # check if all values in columns are NA (ie not in study area)
        na_table <- as.data.frame(sapply(rsf, function(x) all(is.na(x))))

        if (any(na_table[, 1] == TRUE)) {
          cli::cli_alert_warning("Some variables with all NA values, skipping tile...")
        } else {

          # predict
          pred <- terra::predict(tstack, rf_fit, na.rm = TRUE)
          #pred <- terra::predict(tstack, model, na.rm = TRUE)
          # mask to BEC
          pred <- terra::mask(pred, bec_tile)

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

          best_class <- colnames(pdf)[apply(pdf[, 2:length(pdf)], 1, which.max)]

          r_out <- cbind(pdfid, as.factor(best_class))
          names(r_out) <- c("x", "y", ".pred_class")

          ## change the text values to numeric values.
          r_out <- dplyr::left_join(r_out, respNames, by = ".pred_class")
          r_out <- r_out |> dplyr::select(-".pred_class")

          cli::cli_alert_info("... exporting raster tiles...")

          out <- tidyterra::as_spatraster(r_out, crs = "epsg:3005")

          terra::writeRaster(out, fs::path(out_dir, "best", out_name), overwrite = TRUE)
        }
      } else {
        # print("skip tile and not within BEC zone")
        cli::cli_alert_info("skipping tile and not within BEC zone")
        terra::writeRaster(t, fs::path(out_dir, "best", out_name), overwrite = TRUE)
      }
    }
  }, .progress = list(
    type = "iterator",
    format = "Calculating {cli::pb_bar} {cli::pb_percent}",
    clear = TRUE
  ))


  cli::cli_alert_success("All predicted tiles generated")

  r_tiles <- list.files(fs::path(out_dir, "best"), pattern = ".tif$", full.names = TRUE)
  rsrc <- terra::sprc(r_tiles)
  m <- terra::mosaic(rsrc, fun = "min")
  terra::writeRaster(m, fs::path(out_dir, map_label), overwrite = TRUE)

  if (probability == TRUE) {
    r_tiles <- list.files(fs::path(out_dir, "probability"), pattern = ".tif$", full.names = TRUE)
    rsrc <- terra::sprc(r_tiles)
    m <- terra::mosaic(rsrc, fun = "min")
    terra::writeRaster(m, fs::path(out_dir, paste0("probability_", map_label)), overwrite = TRUE)
  }

  return(TRUE)
}
