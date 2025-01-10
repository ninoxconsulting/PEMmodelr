#' Combine the final map including forest/non-forest, forest and non-forest maps
#'
#' @param fnf_dir a character or path to the location of the forest/non-forest directory
#' @param fnf_key a character or path to the location of the forest/non-forest key
#' @param fnf_map a character or path to the location of the forest/non-forest map (.tif)
#' @param f_dir a character or path to the location of the forest directory
#' @param f_map a character or path to the location of the forest map (.tif)
#' @param f_key a character or path to the location of the forest key
#' @param nf_dir a character or path to the location of the non-forest directory
#' @param nf_map a character or path to the location of the non-forest map (.tif)
#' @param nf_key a character or path to the location of the non-forest key
#' @param out_dir a character or path to the location of the output directory
#' @param outname a character with the name of the output map. Default is "full_map.tif"
#'
#' @returns a SpatRaster object and response key saved to out_dir
#' @export
#'
#' @examples
#' \dontrun{
#' final_map( fnf_dir = fs::path(PEMprepr::read_fid()$dir_3020_draft$path_rel, "10_fnf","fnf","map"),
#'           fnf_map = terra::rast(fs::path(fnf_dir, "best_map.tif")),
#'           fnf_key = utils::read.csv(fs::path(fnf_dir, "response_names.csv")),
#'           f_dir = fs::path(PEMprepr::read_fid()$dir_3020_draft$path_rel, "20_f","map"),
#'           f_map <- terra::rast(fs::path(f_dir, "best_map.tif")),
#'           f_key <- read.csv(fs::path(f_dir, "response_key.csv")),
#'           nf_dir = fs::path(PEMprepr::read_fid()$dir_3020_draft$path_rel, "30_nf", "nf","map"),
#'           nf_map <- terra::rast(fs::path(nf_dir, "best_map.tif")),
#'           nf_key <- read.csv(fs::path(nf_dir, "response_names.csv")),
#'           out_dir = fs::path(PEMprepr::read_fid()$dir_3020_draft$path_rel),
#'           outname = "full_map.tif")
#' }
final_map <- function(
    fnf_dir = fs::path(PEMprepr::read_fid()$dir_3020_draft$path_rel, "10_fnf","fnf","map"),
    fnf_map = fnf_map,
    fnf_key = fnf_key,
    f_dir = fs::path(PEMprepr::read_fid()$dir_3020_draft$path_rel, "20_f","map"),
    f_map = f_map,
    f_key = f_key,
    nf_dir = fs::path(PEMprepr::read_fid()$dir_3020_draft$path_rel, "30_nf", "nf","map"),
    nf_map = nf_map,
    nf_key = nf_key,
    out_dir = fs::path(PEMprepr::read_fid()$dir_3020_draft$path_rel),
    outname = "full_map.tif") {

  if(!dir.exists(out_dir)){
    fs::dir_create(out_dir)
  }

  # mask and apply to full non forest model

  fkey <- fnf_key |>
    dplyr::filter(.data$.pred_class == "forest") |>
    dplyr::pull(.data$pred_no)

  msk_nf <- terra::ifel(fnf_map == fkey, NA, 1)
  nf_mask <- terra::mask(fnf_map, msk_nf)

  nonfor_map <- terra::mask(nf_map, nf_mask)

  # mask and apply to full forest model

  nfkey <- fnf_key |>
    dplyr::filter(.data$.pred_class == "nonforest") |>
    dplyr::pull(.data$pred_no)

  msk_f <- terra::ifel(fnf_map == nfkey, NA, 1)
  f_mask <- terra::mask(fnf_map, msk_f)
  #terra::plot(f_mask)

  for_map <- terra::mask(f_map, f_mask)
  #terra::plot(for_map)
  #f_key

  ## non- vegetated filter....

  #  nvkey <- fnf_key |>
  #    dplyr::filter(.pred_class == "nonvegetated") |>
  #    dplyr::pull(pred_no)

  # Merge the response key to match new full map

  nf_key <- nf_key |> dplyr::mutate(model = "nf")

  rkey <- list(f_key, nf_key) |>
    dplyr::bind_rows()

  rkey <- rkey |> dplyr::mutate(map.response_full = seq_len(nrow(rkey)))

  # update the non-forest map codes
  #nonfor_map
  #terra::plot(nonfor_map)
  #rkey

  rtemp <- nonfor_map

  subkey <- rkey |>
    dplyr::filter(.data$model == "nf")

  # check if the key matches or needs reclassification
  if (isTRUE(unique(subkey$.pred_class == subkey$map.response_full))) {
    cli::cli_alert_info("matching key")
  } else {
    cli::cli_alert_info("updating key")

    for (i in 1:nrow(subkey)) {
      subkey_row <- subkey[i, ]

      from <- subkey_row$pred_no
      to <- subkey_row$map.response_full

      rtemp <- terra::subst(rtemp, from, to)
    }
  }

  # rtemp

  # Merge the forest and non-forest map

  full_map <- terra::mosaic(for_map, rtemp, fun = "max")
  terra::plot(full_map)

  # tidy key and output maps
  rkey <- rkey |> dplyr::select(.data$.pred_class, .data$pred_no, .data$model, .data$map.response_full)

  #write the full map and key

  terra::writeRaster(full_map, fs::path(out_dir, outname), overwrite = TRUE)

  utils::write.csv(rkey, fs::path(out_dir, "full_map_response.csv"))

  return(full_map)
}
