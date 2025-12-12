#' Prepare cleaned training points for model runs
#'
#' @param allpts A `sf` object with cleaned and attribute points. This is the output of `PEMsamplr::attribute_pts()`
#' @param mapkey A csv with map unit key. This is the output of `PEMsamplr::generate_mapkey()`
#' @param covarkey A csv with covariate key. This is the output of `PEMmodelr::generate_covar_key()`
#' @param attribute A character string matching the columns within the mapkey which fieldmap units will be matched to. The default
#'  is "mapunit_ss_realm" for forest model, and "mapunit_fnf" for forest non-forest model, and "mapunit_fnf" for non-forest model.
#' @param extra_pts A Logical TRUE/FALSE to include extra points in the training dataset. Default is FALSE.
#' @param bec A `sf` object with BEC zones. The default points to the BEC zones .gpkg in the standard workflow.
#' @param min_no A numeric value of the minimum number of points required for each map unit. Default is 10.
#'
#' @returns A cleaned training point dataset with BEC zones and map units assigned.
#' @export
#'
#' @examples
#' \dontrun{
#' tpts <- prep_tps(
#'   allpts = sf::st_read(
#'     fs::path(PEMprepr::read_fid()$dir_20105030_attributed_field_data$path_rel,
#'     "allpoints_att.gpkg")),
#'   mapkey = read.csv(fs::path(PEMprepr::read_fid()$dir_3010_inputs$path_rel,
#'   "mapunitkey_final.csv")),
#'   covarkey = covarkey,
#'   attribute = "mapunit_ss_realm",
#'   extra_pts = FALSE,
#'   bec = sf::st_read(fs::path(PEMprepr::read_fid()$dir_1010_vector$path_rel, "bec.gpkg")),
#'   min_no = 20
#' )
#' }
prep_tps <- function(
    allpts = sf::st_read(fs::path(PEMprepr::read_fid()$dir_20105030_attributed_field_data$path_rel, "allpoints_att.gpkg")),
    mapkey = utils::read.csv(fs::path(PEMprepr::read_fid()$dir_3010_inputs$path_rel, "mapunitkey_final.csv")),
    covarkey = utils::read.csv(fs::path(PEMprepr::read_fid()$dir_30_model$path_rel, "covar_key.csv")),
    attribute = "mapunit_ss_realm",
    extra_pts = FALSE,
    bec = sf::st_read(fs::path(PEMprepr::read_fid()$dir_1010_vector$path_rel, "bec.gpkg")),
    min_no = 10) {

  # assign nap unit name based on mapkey and atttribute param as column
  mpts <- set_mapunits(allpts, mapkey, attribute)

  # assign BEC zone to each point
  subzones <- unique(bec$MAP_LABEL)
  subzones <- tolower(gsub("\\s+", "", subzones))

  # Intersect BEC zones and format the dataset
  tpts <- sf::st_join(mpts, bec[, "MAP_LABEL"])

  tpts <- tpts |>
    #dplyr::mutate(fnf = ifelse(grepl(paste0(subzones, collapse = "|"), tolower(.data$mapunit1)), "forest", "non_forest")) |>
    dplyr::rename(bgc_cat = .data$MAP_LABEL) |>
    dplyr::rename_all(.funs = tolower)
#
#   if (attribute == "mapunit_fnf") {
#     tpts <- tpts |>
#       dplyr::mutate(fnf = .data$mapunit1)
#   }

  # remove points with less than min_no points
  tpts <- .filter_min_mapunits(tpts, min_no, extra_pts)

  tpts <- tpts |>
    dplyr::mutate(
      mapunit1 = as.factor(.data$mapunit1),
      mapunit2 = as.factor(.data$mapunit2)
    )

  extra_names <- covarkey |>
    dplyr::filter(.data$type == "extra") |>
    dplyr::select(.data$value) |>
    dplyr::pull()

  tpts <- tpts |> dplyr::select(-dplyr::any_of(extra_names))

  return(tpts)
}
