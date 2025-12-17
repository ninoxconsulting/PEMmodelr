#' Define mapunits to convert field data to modelled data
#'
#' @param tps sf attribute with field data to be converted
#' @param mapkey datatable with mapunit key to define field points and attribute to which mapunits will be converted to
#' @param attribute character to define the column name in mapkey that unis will be converted
#' @return sf attribute with updated mapunit 1 and mapunit 2 fields
#' @export
#'
#' @examples
#' \dontrun{
#' define_mapunits(tps, mapkey, attribute)
#' }
set_mapunits <- function(tps, mapkey, attribute) {
  #  #  testing lines
  # tps = allpts
  #  mpts = mapkey
  #  attribute = "mapunit_ss_realm"
  #  # end testing lines

  # check if tps is sf
  tps <- PEMprepr:::read_sf_if_necessary(tps)

  # check the attribute is within the mapkey
  if (!attribute %in% colnames(mapkey)) {
    cli::cat_line
    cli::cli_abort("{.var {attribute} is not present in mapkey, please check the attribute name and re-run}")
  }

  # match the column for map unit based on key
  mapkeysub <- mapkey |>
    dplyr::select(.data$fieldcall, dplyr::any_of(attribute))
  names(mapkeysub) <- c("fieldcall", "mapunit")

  # format spaces
  tps <- tps |>
    dplyr::mutate(mapunit1 = stringr::str_trim(.data$mapunit1)) |>
    dplyr::mutate(mapunit2 = stringr::str_trim(.data$mapunit2))

  # check the field call has the equivalent code in mapkey and not blank
  fieldcalls <- unique(c(tps$mapunit1, tps$mapunit2))
  mismatch_calls = setdiff(fieldcalls, mapkeysub$fieldcall)

  if(length(mismatch_calls)>1){
    cli::cat_line
    cli::cli_abort("Missing values within the mapkey for some fieldcalls {.var {mismatch}}")
  }

  # check the field call equivalent is not NA in the mapkey
  key <- mapkeysub |>
    dplyr::filter(.data$fieldcall %in% fieldcalls) |>
    dplyr::filter(is.na(.data$mapunit))


  if(length(key$fieldcall)>0){
    cli::cat_line
    cli::cli_alert_warning("The following field call : {key$fieldcall} is recorded as NA within the mapkey for {.var {attribute}}, please check this is valid")
  }

  # format the mapcalls
  outdata <- tps |>
    dplyr::left_join(mapkeysub, by = c("mapunit1" = "fieldcall")) |>
    dplyr::select(-.data$mapunit1) |>
    dplyr::rename(mapunit1 = .data$mapunit) |>
    dplyr::left_join(mapkeysub, by = c("mapunit2" = "fieldcall")) |>
    dplyr::select(-.data$mapunit2) |>
    dplyr::rename(mapunit2 = .data$mapunit)

  outdata <- outdata |>
    dplyr::mutate(mapunit1 = ifelse(!is.na(.data$mapunit2) & is.na(.data$mapunit1), .data$mapunit2, .data$mapunit1)) |>
    dplyr::mutate(mapunit2 = ifelse((.data$mapunit1 == .data$mapunit2), NA, .data$mapunit2))

  outdata <- outdata |>
    dplyr::filter(!is.na(.data$mapunit1)) |>
    dplyr::filter(.data$mapunit1 != "") |>
    dplyr::mutate(mapunit1 = dplyr::case_when(
      .data$mapunit1 == "" ~ NA,
      .default = as.character(.data$mapunit1)
    )) |>
    dplyr::mutate(mapunit2 = dplyr::case_when(
      mapunit2 == "" ~ NA,
      .default = as.character(.data$mapunit2)
    ))

  return(outdata)
}
