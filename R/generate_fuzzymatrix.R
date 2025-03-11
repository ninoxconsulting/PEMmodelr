#' Generate a fuzzy matrix for all siteseries units
#'
#' @param training_pts a data frame with the training points
#' @param bgc_col A character string of the column name in the training_pts data
#'  frame that contains BGC unit. Default is "bgc_col"
#' @param nbrs A `data.frame` with the a key to assign all adjacent cell positions
#' within an edatopic positions This is an internal dataset
#' @param edat A `data.frame` with the a list of all site series units and the
#' edotopic positions they have on a standard grid. This is an internal dataset
#' @param overwrite logical value to overwrite the existing file.
#' @param write_output A `logical`if the fuzzy matrix should be be written to disk?
#'     If `TRUE` (default), will write to `out_dir` under the appropriate resolution subfolder.
#' @param out_dir A character string of path which points to output location. A default
#'    location and name are applied in line with standard workflow.
#' @param out_name A character string of the output file name. Default is `fuzzy_matrix.csv`
#'
#'
#' @returns A `data.frame` with the fuzzy matrix for all site series units within the training point dataset
#' @export
#'
#' @examples
#' \dontrun{
#' fmat <- generate_fuzzymatrix(
#'   tpts,
#'   "bgc_cat",
#'   nbrs = utils::read.csv(fs::path_package("PEMmodelr", "extdata/edatopic_neighbours.csv")),
#'   edat = utils::read.csv(fs::path_package("PEMmodelr", "extdata/Edatopic_v12_12.csv")),
#'   write_output = TRUE,
#'   out_dir = fs::path(PEMprepr::read_fid()$dir_3020_draft$path_rel, "20_f"),
#'   out_name = "fuzzy_matrix.csv"
#' )
#' }
generate_fuzzymatrix <- function(training_pts,
                                 bgc_col = "bgc_col",
                                 nbrs = utils::read.csv(fs::path_package("PEMmodelr", "extdata/edatopic_neighbours.csv")),
                                 edat = utils::read.csv(fs::path_package("PEMmodelr", "extdata/Edatopic_v12_12.csv")),
                                 out_dir = NA,
                                 out_name = "fuzzy_matrix.csv",
                                 write_output = TRUE,
                                 overwrite = FALSE)

{

  # generate outname
  outfile <- fs::path(out_dir, out_name)

  # if file exists
  if (fs::file_exists(outfile) & write_output == TRUE & overwrite == FALSE)  {
    cli::cat_line()
    cli::cli_abort("WARNING! {.var {outfile}} already exists, use overwrite = TRUE to overwrite this file")
  }

  edat <- edat |>
    dplyr::mutate(SS_NoSpace = gsub(pattern = "/", replacement = "_", .data$SS_NoSpace))

  bgc_ls <- training_pts |>
    dplyr::select(dplyr::any_of(bgc_col)) |>
    dplyr::pull() |>
    unique()

  bgc_ls <- bgc_ls[!is.na(bgc_ls)]

  # loop through all the bgc units in the file.
  fmat <- purrr::map(bgc_ls, function(bgc) {
    #  bgc <- bgc_ls[1]
    cli::cat_line()
    cli::cli_alert_warning("generating matrix for {.var {bgc}}")

    # Get vector of unique site series in BGC for loop
    ss_all <- dplyr::filter(edat, .data$BGC == bgc) |>
      dplyr::pull(.data$SS_NoSpace) |>
      unique() |>
      sort()

    # Initialize a matrix with all of the site series in the BGC as both the row names and column names
    mtx <- matrix(
      data = NA_integer_,
      nrow = length(ss_all),
      ncol = length(ss_all),
      byrow = FALSE,
      dimnames = list(ss_all, ss_all)
    )
    # Loop through all the site series in the BGC

    for (i in 1:length(ss_all)) {
      # i <- 1

      ss_tar <- ss_all[i]

      # Find cells of the target site series
      ss_cells_tar <- dplyr::filter(edat, .data$SS_NoSpace == ss_tar) |>
        dplyr::pull(.data$Edatopic) |>
        sort()

      # Set score of ss_tar == ss_tar (left-right diagonal) to 1
      mtx[ss_tar, ss_tar] <- 1

      # Extract remaining site series for one-sided matrix
      ss_rem <- dplyr::setdiff(ss_all, ss_tar) |>
        sort()

      # if(length(ss_rem) < 1){
      #  return()
      # }

      for (j in 1:length(ss_rem)) {
        # j <- 1

        # Choose non-target site series from list
        ss_fuzz <- ss_rem[j]

        # Find cells of the non-target site series that may neighbour or overlap the target site series
        ss_cells_fuzz <- dplyr::filter(edat, .data$SS_NoSpace == ss_fuzz) |>
          dplyr::pull(.data$Edatopic)

        # Find neighbours and remove neighbours that are other target cells AND remove the neighbours of target cells that overlap with the fuzzy cells
        ss_nbrs <- dplyr::filter(nbrs, .data$target %in% ss_cells_tar) |>
          dplyr::filter(!.data$fuzzy %in% .data$target) |>
          dplyr::filter(!.data$target %in% ss_cells_tar[ss_cells_tar %in% ss_cells_fuzz]) |>
          dplyr::pull(.data$fuzzy) |>
          sort()

        # Extract which target cells have overlap (share a square) with the non-target cells
        sq_count <- ss_cells_tar[ss_cells_tar %in% ss_cells_fuzz] |> length()
        # Extract which target neighbours have overlap (share a border) with non-target cells
        br_count <- ss_nbrs[ss_nbrs %in% ss_cells_fuzz] |> length()

        # add up the total score as per rules where shared border = +0.1 to score and shared cells = proportion of shared area
        score <- sq_count / length(ss_cells_tar) + br_count * 0.05

        # Update the relevant cell in the matrix
        mtx[ss_tar, ss_fuzz] <- score

        # Check if the inverse score has been computed. If it has, choose the smaller of the two values and update the score for both.
        if (!is.na(mtx[ss_fuzz, ss_tar])) {
          mtx[ss_tar, ss_fuzz] <- min(c(mtx[ss_tar, ss_fuzz], mtx[ss_fuzz, ss_tar]), na.rm = T)
        }
        mtx[ss_tar, ss_fuzz] <- min(c(mtx[ss_tar, ss_fuzz], mtx[ss_fuzz, ss_tar]), na.rm = T)
      }
    }

    mtx <- as.data.frame(mtx)

    # convert format and combine with other bgc units
    mtx_long <- mtx |>
      tibble::rownames_to_column(var = "target") |>
      tidyr::pivot_longer(cols = -.data$target, names_to = "Pred", values_to = "fVal") |>
      dplyr::filter(.data$fVal > 0) |>
      dplyr::arrange(.data$target, dplyr::desc(.data$fVal)) |>
      dplyr::mutate(fVal = round(.data$fVal, 2))

    mtx_long

  }) |> dplyr::bind_rows()


  # check if there are any units that do not appear in the fuzzy matrix that are in the training data set

  munits <- unique(training_pts$mapunit1)

  fmat_units <- unique(fmat$target)

  if (setdiff(munits, fmat_units) |> length() > 0) {
    to_review <- setdiff(munits, fmat_units)
    cli::cat_line()
    cli::cli_alert_warning("There are units in the training data that are not in the fuzzy matrix")
    cli::cli_alert_info("Please review the following units:{.var {to_review}}")
  }


  # write out the units

  if (write_output) {

    if (fs::file_exists(outfile)) {
      cli::cat_line()
      cli::cli_alert_warning("file already exists at {.var {outfile}}, this file will be overwriten")
    }

    output <- utils::write.csv(fmat, fs::path(outfile), row.names = FALSE)
  }

  return(fmat)

}
