#' Reduce the number of features by removing highly correlated covariates
#'
#' @param mcols A character vector of the names of all covariates to be assessed
#' @param covarkey A data frame of the covariates listed by type.
#' @param covar_dir A character string of the path to the directory containing the covariates
#' @param covtype A character vector of the type of covariates to be assessed. Options must match covarkey types.
#' Default are (dem, structure, satellite)
#' @param cutoff A numeric value of the correlation cutoff. Default is 0.90
#' @export
#' @examples
#' \dontrun{
#' reduce_features(mcols,
#'   covarkey = read.csv(fs::path(PEMprepr::read_fid()$dir_30_model$path_rel, "covar_key.csv")),
#'   covar_dir = fs::path(PEMprepr::read_fid()$dir_1020_covariates$path_rel, "5m"),
#'   covtype = c("dem", "satellite"),
#'   cutoff = 0.90
#' )
#' }
reduce_features <- function(
    mcols = mcols,
    covarkey = utils::read.csv(fs::path(PEMprepr::read_fid()$dir_30_model$path_rel, "covar_key.csv")),
    covar_dir = fs::path(PEMprepr::read_fid()$dir_1020_covariates$path_rel, "5m"),
    covtype = "dem",
    cutoff = 0.90) {

  # check covarkey is a data frame and contains the appropriate columns
  if (!is.data.frame(covarkey)) {
    cli::cli_abort("{.var covarkey} must be a data frame")
  }

  if (!all(c("value", "type") %in% colnames(covarkey))) {
    cli::cli_abort("{.var covarkey} must contain columns 'value' and 'type'")
  }

  # check covtype is a character vector within the covarkey type columns
  if (!all(covtype %in% unique(covarkey$type))) {
    cli::cli_abort("{.var covtype} must be a character vector within the covarkey type column")
  }

  # subset by type of covariate
  covall <- covarkey |>
    dplyr::filter(.data$type %in% covtype) |>
    dplyr::select(.data$value) |>
    dplyr::pull() |>
    tolower()

  # select covars within the reduced list which are within covar type assigned
  mcols <- mcols[mcols %in% covall]

  # read in the raster template used for modelling (i.e 5m resolution)
  rastlist <- list.files(fs::path(covar_dir), pattern = ".sdat$|.tif$", full.names = TRUE, recursive = TRUE)
  rast_list <- rastlist[tolower(gsub(".sdat|.tif", "", basename(rastlist))) %in% tolower(mcols)]

  trasts <- rastlist[tolower(gsub(".sdat|.tif", "", basename(rastlist))) %in% tolower(mcols)]
  trstack <- terra::rast(trasts)

  subsmpl <- terra::spatSample(trstack,
    size = 1000000,
    method = "regular",
    xy = FALSE,
    na.rm = TRUE
  )

  subdf <- data.frame(subsmpl)

  # remove rows with all NA
  not_all_na <- function(x) any(!is.na(x))
  subdf <- subdf |> dplyr::select(dplyr::where(not_all_na))

  subdf <- subdf[stats::complete.cases(subdf), ]

  if (nrow(subdf) == 0) {
    cli::cli_abort("check the covariates listed or run in groups as as pixals have data in all covariates")
  }

  uniquevals <- apply(subdf, 2, function(x) length(unique(x)))
  uniquenames <- names(uniquevals[uniquevals == 1])
  subdf <- subdf[!names(subdf) %in% uniquenames]

  # check correlation between covars
  corr_matrix <- stats::cor(subdf)
  hc <- caret::findCorrelation(corr_matrix, cutoff = cutoff)

  rcovs <- names(subsmpl[, -c(hc)])

  binary_covars <- c("sinkroute", "channelsnetwork")

  if (any(binary_covars %in% rcovs)) {
    rcovs <- rcovs[!rcovs %in% binary_covars]
  }

  removed_covs <- mcols[!mcols %in% rcovs]
  print("the following covariates are removed as highly correlated:")
  print(removed_covs)

  return(rcovs)
}
