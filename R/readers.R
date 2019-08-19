#' Process directory of raw data to xy data
#'
#' @param format One of "tobii", "smi", "eyelink"
#' @param dir Director with raw data
#'
#' @export
process_to_xy <- function(format, dir) {
  readers <- list(
    "tobii" = process_tobii,
    "smi" = process_smi,
    "eyelink" = process_eyelink
  )
  assertthat::assert_that(format %in% names(readers))
  reader <- readers[format]
  reader(dir)
}

#' Process tobii raw data
#'
#' @param dir Director with raw data
#'
#' @export
process_tobii <- function(dir) {

}

#' Process smi raw data
#'
#' @param dir Director with raw data
#'
#' @export
process_smi <- function(dir) {

}

#' Process eyelink raw data
#'
#' @param dir Director with raw data
#'
#' @export
process_eyelink <- function(dir) {

}

#' Generate aoi data from xy data
#'
#' @param dir Directory with xy data and metadata
#'
#' @export
generate_aoi <- function(dir) {
  validate_for_aoi_conversion(dir)
  # read in xy_data, trials, aoa_coordinates
  # assign aoa based on aoa_coordinates
  # find correct aoi based on trials
  # interpolate
  # center timestamp (0 POD)
  # downsample to 30Hz
  # write aoi_data
}
