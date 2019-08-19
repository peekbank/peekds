#' Process directory to xy data
#'
#' @param format
#' @param dir
#'
#' @return
#' @export
#'
#' @examples
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

process_tobii <- function(dir) {

}

process_smi <- function(dir) {

}

process_eyelink <- function(dir) {

}

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
