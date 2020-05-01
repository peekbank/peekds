#' This function trims the xy_data such that only gaze coordinates that fall within the stimuli space is preserved
#'
#'
#' @param xy_data the pre-trim xy data generated from the import script
#' @param datasets the peekDS formatted datasets.csv that contains screen dimension info
#' @export

xy_trim <- function(xy_data, datasets) {
  datasets <- datasets
  datasets <- datasets %>%
    dplyr::mutate(video_size_x = ifelse(monitor_size_x == 1280, 1280, 1200),
                  video_size_y = ifelse(monitor_size_y == 1024, 960, 900))
  xy_data <- xy_data %>%
    dplyr::filter(x > (datasets$monitor_size_x - datasets$video_size_x)/2,
                  x < datasets$monitor_size_x - (datasets$monitor_size_x - datasets$video_size_x)/2,
                  y > (datasets$monitor_size_y - datasets$video_size_y)/2,
                  y < datasets$monitor_size_y - (datasets$monitor_size_y - datasets$video_size_y)/2)
}
