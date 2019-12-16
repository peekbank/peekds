# This function trims the xy_data such that only gaze coordinates that fall within the stimuli space is preserved
# To make this work, a line of code will need to be added to each import script as follows:
# xy_data <- xy_trim(here(lab_dir, "processed_data/"))

xy_trim <- function(dir) {
  xy <- readr::read_csv(file.path(dir, "xy_data.csv"))
  datasets <- readr::read_csv(file.path(dir, "datasets.csv"))
  datasets <- datasets %>%
    mutate(video_size_x = ifelse(monitor_size_x == 1280, 1280, 1200),
           video_size_y = ifelse(monitor_size_y == 1024, 960, 900))
  xy <- xy %>%
    filter(x > (datasets$monitor_size_x - datasets$video_size_x)/2,
           x < datasets$monitor_size_x - (datasets$monitor_size_x - datasets$video_size_x)/2,
           y > (datasets$monitor_size_y - datasets$video_size_y)/2,
           y < datasets$monitor_size_y - (datasets$monitor_size_y - datasets$video_size_y)/2)
}
