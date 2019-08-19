#' Generate aoi data from xy data
#'
#' @param dir Directory with xy data and metadata
#'
#' @export
generate_aoi <- function(dir) {
  validate_for_aoi_conversion(dir)

  # read in xy_data, trials, aoa_coordinates
  xy <- read_csv(file.path(dir, "xy_data.csv"))
  trials <- read_csv(file.path(dir, "trials.csv"))
  aoi_coordinates <- read_csv(file.path(dir, "aoi_coordinates.csv"))

  xy_joined <- xy %>%
    left_join(aoi_coordinates) %>%
    left_join(trials)

  # assign aoa based on aoa_coordinates
  # find correct aoi based on trials
  xy_joined <- add_aois(xy_joined)

  # center timestamp (0 POD)
  # TODO: check if POD is unique
  xy_joined <- xy_joined %>%
    group_by(subid, trial_id) %>%
    mutate(t_trial = t - t[1],
           t_zeroed = t_trial - point_of_disambiguation)

  # downsample to 30Hz
  foo <- zoo(x = sin(1:10),
                lubridate::parse_date_time((1:10) * 1/60,"s"))
  foo_ts <- xts::xts(foo)
  xts::endpoints(foo_ts, "ms", 30)

  # interpolate

    # write aoi_data
}


#' Generate aoi data from xy data
#'
#' @param dir Directory with xy data and metadata
#'
#' @export
add_aois <- function(xy_joined) {
  xy_joined %<>%
    mutate(
      side = case_when(
        x > l_xmin & x < l_xmax & y > l_ymin & y < l_ymax ~ "left",
        x > r_xmin & x < r_xmax & y > r_ymin & y < r_ymax ~ "right",
        !is.na(x) & !is.na(y) ~ "other",
        TRUE ~ NA),
      aoi = case_when(
        side %in% c("left","right") & side == target_side ~ "target",
        side %in% c("left","right") & side != target_side ~ "distractor",
        TRUE ~ side # other or NA, which is same as side
      ))

  return(xy_joined)
}
