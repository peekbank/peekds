#' Generate aoi data from xy data
#'
#' @param dir Directory with xy data and metadata
#'
#' @export
generate_aoi <- function(dir) {
  # validate_for_aoi_conversion(dir)

  # read in xy_data, trials, aoa_coordinates
  xy <- readr::read_csv(file.path(dir, "xy_data.csv"))
  trials <- readr::read_csv(file.path(dir, "trials.csv"))
  aoi_regions <- readr::read_csv(file.path(dir, "aoi_regions.csv"))

  xy_joined <- xy %>%
    dplyr::left_join(trials) %>%
    dplyr::left_join(aoi_regions)

  # assign aoa based on aoa_coordinates
  # find correct aoi based on trials
  xy_joined <- add_aois(xy_joined)

  # center timestamp (0 POD)
  xy_joined <- xy_joined %>%
    dplyr::group_by(.data$subject_id, .data$trial_id) %>%
    dplyr::mutate(t_trial = .data$t - .data$t[1],
                  t_zeroed = .data$t_trial - .data$point_of_disambiguation)

  t_resampled <- downsample(xy_joined)

  aoi <- dplyr::group_by(t_resampled, subject_id, trial_id) %>%
    dplyr::group_by(subject_id, trial_id, t) %>%
    dplyr::summarise(aoi = na_mode(aoi), x = mean(x), y = mean(y)) %>% # take the most common aoi for that rounded time
    # if more than N NAs, keep them. if not, set to last
    select(subject_id, trial_id, t, aoi, x, y) %>%
    ungroup() %>%
    tidyr::complete(t) %>%
    dplyr::group_by(subject_id, trial_id) %>%
    dplyr::mutate(aoi = zoo::na.locf(aoi,
                                     maxgap = MAX_GAP_SAMPLES,
                                     na.rm=FALSE),
                  aoi_data_id = 0:(n() - 1))


  readr::write_csv(aoi, file.path(dir, "aoi_data.csv"))
}

#' Add AOIs
#'
#' @param xy_joined
#'
#' @export
add_aois <- function(xy_joined) {
  xy_joined %<>%
    dplyr:: mutate(
      side = dplyr::case_when(
        x > l_x_min & x < l_x_max & y > l_y_min & y < l_y_max ~ "left",
        x > r_x_min & x < r_x_max & y > r_y_min & y < r_y_max ~ "right",
        !is.na(x) & !is.na(y) ~ "other",
        TRUE ~ as.character(NA)),
      aoi = dplyr::case_when(
        side %in% c("left","right") & side == target_side ~ "target",
        side %in% c("left","right") & side != target_side ~ "distractor",
        TRUE ~ side # other or NA, which is same as side
      ))

  return(xy_joined)
}

#' Generate aoi data from xy data
#'
#' @param xy_joined
#'
#' @export
downsample <- function(xy_joined) {
  # set sample rates
  SAMPLE_RATE = 40 # Hz
  SAMPLE_DURATION = 1000/SAMPLE_RATE
  MAX_GAP_LENGTH = .100 # S
  MAX_GAP_SAMPLES <<- MAX_GAP_LENGTH / (1/SAMPLE_RATE)

  # zero out times, round down
  t_resampled <- dplyr::group_by(xy_joined, subject_id, trial_id) %>%
    dplyr::mutate(t_zeroed = t_zeroed - (t_zeroed %% SAMPLE_DURATION))  %>%
    dplyr::mutate(t = t_zeroed)

  return(t_resampled)
}
