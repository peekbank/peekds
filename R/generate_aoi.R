#' @importFrom dplyr "%>%"
#' @importFrom magrittr "%<>%"
#' @importFrom rlang .data
NULL

#' Resample times to be consistent across labs
#'
#' @param dir df that has subject_id, dataset_id, trial_id and times
#'
#' @return
#' @export
resample_times <- function(df) {
  # set sample rates
  SAMPLE_RATE = 40 # Hz
  SAMPLE_DURATION = 1000/SAMPLE_RATE
  MAX_GAP_LENGTH = .100 # S
  MAX_GAP_SAMPLES = MAX_GAP_LENGTH / (1/SAMPLE_RATE)

  # center timestamp (0 POD)
  df <- df %>%
    dplyr::group_by(.data$administration_id, .data$trial_id, .data$dataset_id) %>%
    dplyr::mutate(t_trial = .data$t - .data$t[1],
                  t_zeroed = .data$t_trial - .data$point_of_disambiguation)

  df %>% dplyr::group_by(.data$administration_id, .data$trial_id) %>%
  tidyr::nest() %>%
  dplyr::mutate(
    data = .data$data %>%
      purrr::map(function(df) {
        df_rounded <- df %>%
          dplyr::mutate(t_zeroed = round(SAMPLE_DURATION * round(t_zeroed/SAMPLE_DURATION)))

        t_resampled <- tibble::tibble(t_zeroed = round(seq(min(df_rounded$t_zeroed),
                                                           max(df_rounded$t_zeroed),
                                                           SAMPLE_DURATION)))

        dplyr::left_join(t_resampled, df_rounded, by = "t_zeroed") %>%
          dplyr::group_by(t_zeroed)
      })) %>%
  tidyr::unnest(.data$data)
}


#' Generate aoi data from xy data
#'
#' @param dir Directory with xy data and metadata
#'
#' @export
generate_aoi <- function(dir) {
  SAMPLE_RATE = 40 # Hz
  SAMPLE_DURATION = 1000/SAMPLE_RATE
  MAX_GAP_LENGTH = .100 # S
  MAX_GAP_SAMPLES = MAX_GAP_LENGTH / (1/SAMPLE_RATE)

  # read in xy_data, trials, aoa_coordinates
  xy <- readr::read_csv(file.path(dir, "xy_timepoints.csv"))
  trials <- readr::read_csv(file.path(dir, "trials.csv"))
  aoi_regions <- readr::read_csv(file.path(dir, "aoi_region_sets.csv"))

  xy_joined <- xy %>%
    dplyr::left_join(trials, by = "trial_id") %>%
    dplyr::left_join(aoi_regions, by = "aoi_region_set_id")

  # assign aoa based on aoa_coordinates
  # find correct aoi based on trials
  xy_joined <- add_aois(xy_joined)

  aoi = resample_times(xy_joined) %>%
    dplyr::select(dataset_id, administration_id, trial_id, t_zeroed, aoi) %>%
    dplyr::rename(t_norm = t_zeroed) %>%
    dplyr::group_by(dataset_id, administration_id, trial_id, t_norm) %>%
    dplyr::summarise(aoi = na_mode(aoi)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(dataset_id, administration_id, trial_id) %>%
    dplyr::mutate(aoi = zoo::na.locf(aoi,
                                     maxgap = MAX_GAP_SAMPLES,
                                     na.rm=FALSE)) %>%  # last observation carried forward
    dplyr::ungroup() %>%
    dplyr::mutate(aoi_timepoint_id = 0:(dplyr::n() - 1)) %>%
    dplyr::select(-dataset_id)
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
