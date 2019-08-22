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
                  t_zeroed = .data$t_trial - .data$point_of_disambig)

  # set sample rates
  SAMPLE_RATE = 40 # Hz
  SAMPLE_DURATION = 1000/SAMPLE_RATE
  MAX_GAP_LENGTH = .100 # S
  MAX_GAP_SAMPLES = MAX_GAP_LENGTH / (1/SAMPLE_RATE)

  # resample and interpolate
  aoi <- xy_joined %>%
    dplyr::group_by(.data$subject_id, .data$trial_id) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      data = .data$data %>%
        purrr::map(function(df) {
          df_rounded <- df %>%
            dplyr::select(t_zeroed, aoi) %>%
            dplyr::mutate(t_zeroed = round(SAMPLE_DURATION * round(t_zeroed/SAMPLE_DURATION)))

          t_resampled <- tibble::tibble(t_zeroed = round(seq(min(df_rounded$t_zeroed),
                                                             max(df_rounded$t_zeroed),
                                                             SAMPLE_DURATION)))


          dplyr::left_join(t_resampled, df_rounded) %>%
            # fuzzyjoin::difference_left_join(select(df,
            #                                        t_zeroed, aoi),
            #                                 max_dist = round(1000/SAMPLE_RATE)/2) %>%
            dplyr::group_by(t_zeroed) %>%
            dplyr::summarise(aoi = na_mode(aoi)) %>%
            dplyr::rename(t = t_zeroed) %>%
            dplyr::mutate(aoi = zoo::na.locf(aoi,
                                             maxgap = MAX_GAP_SAMPLES,
                                             na.rm=FALSE)) # last observation carried forward
          })) %>%
    tidyr::unnest(.data$data) %>%
    mutate(aoi_data_id = 0:(n() - 1))

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
