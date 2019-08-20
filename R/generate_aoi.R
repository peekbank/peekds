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


  # resample and interpolate

  # set sample rates
  SAMPLE_RATE = 30 # Hz
  MAX_GAP_LENGTH = .100 # S
  MAX_GAP_SAMPLES = MAX_GAP / (1/SAMPLE_RATE)

  sample_rate_fine <- as.numeric(lubridate::round_date(
    lubridate::origin + lubridate::seconds(1/SAMPLE_RATE), ".000001 sec"))
  sample_rate_rounded <- as.numeric(lubridate::round_date(
    lubridate::origin + lubridate::seconds(1/SAMPLE_RATE), ".001 sec"))

  # sample data with missingness and heterogeneous timing
  test_data <- tsibble::as_tsibble(
    tibble(aoi = c(rep("target", 5), NA, rep("distractor", 5), "other"),
           t = c(lubridate::origin +
                   lubridate::seconds(1/30)*(1:10),
                 lubridate::origin +
                   lubridate::seconds(1/30)*14,
                 lubridate::origin +
                   lubridate::seconds(1/30)*24)),
    index = t, regular = FALSE) %>%
    mutate(t_num = as.numeric(t))

  # round to fine sample rate
  # collapse observations within those samples
  # fill gaps - because of rounding issues, creates duplicates
  # round to coarse sample rate
  # recollapse observations
  # interpolate across gaps

  processed_data <- test_data %>%
    tsibble::index_by(
      t_regular = ~ lubridate::round_date(., paste0(sample_rate_fine," sec"))) %>%
    summarise(aoi = na_mode(aoi)) %>%
    tsibble::fill_gaps() %>%
    tsibble::index_by(
      t = ~ lubridate::round_date(., paste0(sample_rate_rounded," sec"))) %>%
    summarise(aoi = na_mode(aoi)) %>%
    mutate(aoi = zoo::na.locf(aoi, maxgap = MAX_GAP_SAMPLES),
           t = as.numeric(t)) %>%
    as_tibble()
}

na_mode <- function (x) {
  if (all(is.na(x))) {
    return(as.character(NA))
  } else {
    x_nona <- x[!is.na(x)]

    # https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode
    ux <- unique(x_nona)
    x_mode <- ux[which.max(tabulate(match(x_nona, ux)))]

    return(x_mode)
  }
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
