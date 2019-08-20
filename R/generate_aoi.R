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


  ###### HOT MESS STARTS HERE #######
  # downsample to 30Hz
  foo <- tsibble::as_tsibble(
    tibble(aoi = c(rep("target", 5), NA, rep("distractor", 5), "other"),
           t = c(lubridate::origin +
                   lubridate::seconds(1/30)*(1:10),
                 lubridate::origin +
                   lubridate::seconds(1/30)*14,
                 lubridate::origin +
                   lubridate::seconds(1/30)*24)),
    index = t, regular = FALSE) %>%
    mutate(t_num = as.numeric(t))

  sample_rate <- as.numeric(lubridate::round_date(lubridate::origin + lubridate::seconds(1/30),
                                       ".000001 sec"))
  sample_rate_crude <- as.numeric(lubridate::round_date(lubridate::origin + lubridate::seconds(1/30),
                                                  ".001 sec"))

  bar <- foo %>%
    tsibble::index_by(
      t2 = ~ lubridate::round_date(., paste0(sample_rate," sec"))) %>%
    summarise(aoi2 = aoi[1]) %>%
    tsibble::fill_gaps()  %>%
    tsibble::index_by(
      t3 = ~ lubridate::round_date(., paste0(sample_rate_crude," sec"))) %>%
    summarise(aoi3 = na_mode(aoi2)) %>%
    tsibble::slide_dfr(~ fill_gap(aoa3), size = 4) %>%
    mutate(t_num = as.numeric(t3))


  # interpolate

  # write aoi_data
}

fill_gap <- function (x) {
  FIXME
}

na_mode <- function (x) {
  if (all(is.na(x))) {
    return(as.character(NA))
  } else {
    return(x[!is.na(x)][1])
  }
}

###### HOT MESS ENDS HERE #######

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
