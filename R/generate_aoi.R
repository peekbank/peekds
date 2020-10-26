#' @importFrom dplyr "%>%"
#' @importFrom magrittr "%<>%"
#' @importFrom rlang .data
NULL

demo_resample <- function() {
  # check for xy_data, trials, aoa_coordinates
  # require
  rm(list = ls())
  #file_ext = '.csv'
  #setwd("")
  dir_datasets <- "testdataset" # local datasets dir
  lab_dataset_id <- "pomper_saffran_2016"
  dir_csv <- file.path(dir_datasets, lab_dataset_id, "processed_data")
  file_ext <- '.csv'
  table_type <- "aoi_timepoints"
  resample_times(dir_csv, table_type)
}

#' Resample times to be consistent across labs
#'
#' @param dir_csv processed csv folder directory
#' @param table_type table name, can only be "aoi_timepoints" or "xy_timepoints"
#'
#' @return df_out with resampled time, xy or aoi value rows
#' @export
resample_times <- function(dir_csv, table_type) {
  # set sample rates
  SAMPLE_RATE <<- 40 # Hz
  SAMPLE_DURATION <<- 1000/SAMPLE_RATE

  # read data from the csv file
  file_csv <- file.path(dir_csv, paste0(table_type, file_ext))

  if (file.exists(file_csv)) {
    # read in csv file and check if the data is valid
    df_table <- utils::read.csv(file_csv)
  } else {
    warning("Cannot find required file: ", file_csv)
  }

  # initialize the output df
  if (table_type == "aoi_timepoints") {
    x <- c("t_norm", "aoi", "trial_id", "administration_id")
  } else if (table_type == "xy_timepoints") {
    x <- c("t", "x", "y", "trial_id", "administration_id")
  } else {
    stop("Current resample function only processes aoi_timepoints/xy_timepoints type data.")
  }
  df_out <- data.frame(matrix(ncol = length(x), nrow = 0))
  colnames(df_out) <- x

  # Next, slice the df based on administration id and trial id, then go through every sub-df for resample process
  ad_list <- unique(df_table$administration_id)
  trial_list <- unique(df_table$trial_id)

  for (adidx in ad_list) {
    for (trialidx in trial_list) {
      # slicing df based on trial and admin id
      df_trial <- df_table[which(df_table$administration_id == adidx & df_table$trial_id == trialidx), ]
      # if empty, skip loop
      if(nrow(df_trial) < 1) {
        next()
      }
      # if not empty, starting resample process
      if (table_type == "aoi_timepoints") {
        t_origin <- df_trial$t_norm
        data_origin <- df_trial$aoi
        # create the new timestamps for resampling
        t_start <- min(t_origin) - (min(t_origin) %% SAMPLE_DURATION)
        t_resampled <- seq(from = t_start, to = max(t_origin), by = SAMPLE_DURATION)
        # exchange strings values with integers for resampling
        data_num <- dplyr::recode(data_origin, target = 1, distractor = 2, missing = 3)
        # start resampling with approxfun
        f <- approxfun(t_origin, data_num, method = "constant", rule = 2)
        data_resampled <- f(t_resampled) %>%
          dplyr::recode(., '1' = "target", '2' = "distractor", '3' = "missing")

        # adding back the columns to match schema
        df_resampled <- data.frame(
          t_norm = t_resampled,
          aoi = data_resampled,
          trial_id = trialidx,
          administration_id = adidx
        )
      } else if (table_type == "xy_timepoints") {
        t_origin <- df_trial$t
        x_origin <- df_trial$x
        y_origin <- df_trial$y
        # create the new timestamps for resampling
        t_start <- min(t_origin) - (min(t_origin) %% SAMPLE_DURATION)
        t_resampled <- seq(from = t_start, to = max(t_origin), by = SAMPLE_DURATION)
        # start resampling with approxfun
        fx <- approxfun(t_origin, x_origin, method = "linear", rule = 2)
        x_resampled <- fx(t_resampled)

        fy <- approxfun(t_origin, y_origin, method = "linear", rule = 2)
        y_resampled <- fy(t_resampled)

        # adding back the columns to match schema
        df_resampled <- data.frame(
          trial_id = trialidx,
          administration_id = adidx,
          t = t_resampled,
          x = x_resampled,
          y = y_resampled
        )
      }
      # adding the resampled rows of data to the output df
      df_out <- rbind(df_out, df_resampled)
    }
  } # end of loop

  # re-order the output dataframes by these columns: t_norm, administration_id, trial_id
  df_out <- dplyr::arrange(df_out, t_norm, administration_id, trial_id)

  # write the resampled df into a new csv
  file_resampled <- file.path(dir_csv, paste0(table_type, '_resampled', file_ext))
  write.csv(df_out, file = file_resampled)
}

#' Normalize time by the onset
#'
#' @param df df that has subject_id, dataset_id, trial_id and times
#'
#' @return df_out df that has the normalized times
#' @export
center_times <- function(df) {
  # center timestamp (0 POD)
  df_out <- df %>%
    dplyr::group_by(.data$administration_id, .data$trial_id, .data$dataset_id) %>%
    dplyr::mutate(t_trial = .data$t - .data$t[1],
                  t_zeroed = .data$t_trial - .data$point_of_disambiguation)
  return(df_out)
}


#' Resample times to be consistent across labs
#'
#' @param dir df that has subject_id, dataset_id, trial_id and times
#'
#' @return
#' @export
resample_times_obsolete <- function(df) {
  # set sample rates
  SAMPLE_RATE = 40 # Hz
  SAMPLE_DURATION = 1000/SAMPLE_RATE
  MAX_GAP_LENGTH = .100 # S
  MAX_GAP_SAMPLES = MAX_GAP_LENGTH / (1/SAMPLE_RATE)

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
#' @return
generate_aoi <- function(dir) {
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

  resample_times(xy_joined) %>%
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
        TRUE ~ "missing"),
      aoi = dplyr::case_when(
        side %in% c("left","right") & side == target_side ~ "target",
        side %in% c("left","right") & side != target_side ~ "distractor",
        TRUE ~ side # other or NA, which is same as side
      ))

  return(xy_joined)
}
