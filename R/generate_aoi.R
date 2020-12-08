#' @importFrom dplyr "%>%"
#' @importFrom magrittr "%<>%"
#' @importFrom rlang .data
NULL

# set package globals in this way to avoid messing up the workspace of the user loading the package
pkg_globals <- new.env()
pkg_globals$SAMPLE_RATE <- 40 # Hz
pkg_globals$SAMPLE_DURATION <- 1000/pkg_globals$SAMPLE_RATE
pkg_globals$MAX_GAP_LENGTH <- .100 # S
pkg_globals$MAX_GAP_SAMPLES <- pkg_globals$MAX_GAP_LENGTH / (1/pkg_globals$SAMPLE_RATE)

# key private function to do resampling of aois within a single trial
# uses approxfun to resample
# because missingness is coded as an integer and interpolation is "constant" then
# no "gaps" between AOIs are filled. actually interpolating across blinks is left
# for a different function as this is a theory-laden decision.
resample_aoi_trial <- function(df_trial) {
  t_origin <- df_trial$t_norm
  data_origin <- df_trial$aoi
  trialidx <- df_trial$trial_id[1]
  adidx <- df_trial$administration_id[1]

  # create the new timestamps for resampling
  t_start <- min(t_origin) - (min(t_origin) %% pkg_globals$SAMPLE_DURATION)
  t_resampled <- seq(from = t_start, to = max(t_origin), by = pkg_globals$SAMPLE_DURATION)

  # exchange strings values with integers for resampling
  data_num <- dplyr::recode(data_origin, target = 1, distractor = 2, other = 3, missing = 4)

  # start resampling with approxfun
  f <- approxfun(t_origin, data_num, method = "constant", rule = 2)
  data_resampled <- f(t_resampled) %>%
    dplyr::recode(., '1' = "target", '2' = "distractor", '3' = "other", '4' = "missing")

  # adding back the columns to match schema
  dplyr::tibble(t_norm = t_resampled,
                aoi = data_resampled,
                trial_id = trialidx,
                administration_id = adidx)
}

# key private function to do resampling of aois within a single trial
# uses approxfun to resample
resample_xy_trial <- function(df_trial) {
  MISSING_CONST <- -10000

  t_origin <- df_trial$t
  x_origin <- df_trial$x
  y_origin <- df_trial$y

  # create the new timestamps for resampling
  t_start <- min(t_origin) - (min(t_origin) %% pkg_globals$SAMPLE_DURATION)
  t_resampled <- seq(from = t_start, to = max(t_origin), by = pkg_globals$SAMPLE_DURATION)

  # because of the behavior of approxfun, we need numerical values for missingness
  x_origin[is.na(x_origin)] <- MISSING_CONST
  y_origin[is.na(y_origin)] <- MISSING_CONST

  # resample
  x_resampled <- approx(x = t_origin, y = x_origin, xout = t_resampled, method = "constant")$y
  y_resampled <- approx(x = t_origin, y = y_origin, xout = t_resampled, method = "constant")$y

  # replace missing values
  x_resampled[x_resampled == MISSING_CONST] <- NA
  y_resampled[y_resampled == MISSING_CONST] <- NA

  # adding back the columns to match schema
  dplyr::tibble(xy_timepoint_id = (1:length(t_resampled)) - 1, # add IDs
                trial_id = df_trial$trial_id[1],
                administration_id = df_trial$administration_id[1],
                t = t_resampled,
                x = x_resampled,
                y = y_resampled)
}


#' This function resample times to be consistent across labs.
#'
#' Resampling is done by the following steps:
#'
#' 1. iterate through every trial for every administration
#'
#' 2. create desired timepoint sequence with equal spacing according to pre-specified SAMPLE_RATE parameter
#'
#' 3. use approxfun to interpolate given data points to align with desired timepoint sequence
#'     "constant" interpolation method is used for AOI timepoints;
#'     "linear" interpolation method is used for xy timepoints;
#'     for more details on approxfun, please see: https://stat.ethz.ch/R-manual/R-devel/library/stats/html/approxfun.html
#'
#' 4. after resampling, bind resampled dataframes back together and re-assign aoi_timepoint_id
#'
#' @param df_table to-be-resampled dataframe with t, aoi/xy values, trial_id and administration_id
#'
#' @param table_type table name, can only be "aoi_timepoints" or "xy_timepoints"
#'
#' @return df_out with resampled time, xy or aoi value rows
#'
#' #' @examples
#' \dontrun{
#' dir_datasets <- "testdataset" # local datasets dir
#' lab_dataset_id <- "pomper_saffran_2016"
#' dir_csv <- file.path(dir_datasets, lab_dataset_id, "processed_data")
#' table_type <- "aoi_timepoints"
#' file_csv <- file.path(dir_csv, paste0(table_type, '.csv'))
#' df_table <- utils::read.csv(file_csv)
#' df_resampled <- resample_times(df_table, table_type = "aoi_timepoints")
#' }
#'
#' @export
resample_times <- function(df_table, table_type) {
  if (table_type == "aoi_timepoints") {
    df_out <- df_table %>%
      mutate(admin_trial_id = paste(administration_id, trial_id, sep = "_")) %>%
      split(.$admin_trial_id) %>%
      map_df(resample_aoi_trial)
  } else if (table_type == "xy_timepoints") {
    df_out <- df_table %>%
      mutate(admin_trial_id = paste(administration_id, trial_id, sep = "_")) %>%
      split(.$admin_trial_id) %>%
      map_df(resample_xy_trial)
  }

  # write the resampled df into a new csv
  #readr::write_csv(pdf_out, path = paste0(dir_csv, file_resampled))
  return(df_out)
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


#' Add AOIs to an xy dataframe
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

#' Round times to our specified sample rate to be consistent across labs
#'
#' @param dir df that has subject_id, dataset_id, trial_id and times
#'
#' @return
#' @export
round_times <- function(df) {
  # set sample rates

  df %>% dplyr::group_by(.data$administration_id, .data$trial_id) %>%
  tidyr::nest() %>%
  dplyr::mutate(
    data = .data$data %>%
      purrr::map(function(df) {
        df_rounded <- df %>%
          dplyr::mutate(t_zeroed = round(pkg_globals$SAMPLE_DURATION * round(t_zeroed/pkg_globals$SAMPLE_DURATION)))

        t_resampled <- tibble::tibble(t_zeroed = round(seq(min(df_rounded$t_zeroed),
                                                           max(df_rounded$t_zeroed),
                                                           pkg_globals$SAMPLE_DURATION)))

        dplyr::left_join(t_resampled, df_rounded, by = "t_zeroed") %>%
          dplyr::group_by(t_zeroed)
      })) %>%
  tidyr::unnest(.data$data)
}


