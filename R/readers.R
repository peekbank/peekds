#' @importFrom dplyr "%>%"
#' @importFrom magrittr "%<>%"
#' @importFrom rlang .data
NULL

#' parse json file from peekbank github into a dataframe
#'
#' @return peekjson -- the organized dataframe from json file
#'
#' @examples
#' \dontrun{
#' peekjson <- get_peekjson()
#' }
#'
#' @export
get_peekjson <- function() {
  url_json <- "https://raw.githubusercontent.com/langcog/peekbank/master/static/peekbank-schema.json"
  peekjson <- jsonlite::fromJSON(url(url_json))
  return(peekjson)
}

#' Fetching the list of field names and requirements in each table according to the peekds json file
#'
#' @param table_type the type of table, can be one of the following types:
#'                   aoi_timepoints, aoi_region_sets, administrations, subjects, trials, datasets, stimuli
#'
#' @return fieldnames -- the list of field names
#'
#' @examples
#' \dontrun{
#' fields_json <- get_json_fields(table_type = "aoi_timepoints")
#' }
#'
#' @export
get_json_fields <- function(table_type) {
  # get json file from github
  peekjson <- get_peekjson()
  # fetch the table listb =
  table_list <- as.vector(peekjson[, "table"])

  # check if the input table_type is valid
  if (!(table_type %in% table_list)) {
    warning("Cannot recognize the table type ", table_type, ".")
    return(NULL)
  }

  # get the list of column names in json
  fields_json <-
    peekjson[which(peekjson$table == table_type), "fields"]
  fields_json <- fields_json[[1]]

  # add "_id" to all the foreign key field names
  # e.g. subject -> subject_id
  # mask_fkey <- fields_json$field_class == "ForeignKey"
  # colnames_json[mask_fkey] <- paste0(colnames_json[mask _fkey], "_id")
  return(fields_json)
}

#' List the tables required for datasets with different coding method
#'
#' @param dataset_type character
#'
#' @return table_list
#'
#' @examples
#' \dontrun{
#' table_list <- list_ds_tables(coding_method = "manual gaze coding")
#' }
#'
#' @export
list_ds_tables <- function(coding_method = "eyetracking") {
  # get json file from github
  peekjson <- get_peekjson()
  table_list <- peekjson$table
  table_list_auto <- table_list[table_list != 'admin']

  table_list_manual <- table_list_auto[table_list_auto %in% c("xy_timepoints", "aoi_region_sets") == FALSE]

  if (coding_method == "eyetracking" | coding_method == "automated gaze coding") {
    table_list <- table_list_auto
  } else if (coding_method == "manual gaze coding" | coding_method == "preprocessed eyetracking") {
    table_list <- table_list_manual
  } else {
    stop("Invalid coding method type! The type can only be one of the following: ",
         paste0(methods_json, collapse = ", "), ".")
  }
  return(table_list)
}

#' Get coding method list from json file
#'
#' @return
#' @export
list_coding_methods <- function() {
  fields_json <- get_json_fields(table_type = "administrations")
  methods_json <- fields_json$options[fields_json$field_name == "coding_method", "choices"] %>%
    unlist() %>%
    unique()
  return(methods_json)
}

#' Is this table required?
#'
#' @param table_type character
#' @param dataset_type character
#'
#' @return
#'
#' @examples
#' \dontrun{
#' is_required <- is_table_required(table_type = "xy_timepoints", coding_method = "manual gaze coding")
#' }
#'
#' @export
is_table_required <- function(table_type, coding_method) {
  table_list <- list_ds_tables(coding_method)
  is_required <- table_type %in% table_list
  return(is_required)
}

#' List current allowed language choices for db import
#'
#' @return language_list
#'
#' @examples
#' \dontrun{
#' language_list <- list_language_choices()
#' }
#'
#' @export
list_language_choices <- function() {
  # get json file from github
  fields_json <- get_json_fields(table_type = "trial_types")
  idx <- match("full_phrase_language", fields_json$field_name)
  fieldoptions <- fields_json$options[idx, ]
  language_list <- unique(unlist(fieldoptions$choices))

  return(language_list)
}

#' Function for map raw data columns to processed table columns
#'
#' @param raw_data raw dara frame
#' @param raw_format source of the eye-tracking data, e.g. "tobii"
#' @param table_type type of processed table, e.g. "xy_data" | "aoi_table"
#'
#' @return processed data frame with specified column names
#'
#' @examples
#' \dontrun{
#' df_xy_data <- map_columns(raw_data = raw_data, raw_format = "tobii", table_type = "xy_data")
#' df_aoi_data <- map_columns(raw_data = raw_data, raw_format = "tobii", table_type = "aoi_data")
#' }
#'
#' @export
map_columns <- function(raw_data, raw_format, table_type) {
  # dir_mapping won't be an input parameter, instead the function by default
  # assumes that this file should be in import_scripts/import_header.csv
  file_header <- "import_header.csv"
  if (!file.exists(file_header)) {
    stop("'import_header.csv' file is required for calling map_columns function.\n")
  }

  ## STEP 0. read in raw datafiles and the column names
  df_header <- utils::read.csv(file=file_header, header=TRUE, sep=",")
  df_map <- df_header[
    which((df_header$format == raw_format) & (df_header$table == table_type)), ]
  if (nrow(df_map) == 0) {
    stop("User did not provide mapping columns between raw data and table ",
             table_type, " in 'import_scripts/import_header.csv'. Thus ",
             table_type, " table is not processed.\n")
  }
  colnames_raw <- colnames(raw_data)

  colnames_fetch <- as.vector(df_map[, "raw_column"])
  colnames_map <- as.vector(df_map[, "mapped_column"])

  ## create new data table with NA values
  df_table <- data.frame(
    matrix(
      ncol = length(colnames_map), nrow = nrow(raw_data)))
  colnames(df_table) <- colnames_map

  ## search through raw data table and find desired columns,
  ## if they did not exist, then just the column will be left with NA values
  for (i in 1:length(colnames_fetch)) {
    if (colnames_fetch[i] %in% colnames_raw) {
      df_table[i] = raw_data[colnames_fetch[i]]
    } else if (colnames_fetch[i] == "") {
      stop("The raw data column to be mapped to ",
              colnames_map[i], " is not specified.")
    }
      else {
      stop("Cannot find column ", colnames_fetch[i], " in ",
              raw_format, " raw data file.")
    }
  }

  return(df_table)
}


#' supporting function for saving processed table as csv files in processed_data/
#'
#' @param df_procd processed data frame
#' @param table_type type of table to be saved
#'
#' @return null
#'
#' @export
save_table <- function(df_procd, table_type) {
  # because of the standardized data structure, processed csvs need to be under
  # "[dataset_name]/processed_data"
  dir_procd <- "../processed_data"

  # save processed data table in csv file
  # create dir for saving processed data if this directory does not exist
  if (!file.exists(dir_procd)) {
    dir.create(dir_procd)
  }
  utils::write.csv(df_procd, file.path(
    dir_procd, paste(table_type, ".csv", sep="")), row.names=FALSE)
}

# filter_eyetracking
# average
# universal function for converting orgin coordinates

#' Process directory of raw data to xy data
#'
#' @param format One of "tobii", "smi", "eyelink"
#' @param dir Directory with raw data
#'
#' @export
#'
process_to_xy <- function(format, dir) {
  # can be changed to process_smi_xy or for any dataset that needs to average
  # xy data in order to xy_data table
  # 1. average
  # 2. filter out of range data

  readers <- list(
    "tobii" = process_tobii,
    "smi" = process_smi,
    "eyelink" = process_eyelink
  )
  assertthat::assert_that(format %in% names(readers))
  reader <- readers[format]
  reader(dir)
}

#' Create empty table
#'
#' @param table_type character
#'
#' @return
#' @export
create_emtpy_table <- function(table_type) {
  # fetch the required columns from json file
  colnames_json <- get_json_colnames(table_type)
  # create emtpy df
  df_table <- utils::read.csv(text = paste0(colnames_json, collapse = ","))
  # add NA values
  df_table[1,] = c(0, rep(NA, length(colnames_json)-1))
  return(df_table)
}


#' Process tobii raw data
#' dataset specific: datasets
#'
#' @param dir_raw Directory with raw data
#'
#' @export
process_tobii <- function(dir_raw, dataset_name = "sample_data", dataset_type = "automated") {
  raw_format <- "tobii"
  # list all the files under the directory
  file_raw <- list.files(path = dir_raw,
                         pattern = '*data*.tsv',
                         all.files = FALSE)

  # read in raw data frame from file oi
  raw_data <- utils::read.table(file = file.path(dir_raw, file_raw),
                                sep = '\t',
                                header = TRUE)

  # Start processing table by table type
  ######## DATASETS ########
  table_type <- "datasets"
  if (is_table_required(table_type, dataset_type)) {
    ## [monitor_size_x], [monitor_size_y]
    # get the unique monitor sizes
    monitor_size_str <- raw_data[["RecordingResolution"]] %>%
                    unique()  %>%
                    na.omit() %>%
                    as.vector()
    # when multiple monitor size, just pick one
    if (length(monitor_size_str) > 1) {
      monitor_size_str <- monitor_size_str[1]
      warning("Dataset ", dataset_name, " has multiple monitor sizes, ",
              "only ", monitor_size_str, " will be saved.")
    }
    # extract numeric values from list
    monitor_size <- regmatches(monitor_size_str,
                               gregexpr("[[:digit:]]+", monitor_size_str))
    monitor_size <- monitor_size[[1]]
    monitor_size_x <- as.numeric(monitor_size[1])
    monitor_size_y <- as.numeric(monitor_size[2])

    ## [sample_rate]
    sample_timestamp <- raw_data[["RecordingTimestamp"]] %>%
                        as.vector()
    sample_steps <- sample_timestamp[-1] - sample_timestamp[-length(sample_timestamp)]
    sample_rate <- 1000 / mean(sample_steps)

    df_datasets <- data.frame(dataset_id = 0,
                              monitor_size_x = monitor_size_x,
                              monitor_size_y = monitor_size_y,
                              sample_rate = sample_rate,
                              tracker = "tobii",
                              lab_dataset_id = dataset_name)

    # validate against json, if valid, then save csv
    if (validate_table(df_datasets, table_type)) {
      save_table(df_datasets, table_type)
    } # no else error message, because that will be handled by validator.R
  }

  ######## AOI_REGIONS ########
  table_type <- "aoi_regions"
  if (is_table_required(table_type, dataset_type)) {
    # find the roi region file under dir_raw
    file_aoi <- list.files(path = dir_raw,
                           pattern = '*aoi*',
                           all.files = FALSE)

    if (length(file_aoi) != 0) {
      file_aoi <- file.path(dir_raw, file_aoi)
      has_aoi_info <- file.exists(file_aoi)
    } else {
      has_aoi_info = FALSE
    }
    if (has_aoi_info) {
      df_aoi <- utils::read.table(file_aoi, header = TRUE, sep = "")
      aoi_id <- seq(0, (nrow(df_aoi)-1))
      df_aoi[["aoi_region_id"]] <- c(aoi_id)
    } else {
      stop("Cannot find aoi_regions info file ", file_aoi, ".")
    }

    # validate against json, if valid, then save csv
    if (validate_table(df_aoi, table_type)) {
      save_table(df_aoi, table_type)
    } # no else error message, because that will be handled by validator.R
  }

  ######## TRIALS ########
  table_type <- "trials"
  if (is_table_required(table_type, dataset_type)) {
    # find the trial file under dir_raw
    file_trials <- list.files(path = dir_raw,
                           pattern = '*trial*',
                           all.files = FALSE)
    if (length(file_trials) != 0) {
      file_trials <- file.path(dir_raw, file_trials)
      has_trial_info <- file.exists(file_trials)
    } else {
      has_trial_info = FALSE
    }
    if (has_trial_info) {
      df_trials <- utils::read.table(file_trials, header = TRUE, sep = "")
      trial_id <- seq(0, (nrow(df_trials)-1))
      df_trials[["trial_id"]] <- c(trial_id)
    } else {
      stop("Cannot find trial_info file ", file_trials, ".")
    }

    # validate against json, if valid, then save csv
    if (validate_table(df_trials, table_type)) {
      save_table(df_trials, table_type)
    } # no else error message, because that will be handled by validator.R
  }

  ######## TRIAL TYPES ########
  table_type <- "trial_types"
  if (is_table_required(table_type, dataset_type)) {
    # find the trial file under dir_raw
    file_trial_types <- list.files(path = dir_raw,
                              pattern = '*trial_type*',
                              all.files = FALSE)
    if (length(file_trials) != 0) {
      file_trials <- file.path(dir_raw, file_trial_types)
      has_trial_type_info <- file.exists(file_trial_types)
    } else {
      has_trial_type_info = FALSE
    }
    if (has_trial_type_info) {
      df_trials <- utils::read.table(file_trial_types, header = TRUE, sep = "")
      trial_id <- seq(0, (nrow(df_trials)-1))
      df_trials[["trial_id"]] <- c(trial_id)
    } else {
      stop("Cannot find trial_type file ", file_trial_types, ".")
    }

    # validate against json, if valid, then save csv
    if (validate_table(df_trials, table_type)) {
      save_table(df_trials, table_type)
    } # no else error message, because that will be handled by validator.R
  }

  ######## SUBJECTS ########
  table_type <- "subjects"
  if (is_table_required(table_type, dataset_type)) {
    # fetch relavant columns from raw data file
    df_subjects <- map_columns(raw_data, raw_format, table_type)

    # get the unique subjects in the raw table
    # + generate primary key for subjects table
    df_subjects <- dplyr::distinct(df_subjects)
    subject_id <- seq(0, (nrow(df_subjects)-1))
    df_subjects[["subject_id"]] <- c(subject_id)

    # validate against json, if valid, then save csv
    if (validate_table(df_subjects, table_type)) {
      save_table(df_subjects, table_type)
    } # no else error message, because that will be handled by validator.R
  }

  ######## XY_DATA ########
  table_type <- "xy_data"
  if (is_table_required(table_type, dataset_type)) {
    # fetch relavant columns from raw data file
    df_xy <- map_columns(raw_data, raw_format, table_type)

    # replace 'subject_id', 'trial_id' to unique integer keys
    xy_data_id <- seq(0, (nrow(df_xy)-1))
    df_xy[["xy_data_id"]] <- c(xy_data_id)

    # validate against json, if valid, then save csv
    if (validate_table(df_xy, table_type)) {
      save_table(df_xy, table_type)
    } # no else error message, because that will be handled by validator.R
  }

  ######## AOI_DATA ########
  table_type <- "aoi_data"
  if (is_table_required(table_type, dataset_type)) {
    # fetch relavant columns from raw data file
    df_aoi <- map_columns(raw_data, raw_format, table_type)

    # replace 'subject_id', 'trial_id' to unique integer keys
    aoi_data_id <- seq(0, (nrow(df_aoi)-1))
    df_aoi[["aoi_data_id"]] <- c(aoi_data_id)

    # validate against json, if valid, then save csv
    if (validate_table(df_aoi, table_type)) {
      save_table(df_aoi, table_type)
    } # no else error message, because that will be handled by validator.R
  }
}


#' Process an individual smi raw data file
#'
#' @param x character, filename
#' @param dir character, directory
#' @param stims_to_remove_chars character, e.g. ".avi"
#' @param stims_to_keep_chars character, e.g. "_"
#' @param possible_delims character vector
#' @param stimulus_coding character
#'
#' @return
#' @export
process_smi_file <- function(x, dir, stims_to_remove_chars = c(".avi"),
                             stims_to_keep_chars = c("_"),
                             possible_delims = c("\t",","),
                             stimulus_coding = "stim_column") {

  #general parameters
  max_lines_search <- 40 #comments from initial header of smi eyetracking file
  subid_name <- "Subject"
  monitor_size <- "Calibration Area"
  sample_rate <- "Sample Rate"
  left_x_col_name = "L POR X [px]"
  right_x_col_name = "R POR X [px]"
  left_y_col_name = "L POR Y [px]"
  right_y_col_name = "R POR Y [px]"

  #create file path
  file_path <- paste0(dir,"/",x)

  #guess delimiter
  sep <- reader::get.delim(file_path, comment="#", delims=possible_delims,skip = max_lines_search)

  #extract information about subject, monitor size, and sample rate from header
  subject_id <- readr::read_lines(file_path, n_max=max_lines_search) %>%
    stringr::str_subset(subid_name) %>%
    stringr::str_extract(paste("(?<=",subid_name,":\\t).*",sep="")) %>%
    trimws()

  monitor_size <- readr::read_lines(file_path, n_max=max_lines_search) %>%
    stringr::str_subset(monitor_size) %>%
    stringr::str_extract(paste("(?<=",monitor_size,":\\t).*",sep="")) %>%
    trimws() %>%
    stringr::str_replace("\t", "x")

  sample_rate <- readr::read_lines(file_path, n_max=max_lines_search) %>%
    stringr::str_subset(sample_rate) %>%
    stringr::str_extract(paste("(?<=",sample_rate,":\\t).*",sep="")) %>%
    trimws()

  # get maximum x-y coordinates on screen
  screen_xy <- stringr::str_split(monitor_size,"x") %>%
    unlist()
  x.max <- as.numeric(as.character(screen_xy[1]))
  y.max <- as.numeric(as.character(screen_xy[2]))

  #read in data
  data <-
    readr::read_delim(
      file_path,
      comment="##",
      delim=sep
    )

  #rename some columns

  #select and rename columns
  data <-  data %>%
    dplyr::rename(
      raw_t = "Time",
      lx = .data$left_x_col_name,
      rx = .data$right_x_col_name,
      ly = .data$left_y_col_name,
      ry = .data$right_y_col_name
    )

  #select rows for xy file
  data <- data %>%
    dplyr::filter(.data$Type=="SMP", #remove anything that isn't actually collecting ET data
                  .data$Stimulus != "-", #remove calibration
                  !grepl(paste(stims_to_remove_chars,collapse="|"), .data$Stimulus),  #remove anything that isn't actually a trial based on specific characters
                  grepl(paste(stims_to_keep_chars,collapse="|"), .data$Stimulus)) #from here, keep only trials fitting desired patters;

  # add sub_id column (extracted from data file)
  data <- data %>%
    dplyr::mutate(subject_id=.data$subject_id)

  #### General eyetracking data processing

  ## Remove out of range looks
  data <-
    data %>%
    dplyr::mutate(
      rx = dplyr::if_else(.data$rx <= 0 | .data$rx >= x.max, NA_real_, .data$rx),
      lx = dplyr::if_else(.data$lx <= 0 | .data$lx >= x.max, NA_real_, .data$lx),
      ry = dplyr::if_else(.data$ry <= 0 | .data$ry >= y.max, NA_real_, .data$ry),
      ly = dplyr::if_else(.data$ly <= 0 | .data$ly >= y.max, NA_real_, .data$ly)
    )

  ## Average left-right x-y coordinates
  #Take one eye's measurements if we only have one; otherwise average them
  data <-
    data %>%
    dplyr::mutate(
      x = dplyr::case_when(
        is.na(rx) & !is.na(lx) ~ lx,
        !is.na(rx) & is.na(lx) ~ rx,
        !is.na(rx) & !is.na(lx) ~ (rx + lx) / 2,
        is.na(rx) & is.na(lx) ~ NA_real_
      ),
      y = dplyr::case_when(
        is.na(ry) & !is.na(ly) ~ ly,
        !is.na(ry) & is.na(ly) ~ ry,
        !is.na(ry) & !is.na(ly) ~ (ry + ly) / 2,
        is.na(ry) & is.na(ly) ~ NA_real_
      )
    ) %>%
    dplyr::select(
      -.data$rx, -.data$ry, -.data$lx, -.data$ly
    )

  ## Convert time into ms starting from 0
  data <- data %>%
    dplyr::mutate(
      timestamp = round((.data$raw_t - .data$raw_t[1])/1000, 3)
    )

  ## Redefine coordinate origin (0,0)
  # SMI starts from top left
  # Here we convert the origin of the x,y coordinate to be bottom left (by "reversing" y-coordinate origin)
  data <- data %>%
    dplyr::mutate(
      y = y.max - .data$y
    )

  ##If trials are identified via a Stimulus column, determine trials and redefine time based on trial onsets
  if (stimulus_coding == "stim_column") {

    # Redefine trials based on stimuli rather than SMI output
    #check if previous stimulus value is equal to current value; ifelse, trial test increases by 1
    data <- data %>%
      dplyr::mutate(stim_lag = dplyr::lag(.data$Stimulus),
                    temp = ifelse(.data$Stimulus != .data$stim_lag, 1, 0),
                    temp_id = cumsum(c(0, .data$temp[!is.na(.data$temp)])),
                    trial_id = 1+.data$temp_id)

    #set time to zero at the beginning of each trial
    data <- data %>%
      dplyr::group_by(.data$trial_id) %>%
      dplyr::mutate(t = .data$timestamp - min(.data$timestamp))
  }

  #extract final columns
  xy.data <- data %>%
    dplyr::select(.data$sub_id,.data$x,.data$y,.data$t,.data$trial_id, .data$Stimulus)

  ##Make dataset table
  dataset.data <- data.frame(id = "refword",
                             tracker = "SMI",
                             monitor_size = monitor_size,
                             sample_rate = sample_rate)

  return(list(xy = xy.data, dataset = dataset.data))

}


#' Process smi raw data
#'
#' @param dir Directory with raw data
#' @param file_ext
#'
#' @export
process_smi <- function(dir, file_ext = '.txt') {

  #list files in directory
  all_files <- list.files(path = dir,
                                 pattern = paste0('*',file_ext),
                                 all.files = FALSE)

  #process individual smi files
  all_data <- lapply(all_files,process_smi_file,dir=dir)

  #extract specific datasets from the processed data
  xy_data <- all_data %>%
    purrr::map("xy") %>%
    dplyr::bind_rows()

  dataset_data <- all_data %>%
    purrr::map("dataset") %>%
    dplyr::bind_rows()

  #save data
  save_table(xy_data, table_type = "xy_data")
  save_table(dataset_data, table_type = "dataset")

}

#' Process eyelink raw data
#'
#' @param dir Directory with raw data
#'
#' @export
process_eyelink <- function(dir) {

}
