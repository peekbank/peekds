#' @importFrom dplyr "%>%"
#' @importFrom magrittr "%<>%"
#' @importFrom rlang .data
NULL

# the tables required for every processed datasets
table_list <- c("xy_data", "aoi_data", "participants", "trials", "dataset", "aoi_regions")


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
    stop("'import_header.csv' file is required for processing raw data.\n")
  }

  ## STEP 0. read in raw datafiles and the column names
  df_header <- utils::read.csv(file=file_header, header=TRUE, sep=",")
  df_raw <- raw_data
  df_map <- df_header[
    which((df_header$format == raw_format) & (df_header$table == table_type)), ]
  if (nrow(df_map) == 0) {
    warning("User did not provide mapping columns between raw data and table ",
             table_type, " in 'import_scripts/import_header.csv'. Thus ",
             table_type, " table is not processed.\n")
    return(NULL)
  }
  colnames_raw <- colnames(df_raw)

  colnames_fetch <- as.vector(df_map[, "raw_column"])
  colnames_map <- as.vector(df_map[, "mapped_column"])

  ## create new data table with NA values
  df_table <- data.frame(
    matrix(
      ncol = length(colnames_map), nrow = nrow(df_raw)))
  colnames(df_table) <- colnames_map

  ## search through raw data table and find desired columns,
  ## if they did not exist, then just the column will be left with NA values
  for (i in 1:length(colnames_fetch)) {
    if (colnames_fetch[i] %in% colnames_raw) {
      df_table[i] = df_raw[colnames_fetch[i]]
    } else {
      cat(colnames_fetch[i], "does not exist in the raw", raw_format, "data file\n")
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

#' Process directory of raw data to xy data
#'
#' @param format One of "tobii", "smi", "eyelink"
#' @param dir Directory with raw data
#'
#' @export
#'
process_to_xy <- function(format, dir) {
  readers <- list(
    "tobii" = process_tobii,
    "smi" = process_smi,
    "eyelink" = process_eyelink
  )
  assertthat::assert_that(format %in% names(readers))
  reader <- readers[format]
  reader(dir)
}

#' Process tobii raw data
#'
#' @param dir_raw Directory with raw data
#'
#' @export
process_tobii <- function(dir_raw) {
  raw_format = "tobii"
  # read in raw data frame from file oi
  raw_data <- utils::read.table(file = dir_raw, sep = '\t', header = TRUE)
  # call mapping function to create the list of tables
  for (table in table_list) {
    df_table <- map_columns(
      raw_data = raw_data, raw_format = raw_format, table_type = table)
    save_table(df_procd = df_table, table_type = table)
  }
}


#' Process an individual smi raw data file
#'
#' @param x
#' @param dir
#' @param stims_to_remove_chars
#' @param stims_to_keep_chars
#' @param possible_delims
#' @param stimulus_coding
#'
#' @return
#' @export
#'
#' @examples
process_smi_file <- function(x, dir, stims_to_remove_chars=c(".avi"), stims_to_keep_chars=c("_"),possible_delims=c("\t",","),stimulus_coding="stim_column") {

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
