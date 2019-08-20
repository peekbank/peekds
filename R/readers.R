# load libraries


# the tables required for every processed datasets
table_list <<- c("xy_data", "aoi_data", "participants", "trials", "dataset", "aoi_coordinates")

#' Function for map raw data columns to processed table columns
#'
#' @param raw_data raw dara frame
#' @param raw_format source of the eye-tracking data, e.g. "tobii"
#' @param table_type type of processed table, e.g. "xy_data" | "aoi_table"
#'
#' @return processed data frame with specified column names
#'
#' @examples
#' df_xy_data <- map_columns(raw_data = raw_data, raw_format = "tobii", table_type = "xy_data")
#' df_aoi_data <- map_columns(raw_data = raw_data, raw_format = "tobii", table_type = "aoi_data")
#' 
#' @author for debug or change, contact Linger -- linger.xt@gmail.com
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
  df_header <- read.csv(file=file_header, header=TRUE, sep=",")
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
  write.csv(df_procd, file.path(
    dir_procd, paste(table_type, ".csv", sep="")), row.names=FALSE)
}

#' Process directory of raw data to xy data
#'
#' @param format One of "tobii", "smi", "eyelink"
#' @param dir Director with raw data
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
#' @param dir Director with raw data
#'
#' @export
process_tobii <- function(dir_raw) {
  raw_format = "tobii"
  # read in raw data frame from file oi
  raw_data <- read.table(file = dir_raw, sep = '\t', header = TRUE)
  # call mapping function to create the list of tables
  for (table in table_list) {
    df_table <- map_columns(
      raw_data = raw_data, raw_format = raw_format, table_type = table)
    save_table(save_table = df_table, table_type = table)
  }
}

#' Process smi raw data
#'
#' @param dir Director with raw data
#'
#' @export
process_smi <- function(dir) {

}

#' Process eyelink raw data
#'
#' @param dir Director with raw data
#'
#' @export
process_eyelink <- function(dir) {

}

#' Generate aoi data from xy data
#'
#' @param dir Directory with xy data and metadata
#'
#' @export
generate_aoi <- function(dir) {
  validate_for_aoi_conversion(dir)
  # read in xy_data, trials, aoa_coordinates
  # assign aoa based on aoa_coordinates
  # find correct aoi based on trials
  # interpolate
  # center timestamp (0 POD)
  # downsample to 30Hz
  # write aoi_data
}

