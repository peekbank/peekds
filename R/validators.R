validate_for_aoi_conversion <- function(dir) {
  # check for xy_data, trials, aoa_coordinates
  # require
  rm(list = ls())
  library(dplyr)
  library(peekds)

}

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
    peekjson[which(peekjson$table == table_type), "fields"] %>%
    purrr::flatten()

  # add "_id" to all the foreign key field names
  # e.g. subject -> subject_id
  # mask_fkey <- fields_json$field_class == "ForeignKey"
  # colnames_json[mask_fkey] <- paste0(colnames_json[mask _fkey], "_id")
  return(fields_json)
}

#' Check if a dataframe/table is EtDS compliant before saving as csv or importing into database
#'
#' @param df_table the data frame to be saved
#' @param table_type the type of table, can be one of this six types:
#'                   xy_data, aoi_data, participants, trials, dataset, aoi_regions
#'
#' @return TRUE when the column names of this table are valid
#'
#' @examples
#' \dontrun{
#' is_valid <- validate_table(df_table = df_table, table_type = "xy_data")
#' }
#'
#' @export
validate_table <- function(df_table, table_type) {
  colnames_table <- colnames(df_table)

  fieldnames_json <- fields_json$field_name
  fieldclasses_json <- fields_json$field_class
  fieldoptions_json <- fields_json$options

  colnames_json <- get_json_fields(table_type = table_type)

  if (is.null(colnames_json)) {
    return(FALSE)
  }

  # check if all
  mask_valid <- colnames_json %in% colnames_table
  if (!all(mask_valid)) {
    stop("Cannot locate fields: ", paste0(colnames_json[!mask_valid], collapse = ", "),
            " in the table. Please add them into the ", table_type, "csv files.")
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' check the peekbank_column_info csv files against database schema
#'
#' @param path_csv the directory of csv file 'peekbank_column_info.csv'
#' @param file_ext the default is ".csv"
#'
#' @return TRUE only if all the csv files have valid columns
#'
#' @examples
#' \dontrun{
#' is_valid = validate_column_info(dir_csv = '.')
#' }
#'
#' @export
validate_column_info <- function(path_csv) {
  # get json file from github
  peekjson <- get_peekjson()
  # fetch the table list
  table_list <- list_ds_tables()
  # admin table is not required
  table_list <- table_list[table_list != "admin"];
  # read in the csv columns info file
  file_name <- "peekbank_column_info.csv"
  file_csv = file.path(dir_csv, file_name)

  if (file.exists(file_csv)) {
    # read in csv file and check if the data is valid
    column_info <- utils::read.csv(file_csv)
  } else {
  is_all_valid = FALSE
  stop("Cannot find file: ", file_csv)
}

  is_all_valid = TRUE

  for (table_type in table_list) {
    fields_json <- get_json_fields(table_type)
    rows_sel <- column_info[column_info$table == table_type, ]

    fieldnames_json <- fields_json$field_name
    fieldnames_csv <- rows_sel[, 'field_name']
  }
  return(is_all_valid)
}

#' check all csv files against database schema for database import
#'
#' @param dir_csv the folder directory containing all the csv files,
#'                the path should end in "processed_data"
#' @param file_ext the default is ".csv"
#'
#' @return TRUE only if all the csv files have valid columns
#'
#' @examples
#' \dontrun{
#' is_valid = validate_for_db_import(dir_csv = "smi_dataset/processed_data")
#' }
#'
#' @export
validate_for_db_import <- function(dataset_type, dir_csv, file_ext = '.csv') {
  # get json file from github
  peekjson <- get_peekjson()
  # fetch the table list
  table_list <- list_ds_tables(dataset_type)
  # admin table is not required
  table_list <- table_list[table_list != "admin"];
  is_all_valid = TRUE

  for (table_type in table_list) {
    file_csv = file.path(dir_csv, paste0(table_type, file_ext))
    if (file.exists(file_csv)) {
      # read in csv file and check if the data is valid
      df_table <- utils::read.csv(file_csv)
      is_valid <- validate_table(df_table, table_type)
      if (!is_valid) {
        is_all_valid = FALSE
        stop("The csv file '", table_type,
                "' does not have the right format for database import.")
      }
    } else {
      is_all_valid = FALSE
      stop("Cannot find file: ", file_csv)
    }
  }
  return(is_all_valid)
}
