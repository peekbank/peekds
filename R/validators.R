validate_for_aoi_conversion <- function(dir) {
  # check for xy_data, trials, aoa_coordinates
}

#' get json file from peekbank github
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

#' Fetching the list of column names in each table according to the json file
#'
#' @param table_type the type of table, can be one of this six types:
#'                   xy_data, aoi_data, participants, trials, dataset, aoi_regions
#'
#' @return colnames_json -- the list of column names
#'
#' @examples
#' \dontrun{
#' colnames_json <- get_json_colnames(table_type = "aoi_data")
#' }
#'
#' @export
get_json_colnames <- function(table_type) {
  # get json file from github
  peekjson <- get_peekjson()
  # fetch the table list
  table_list <- as.vector(peekjson[, "table"])

  # check if the input table_type is valid
  if (!(table_type %in% table_list)) {
    warning("Cannot recognize the table type ", table_type, ".")
    return(NULL)
  }

  # get the list of column names in json
  fields_json <-
    peekjson[which(peekjson$table == table_type), "fields"] %>%
    jsonlite::flatten()
  colnames_json <- fields_json$field_name
  return(colnames_json)
}

#' Check if the table is EtDS compliant before saving as csv or importing into database
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
  colnames_json <- get_json_colnames(table_type = table_type)

  if (is.null(colnames_json)) {
    return(FALSE)
  }

  # check if all
  mask_valid <- colnames_json %in% colnames_table
  if (!all(mask_valid)) {
    warning("Cannot locate fields: ", paste0(colnames_json[!mask_valid], collapse = ", "),
            " in the table.")
    return(FALSE)
  } else {
    return(TRUE)
  }
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
validate_for_db_import <- function(dir_csv, file_ext = '.csv') {
  # get json file from github
  peekjson <- get_peekjson()
  # fetch the table list
  table_list <- as.vector(peekjson[, "table"])
  is_all_valid = TRUE

  for (table_type in table_list) {
    file_csv = file.path(dir_csv, paste0(table_type, file_ext))
    if (file.exists(file_csv)) {
      # read in csv file and check if the data is valid
      df_table <- utils::read.csv(file_csv)
      is_valid <- validate_table(df_table, table_type)
      if (!is_valid) {
        warning("The csv file '", table_type,
                "' does not have the right format for database import.")
        is_all_valid = FALSE
      }
    } else {
      warning("Cannot find csv file: ", file_csv)
      is_all_valid = FALSE
    }
  }
  return(is_all_valid)
}
