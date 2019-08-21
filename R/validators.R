# load libraries
library(jsonlite)

validate_for_aoi_conversion <- function(dir) {
  # check for xy_data, trials, aoa_coordinates
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
get_peekjson <- function() {
  url_json <- "https://raw.githubusercontent.com/langcog/peekbank/master/static/peekbank-schema.json"
  peekjson <- fromJSON(url(url_json))
  return(peekjson)
}

#' Title
#'
#' @param table_type 
#'
#' @return
#' @export
#'
#' @examples
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
    flatten()
  colnames_json <- fields_json$field_name
  return(colnames_json)
}

#' Title
#'
#' @param df_table 
#' @param table_type 
#'
#' @return TRUE when the column names of this table are valid
#' @export
#'
#' @examples
validate_table <- function(df_table, table_type) {
  colnames_table <- colnames(df_table)
  colnames_json <- get_json_colnames(table_type = table_type)
  
  if (is.null(colnames_json)) {
    return(FALSE)
  }
  
  mask_vali <- colnames_json %in% colnames_table
  if (!all(mask_vali)) {
    warning("Cannot locate fields: ", paste0(colnames_json[!mask_vali], collapse = ", "), 
            " in the table.")
    return(FALSE)
  } else {
    return(TRUE)
  }
}

# check all csv files against database schema
#' Title
#'
#' @param dir_csv 
#' @param file_ext 
#'
#' @return
#' @export
#'
#' @examples
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
      df_table <- read.csv(file_csv)
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
