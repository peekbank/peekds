#' @importFrom dplyr "%>%"
NULL

#' check the peekbank_column_info csv files against database schema
#'
#' @param path_csv the directory of csv file 'peekbank_column_info.csv'
#' @param file_ext the default is ".csv"
#'
#' @return TRUE only if all the csv files have valid columns
#'
#' @examples
#' \dontrun{
#' is_valid = validate_column_info(path_csv = '.')
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
  file_csv = file.path(path_csv, file_name)

  if (file.exists(file_csv)) {
    # read in csv file and check if the data is valid
    info_csv <- utils::read.csv(file_csv)
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
  msg_error <- c()
  colnames_table <- colnames(df_table)

  fields_json <- get_json_fields(table_type = table_type)
  fieldnames_json <- fields_json$field_name

  # start checking field/column one by one
  for (idx in 1:length(fieldnames_json)) {
    fieldname <- fieldnames_json[idx]
    fieldclass <- fields_json$field_class[idx]
    fieldoptions <- fields_json$options[idx, ]

    idx_tb <- match(fieldname, colnames_table)
    is_primary <- isTRUE(fieldoptions$primary_key)
    is_field_required <- is.na(idx_tb)

    # step 0: check if this is a required field
    if (is_field_required) {
      msg_new <- paste("\n\t-\tCannot locate required field:", fieldname,
           ". Please add the column into the ", table_type, "processed data file.")
      msg_error <- c(msg_error, msg_new)
      next()
    }

    # step 1: check if values in primary_key and unique-option fields are unique
    content_tb <- df_table[, fieldname]

    if (is_primary | isTRUE(fieldoptions$unique)) {
      if (is_primary) {
        content_tb <- as.integer(content_tb)
      }
      is_unique <- length(content_tb) == length(unique(content_tb))
      if (!is_unique) {
        msg_new <- paste("\n\t-\tThe values in field", fieldname, "are not unique.")
        msg_error <- c(msg_error, msg_new)
      }
      if(sum(is.na(content_tb)) > 0) {
        msg_new <- paste("\n\t-\tField ", fieldname, " cannot contain NA values.")
        msg_error <- c(msg_error, msg_new)
      }
    }

    # step 2: check if values are in the required type/format
    if (!fieldoptions$null & fieldclass == "IntegerField") {
      is_type_valid <- is.integer(content_tb)
      if (!is_type_valid) {
        msg_new <- paste("\n\t-\t", fieldname, " should contain integers only.")
        msg_error <- c(msg_error, msg_new)
      }
    } else if (!fieldoptions$null & fieldclass == "CharField") {
      # numbers are allowed here as well since numbers can be converted into chars
      is_type_valid <- is.character(content_tb) | (typeof(content_tb) == "integer")
      if (!is_type_valid) {
        msg_new <- paste("\n\t-\tField", fieldname, "should contain characters only.")
        msg_error <- c(msg_error, msg_new)
      }
    } else {
      # no required data type
      is_type_valid <- TRUE
    }

    # step 3: if word/label choices are required, check if the values are valid
    if ("choices" %in% colnames(fieldoptions)) {
      choices_json <- unique(unlist(fieldoptions$choices))

      if (is_type_valid & (length(choices_json) > 0)) {
        is_value_valid <- all(unique(content_tb) %in% choices_json)
        if (!is_value_valid) {
          msg_new <- paste("\n\t-\tField", fieldname, "should contain the following values only: ",
                           paste(choices_json, collapse=', '), ".")
          msg_error <- c(msg_error, msg_new)
        }
      }
    }

    # STEP 4.1:
    # if aoi/xy_timepoints table, then check if resampling was done
    if (table_type == "aoi_timepoints") {
      remainder <- unique(df_table$t_norm %% pkg_globals$SAMPLE_DURATION)
      if (remainder != 0) {
        msg_new <- paste("\n\t-\tField t_norm in table", table_type, "is not sampled at 40HZ.")
        msg_error <- c(msg_error, msg_new)
      }
    }
    if (table_type == "xy_timepoints") {
      remainder <- unique(df_table$t %% pkg_globals$SAMPLE_DURATION)
      if (remainder != 0) {
        msg_new <- paste("\n\t-\tField t in table", table_type, "is not sampled at 40HZ.")
        msg_error <- c(msg_error, msg_new)
      }
    }

    # STEP 4.2:
    # if subjects table, then check if native_language field was entered correctly
    if (table_type == "subjects" && fieldname == "native_language") {
      language_list <- list_language_choices()

      # go through every native language in the subjects table, check if all the langauge codes are in the allowed list from json file
      invalid_languages <- df_table %>%
        mutate(row_number = 1:n(),
               valid_language = map_lgl(native_language,
                                        function(lang) all(str_split(lang, ", ")[[1]] %in% language_list))) %>%
        filter(!valid_language)

      if (nrow(invalid_languages) != 0) {
        msg_new <- paste("\n\t-\tThe subjects' native languages in the following entry row(s)", invalid_languages$row_number,
                         "do not belong in the allowed language list in json. Please see function list_language_choices().")
        msg_error <- c(msg_error, msg_new)
      }
    }
  }

  return(msg_error)
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
#' msg_error_all <- validate_for_db_import(dir_csv = "./processed_data")
#' }
#'
#' @export
validate_for_db_import <- function(dir_csv, file_ext = '.csv') {
  # get json file from github
  peekjson <- get_peekjson()

  coding_file <- file.path(dir_csv, paste0(table_type = "administrations", file_ext))
  if (file.exists(coding_file)) {
    coding_table <- utils::read.csv(coding_file)
    coding_method <- unique(coding_table[, "coding_method"])
  } else {
    stop("ERROR: Cannot find required administrations file.")
  }

  # fetch the table list
  table_list <- list_ds_tables(coding_method)
  # admin table is not required
  # table_list <- table_list[table_list != "admin"];
  msg_error_all <- c()

  for (table_type in table_list) {
    file_csv <- file.path(dir_csv, paste0(table_type, file_ext))
    if (file.exists(file_csv)) {
      # read in csv file and check if the data is valid
      df_table <- utils::read.csv(file_csv)
      msg_error <- validate_table(df_table, table_type)
      if (!is.null(msg_error)) {
        warning("The processed data file ", table_type,
                " failed to pass the validator for database import with these error messsages:", msg_error)
        msg_error_all <- c(msg_error_all, msg_error)
      } else {
        print(paste("The processed data file", table_type, "passed the validator!"))
      }
    } else if (is_table_required(table_type, coding_method)){
      warning("Cannot find required file: ", file_csv)
    }
  }
  return(msg_error_all)
}
