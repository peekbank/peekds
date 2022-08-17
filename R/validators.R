#' Check if a dataframe/table is compliant to peekbank json before database
#' import
#'
#' @param df_table the dataframe to be saved
#' @param table_type the type of dataframe, for the most updated table types
#'   specified by schema, please use function list_ds_tables()
#'
#' @return TRUE when the input data frame is compliant with json specification,
#'   such as having all the required columns, primary key field has unique
#'   values, etc.
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
    is_field_missing <- is.na(idx_tb) & !fieldoptions$null

    # step 0: check if this is a required field
    if (is_field_missing) {
      msg_new <- .msg("- Cannot locate required field: {fieldname}. Please add
                      the column into the {table_type} processed data file.")
      msg_error <- c(msg_error, msg_new)
      next
    }

    # step 1: check if values in primary_key and unique-option fields are unique
    content_tb <- df_table[, fieldname]

    if (is_primary | isTRUE(fieldoptions$unique)) {
      # first check if primary key is in integer forms and start from zero
      if (is_primary) {
        content_tb <- as.integer(content_tb)
        if (min(content_tb) != 0) {
          msg_new <- .msg("- Primary key field {fieldname} should start at 0.")
          msg_error <- c(msg_error, msg_new)
        }
      }
      # make sure primary key and unique fields have unique values
      is_unique <- length(content_tb) == length(unique(content_tb))
      if (!is_unique) {
        msg_new <- .msg("- The values in field {fieldname} are not unique.")
        msg_error <- c(msg_error, msg_new)
      }
      if (sum(is.na(content_tb)) > 0) {
        msg_new <- .msg("- Column {fieldname} cannot contain NA values.")
        msg_error <- c(msg_error, msg_new)
      }
    }

    # step 2: check if values are in the required type/format
    if (!fieldoptions$null & (fieldclass == "IntegerField" | fieldclass == "ForeignKey")) {
      is_type_valid <- is.integer(content_tb)
      if (!is_type_valid) {
        msg_new <- .msg("- Column {fieldname} should contain integers only.")
        msg_error <- c(msg_error, msg_new)
      }
    } else if (!fieldoptions$null & fieldclass == "CharField") {
      # numbers are allowed here as well since numbers can be converted into
      # chars
      is_type_valid <- is.character(content_tb) |
        (typeof(content_tb) == "integer")
      if (!is_type_valid) {
        msg_new <- .msg("- Column {fieldname} should contain characters only.")
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
          msg_new <- .msg("- Column {fieldname} should contain the following
                          values only: {paste(choices_json, collapse = ', ')}.")
          msg_error <- c(msg_error, msg_new)
        }
      }
    }

    # STEP 4.1:
    # if aoi/xy_timepoints table, then check if resampling was done
    if (table_type == "aoi_timepoints") {
      remainder <- unique(df_table$t_norm %% pkg_globals$SAMPLE_DURATION)
      if (remainder != 0) {
        msg_new <- .msg("- Column t_norm in table {table_type} is notsampled
                        at 40HZ.")
        msg_error <- c(msg_error, msg_new)
      }
    }
    if (table_type == "xy_timepoints") {
      remainder <- unique(df_table$t_norm %% pkg_globals$SAMPLE_DURATION)
      if (remainder != 0) {
        msg_new <- .msg("- Column t_norm in table {table_type} is not sampled
                        at 40HZ.")
        msg_error <- c(msg_error, msg_new)
      }
    }

    # STEP 4.2:
    # if subjects table, then check if native_language field was entered
    # correctly
    if (table_type == "subjects" && fieldname == "native_language") {
      language_list <- list_language_choices()

      # go through every native language in the subjects table, check if all the
      # language codes are in the allowed list from json file
      invalid_languages <- df_table %>%
        dplyr::mutate(
          row_number = 1:dplyr::n(),
          valid_language = .data$native_language %>%
            purrr::map_lgl(function(lang) {
              all(stringr::str_trim(stringr::str_split(lang, ",")[[1]]) %in%
                    language_list)
            })
        ) %>%
        dplyr::filter(!.data$valid_language)

      if (nrow(invalid_languages) != 0) {
        msg_new <- .msg("- The subjects' native languages in the following entry
                        row(s) {invalid_languages$row_number} do not belong in
                        the allowed language list in json. Please see function
                        list_language_choices().")
        msg_error <- c(msg_error, msg_new)
      }
    }
  }

  return(msg_error)
}

#' check all csv files against database schema for database import
#'
#' @param dir_csv the folder directory containing all the csv files, the path
#'   should end in "processed_data"
#' @param file_ext the default is ".csv"
#'
#' @return an empty string if all tables passed the validator; otherwise, the
#'   function returns a list of messages describing detailed issues that needs
#'   to be fixed
#'
#' @examples
#' \dontrun{
#' msg_error_all <- validate_for_db_import(dir_csv = "./processed_data")
#' }
#'
#' @export
validate_for_db_import <- function(dir_csv, file_ext = ".csv") {
  # check coding method
  coding_file <- file.path(dir_csv, paste0(table_type = "administrations",
                                           file_ext))
  if (file.exists(coding_file)) {
    coding_table <- utils::read.csv(coding_file)
    coding_method <- unique(coding_table[, "coding_method"])
  } else {
    stop("Cannot find required administrations file.")
  }

  # fetch the table list based on coding method
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
        msg_error <- .msg("The processed data file {table_type} failed to pass
                          the validator for database import with these error
                          messsages:\n {paste(msg_error, collapse = '\n')}")
        # cat(crayon::bgMagenta(msg_error), "\n")
        message(msg_error)
        msg_error_all <- c(msg_error_all, msg_error)
      } else {
        print(.msg("The processed data file {table_type} passed the
                   validator!"))
      }
    } else if (is_table_required(table_type, coding_method)) {
      warning(.msg("Cannot find required file: {file_csv}"))
    }
  }
  return(msg_error_all)
}
