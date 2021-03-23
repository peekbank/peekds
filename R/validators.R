#' check the peekbank_column_info csv files against database schema
#'
#' @param path_csv the directory of csv file "peekbank_column_info.csv"
#'
#' @return TRUE only if all the csv files have valid columns
#'
#' @examples
#' \dontrun{
#' is_valid = validate_column_info(path_csv = ".")
#' }
#'
#' @export
validate_column_info <- function(path_csv) {
  # get json file from github
  peekjson <- get_peekjson() # TODO: unused?
  # fetch the table list
  table_list <- list_ds_tables()
  # admin table is not required
  table_list <- table_list[table_list != "admin"];
  # read in the csv columns info file
  file_name <- "peekbank_column_info.csv"
  file_csv <- file.path(path_csv, file_name)

  if (file.exists(file_csv)) {
    # read in csv file and check if the data is valid
    info_csv <- utils::read.csv(file_csv) # TODO: unused?
  } else {
    is_all_valid <- FALSE
    stop(.msg("Cannot find file: {file_csv}"))
  }

  is_all_valid <- TRUE

  for (table_type in table_list) {
    fields_json <- get_json_fields(table_type)
    column_info <- NULL  # TODO: added because column_info wasn't defined
    rows_sel <- column_info[column_info$table == table_type, ]

    fieldnames_json <- fields_json$field_name # TODO: unused?
    fieldnames_csv <- rows_sel[, "field_name"] # TODO: unused?
  }
  return(is_all_valid)
}

#' Check if a dataframe/table is EtDS compliant before saving as csv or
#' importing into database
#'
#' @param df_table the data frame to be saved
#' @param table_type the type of table, can be one of this six types: xy_data,
#'   aoi_data, participants, trials, dataset, aoi_regions
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
    if (!fieldoptions$null & fieldclass == "IntegerField") {
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

#' Generate time course plots after all the processed tables are validated
#'
#' @param dir_csv the folder directory containing all the csv files,
#'                the path should end in "processed_data"
#' @param lab_dataset_id TODO
#' @param is_save TODO
#'
#' @return TODO
#'
#' @examples
#' \dontrun{
#' visualize_for_db_import(dir_csv = "./processed_data")
#' }
#'
#' @export
visualize_for_db_import <- function(dir_csv, lab_dataset_id, is_save = FALSE) {
  # first read in all the csv files
  aoi_data <- utils::read.csv(file.path(dir_csv, "aoi_timepoints.csv"))
  trials_data <- utils::read.csv(file.path(dir_csv, "trials.csv"))
  trial_types_data <- utils::read.csv(file.path(dir_csv, "trial_types.csv"))
  stimuli_data <- utils::read.csv(file.path(dir_csv, "stimuli.csv"))
  # aoi_data <- utils::read.csv(fs::path(dir_csv, "aoi_timepoints.csv"))
  # trials_data <- utils::read.csv(fs::path(dir_csv, "trials.csv"))
  # trial_types_data <- utils::read.csv(fs::path(dir_csv, "trial_types.csv"))
  # stimuli_data <- utils::read.csv(fs::path(dir_csv, "stimuli.csv"))

  # rename columns for distractor
  distractor_stimuli_data <- stimuli_data
  colnames(distractor_stimuli_data) <- paste("distractor_",
                                             colnames(stimuli_data), sep = "")

  #join to full dataset
  full_data <- aoi_data %>%
    dplyr::left_join(trials_data) %>%
    dplyr::left_join(trial_types_data) %>%
    dplyr::left_join(stimuli_data, by = c("target_id" = "stimulus_id",
                                          "dataset_id")) %>%
    dplyr::left_join(distractor_stimuli_data %>%
                       dplyr::select(-.data$distractor_dataset_id),
                     by = c("distractor_id" = "distractor_stimulus_id"))

  #dplyr::mutate aoi
  full_data <- full_data %>%
    dplyr::mutate(aoi_new = dplyr::case_when(
      aoi == "target" ~ 1,
      aoi == "distractor"~0,
      aoi == "missing"~ NaN
    )) %>%
    dplyr::mutate(aoi_new = ifelse(is.nan(.data$aoi_new), NA, .data$aoi_new))

  ##### summarize by subject (really: administrations) ####
  summarize_by_subj <- full_data %>%
    dplyr::group_by(.data$administration_id, .data$t_norm) %>%
    dplyr::summarize(N = sum(!is.na(.data$aoi_new)),
                     mean_accuracy = mean(.data$aoi_new, na.rm = TRUE))

  #### summarize across subjects ####
  summarize_across_subj <- summarize_by_subj %>%
    dplyr::group_by(.data$t_norm) %>%
    dplyr::summarize(N = sum(!is.na(.data$mean_accuracy)),
                     accuracy = mean(.data$mean_accuracy, na.rm = TRUE),
                     sd_accuracy = stats::sd(.data$mean_accuracy, na.rm = TRUE))

  # plot (remove data points where not a lot of subjects contributed, to avoid
  # discontinuities in the slope)
  g1 <- summarize_across_subj %>%
    dplyr::filter(.data$N > length(unique(full_data$administration_id)) / 3) %>%
    ggplot2::ggplot(ggplot2::aes(.data$t_norm, .data$accuracy)) +
    ggplot2::geom_line(data = dplyr::filter(summarize_by_subj, .data$N > 10),
                       ggplot2::aes(y = .data$mean_accuracy,
                                    color = as.factor(.data$administration_id),
                                    group = as.factor(.data$administration_id)),
                       alpha = 0.2) +
    ggplot2::geom_line() +
    ggplot2::geom_smooth(method = "gam", se = FALSE) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::geom_vline(xintercept = 300, linetype = "dotted") +
    ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed") +
    ggplot2::theme(legend.position = "none")
  print(g1)

  #### by condition plotting (only if applicable) ####

  ##### summarize by subject by condition ####
  summarize_by_subj_by_condition <- full_data %>%
    dplyr::group_by(.data$administration_id, .data$condition, .data$t_norm) %>%
    dplyr::summarize(N = sum(!is.na(.data$aoi_new)),
                     mean_accuracy = mean(.data$aoi_new, na.rm = TRUE))

  #### summarize across subjects ####
  summarize_across_subj_by_condition <- summarize_by_subj_by_condition %>%
    dplyr::group_by(.data$condition, .data$t_norm) %>%
    dplyr::summarize(N = sum(!is.na(.data$mean_accuracy)),
                     accuracy = mean(.data$mean_accuracy, na.rm = TRUE),
                     sd_accuracy = stats::sd(.data$mean_accuracy, na.rm = TRUE))

  g2 <- summarize_across_subj_by_condition %>%
    dplyr::filter(.data$N > length(unique(full_data$administration_id)) / 3) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$t_norm, y = .data$accuracy,
                                 color = .data$condition,
                                 group = .data$condition)) +
    ggplot2::geom_line() +
    ggplot2::geom_smooth(method = "gam", se = FALSE) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::geom_vline(xintercept = 300, linetype = "dotted") +
    ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed")
  # print(g2)

  if (is_save) {
    plot_name <- paste0(lab_dataset_id, "_profile.png")
    ggplot2::ggsave(plot_name)
  }
}

#' check all csv files against database schema for database import
#'
#' @param dir_csv the folder directory containing all the csv files,
#'                the path should end in "processed_data"
#' @param file_ext the default is ".csv"
#'
#' @return msg_error_all if not all tables passed the validator,
#'                the returned variable contains a list of messages
#'                about issues to be corrected
#'
#' @examples
#' \dontrun{
#' msg_error_all <- validate_for_db_import(dir_csv = "./processed_data")
#' }
#'
#' @export
validate_for_db_import <- function(dir_csv, file_ext = ".csv") {
  #, want_plots = FALSE) {

  # get json file from github
  peekjson <- get_peekjson() # TODO: unused?

  coding_file <- file.path(dir_csv, paste0(table_type = "administrations",
                                           file_ext))
  if (file.exists(coding_file)) {
    coding_table <- utils::read.csv(coding_file)
    coding_method <- unique(coding_table[, "coding_method"])
  } else {
    stop("Cannot find required administrations file.")
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

  # if (want_plots) {
  #
  # }
}
