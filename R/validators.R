#' @importFrom dplyr "%>%"
NULL

# demo function for running validator
demo_validator <- function() {
  # check for xy_data, trials, aoa_coordinates
  rm(list = ls())
  #setwd("")
  dir_datasets <- "testdataset" # local datasets dir
  datasets <- get_datasets()
  lab_dataset_id <- "reflook_v3"
  dir.create(file.path(dir_datasets, lab_dataset_id))
  dir_csv = file.path(dir_datasets, lab_dataset_id, "processed_data")
  get_processed_data(lab_dataset_id, path = dir_csv, osf_address = "pr6wu") # if you dont have the most updated version of processed_data
  msg_error_all <- validate_for_db_import(dir_csv)
}

# demo function for running visualization check
demo_vis <- function() {
  rm(list = ls())
  setwd("C:/Dropbox/_codes/peek/peekds")
  datasets <- get_datasets()
  dataset_list <- datasets$dataset_name
  file_ext <- '.csv'
  for (lab_dataset_id in dataset_list) {
    dir_csv <- file.path(dir_datasets, lab_dataset_id, "processed_data")

  }

  # get in all the tables
  datasets <- data.frame(
    dataset_id = 0,
    lab_dataset_id == lab_dataset_id
  )

  df_aoi <- utils::read.csv(file.path(dir_csv, paste0("aoi_timepoints_resampled", file_ext)))
  df_stimuli <- utils::read.csv(file.path(dir_csv, paste0("stimuli", file_ext)))
  df_trials <- utils::read.csv(file.path(dir_csv, paste0("trials", file_ext)))
  df_admin <- utils::read.csv(file.path(dir_csv, paste0("administrations", file_ext)))

  t_range <- c(-1000,3000)
  aoi_data_joined <- df_aoi %>%
    right_join(df_admin) %>%
    right_join(df_trials) %>%
    right_join(datasets) %>%
    mutate(stimulus_id = target_id) %>%
    right_join(df_stimuli) %>%
    filter(t_norm > t_range[1],
           t_norm < t_range[2])

  # get subject info
  subinfo <- aoi_data_joined %>%
    group_by(subject_id, dataset_id, lab_dataset_id, age) %>%
    summarise(trials = length(unique(trial_id)))

  subinfo %>%
    ggplot(aes(x = age, fill = lab_dataset_id)) +
    geom_histogram(binwidth = 3) +
    scale_fill_solarized(name = "Dataset") +
    xlab("Age (months)")

  # time series
  means <- aoi_data_joined %>%
    filter(age > 12, age <= 60) %>%
    mutate(age_binned = cut(age, seq(0,60,12))) %>%
    group_by(t_norm, dataset_name, age_binned, stimulus_novelty) %>%
    summarise(n = sum(aoi %in% c("target","distractor"), na.rm = TRUE),
              p = sum(aoi == "target", na.rm = TRUE),
              prop_looking = p / n,
              ci_lower = binom::binom.confint(p, n, method = "bayes")$lower,
              ci_upper = binom::binom.confint(p, n, method = "bayes")$upper)

  ggplot(means,
         aes(x = t_norm, y = prop_looking)) +
    geom_rect(xmin = t_range[1],
              xmax = t_range[2],
              ymin = 0,
              ymax = 1, fill = "gray", alpha = .1) +
    geom_line(aes(col = dataset_name)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper,
                    fill = dataset_name), alpha = .5) +
    facet_grid(age_binned~stimulus_novelty) +
    geom_hline(yintercept = .5, lty = 2) +
    geom_vline(xintercept = 0, lty = 2) +
    ylab("Proportion Target Looking") +
    xlab("Time (msec)") +
    theme_classic() +
    scale_color_solarized() +
    scale_fill_solarized()
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
    peekjson[which(peekjson$table == table_type), "fields"]
  fields_json <- fields_json[[1]]

  # add "_id" to all the foreign key field names
  # e.g. subject -> subject_id
  # mask_fkey <- fields_json$field_class == "ForeignKey"
  # colnames_json[mask_fkey] <- paste0(colnames_json[mask _fkey], "_id")
  return(fields_json)
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
    is_field_required <- is_primary | !fieldoptions$null

    # step 0: check if this is a required field
    if (is_field_required & is.na(idx_tb)) {
      msg_new <- paste("\n\t-\tCannot locate required field: ", fieldname,
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
        msg_new <- paste("\n\t-\tThe values in field ", fieldname, "are not unique.")
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
        msg_new <- paste("\n\t-\t ", fieldname, " should contain integers only.")
        msg_error <- c(msg_error, msg_new)
      }
    } else if (!fieldoptions$null & fieldclass == "CharField") {
      # numbers are allowed here as well since numbers can be converted into chars
      is_type_valid <- is.character(content_tb) | (typeof(content_tb) == "integer")
      if (!is_type_valid) {
        msg_new <- paste("\n\t-\tField ", fieldname, " should contain characters only.")
        msg_error <- c(msg_error, msg_new)
      }
    } else {
      # no required data type
      is_type_valid <- TRUE
    }

    # step 3: if word/label choices are required, check if the values are valid
    if ("choices" %in% colnames(fieldoptions)) {
      choices_json <- unique(purrr::flatten(fieldoptions$choices))

      if (is_type_valid & length(choices_json) > 0) {
        is_value_valid <- all(unique(content_tb) %in% choices_json)
        if (!is_value_valid) {
          msg_new <- paste("\n\t-\tField ", fieldname, " should contain the following values only: ",
                           paste(unlist(choices_json), collapse=', '), ".")
          msg_error <- c(msg_error, msg_new)
        }
      }
    }

    # if aoi/xy_timepoints table, then check if resampling was done
    if (table_type == "aoi_timepoints") {
      remainder <- unique(df_table$t_norm %% SAMPLE_DURATION)
      if (remainder != 0) {
        msg_new <- paste("\n\t-\tField t_norm in table ", table_type, " is not sampled at 40HZ.")
        msg_error <- c(msg_error, msg_new)
      }
    }
    if (table_type == "xy_timepoints") {
      remainder <- unique(df_table$t %% SAMPLE_DURATION)
      if (remainder != 0) {
        msg_new <- paste("\n\t-\tField t in table ", table_type, " is not sampled at 40HZ.")
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
  table_list <- peekjson$table
  table_list <- table_list[table_list != 'admin']
  # admin table is not required
  # table_list <- table_list[table_list != "admin"];
  msg_error_all <- c()

  for (table_type in table_list) {
    file_csv = file.path(dir_csv, paste0(table_type, file_ext))
    if (file.exists(file_csv)) {
      # read in csv file and check if the data is valid
      df_table <- utils::read.csv(file_csv)
      msg_error <- validate_table(df_table, table_type)
      if (!is.null(msg_error)) {
        warning("The processed data file '", table_type,
                "' failed to pass the validator for database import with these error messsages:", msg_error)
        msg_error_all <- c(msg_error_all, msg_error)
      } else {
        print(paste("The processed data file ", table_type, "passed the validator!"))
      }
    } else if (is_table_required(table_type, coding_method)){
      warning("Cannot find required file: ", file_csv)
    }
  }
  return(msg_error_all)
}
