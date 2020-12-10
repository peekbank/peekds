#' @importFrom dplyr "%>%"
#' @importFrom magrittr "%<>%"
#' @importFrom rlang .data
NULL

#' Get the mode (respecting NAs)
#'
#' @param x Array
na_mode <- function(x) {
  if (all(is.na(x))) {
    return(as.character(NA))
  } else {
    x_nona <- x[!is.na(x)]

    # https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode
    ux <- unique(x_nona)
    x_mode <- ux[which.max(tabulate(match(x_nona, ux)))]

    return(x_mode)
  }
}


#' Render the peekbank schema directly from github
#'
render_schema <- function(x) {
  schema <- jsonlite::fromJSON("https://raw.githubusercontent.com/langcog/peekbank/master/static/peekbank-schema.json")

  table_names <- schema[[2]]
  tables <- schema[[3]]

  for (i in 1:length(table_names)) {
    print(knitr::kable(as.data.frame(tables[[i]][,1:2])))
  }
}

#' Download specific peekbank dataset from OSF
#'
#' @param token Your OSF PAT, see https://osf.io/settings/tokens.
#' @param lab_dataset_id Specific ID occurring in the file hierarchy of the relevant OSF repo.
#' @param path Where you want it on your own machine. Will error if directory doesn't exist.
#' @param osf_address pr6wu for peekbank.
#' @export
get_raw_data <- function(lab_dataset_id, path = ".", osf_address = "pr6wu") {

    # get file list in the relevant raw data directory and download
  osfr::osf_retrieve_node(osf_address) %>%
    osfr::osf_ls_files(n_max = Inf) %>%
    dplyr::filter(name == lab_dataset_id) %>%
    osfr::osf_ls_files(n_max = Inf) %>%
    dplyr::filter(name == "raw_data") %>%
    osfr::osf_ls_files(n_max = Inf) %>%
    osfr::osf_download(path = path,
                 conflicts = "overwrite", verbose = TRUE, progress = TRUE)
}

#' Download peekbank processed dataset from OSF
#'
#' @param lab_dataset_id Specific ID occurring in the file hierarchy of the relevant OSF repo.
#' @param path Where you want it on your own machine. Will error if directory doesn't exist.
#' @param osf_address pr6wu for peekbank.
#' @export
get_processed_data <- function(lab_dataset_id, path = ".", osf_address = "pr6wu") {
  # check if path exists, if not, create path
  if (!file.exists(path)){
    dir.create(file.path(path), showWarnings = FALSE)
  }

  # get file list in the relevant raw data directory and download
  osfr::osf_retrieve_node(osf_address) %>%
    osfr::osf_ls_files(n_max = Inf) %>%
    dplyr::filter(name == lab_dataset_id) %>%
    osfr::osf_ls_files(n_max = Inf) %>%
    dplyr::filter(name == "processed_data") %>%
    osfr::osf_ls_files(n_max = Inf) %>%
    osfr::osf_download(path = path,
                       conflicts = "overwrite", verbose = TRUE, progress = TRUE)
}

#' Put processed data for specific peekbank dataset on OSF
#'
#' @param dataset_name Specific dataset name occurring in the file hierarchy of the relevant OSF repo.
#' @param path Where the data live on your own machine.
#' @param osf_address pr6wu for peekbank.
#' @export
put_processed_data <- function(token, dataset_name, path = ".",
                               osf_address = "pr6wu") {
  osf_auth(token = token)

  osfr::osf_retrieve_node(osf_address) %>%
    osfr::osf_ls_files(n_max = Inf) %>%
    dplyr::filter(name == dataset_name) %>%
    osfr::osf_ls_files(n_max = Inf) %>%
    dplyr::filter(name == "processed_data") %>%
    osf_upload(path = stringr::str_c(path,
                                     list.files(path = path, recursive = TRUE)),
               recurse = TRUE, conflicts = "overwrite", verbose = TRUE,
               progress = TRUE)
}
