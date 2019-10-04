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
