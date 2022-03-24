#' Retrieve DB Tables
#'
#' @param connection a DBI database object
#'
#' @importFrom dplyr tbl
#' @importFrom purrr map
#' @importFrom DBI dbListTables
#'
#' @return a list with pointers to DB tables
#' @export
retrieve_tables <- function(connection) {
  if (missing(connection)) {
    stop("a connection must be provided")
  }
  all_tables <- dbListTables(connection)
  tbl_list <- map(all_tables, ~ dplyr::tbl(connection, .x))
  names(tbl_list) <- all_tables
  return(tbl_list)
}


round_any <- function (x, accuracy = 1) {
  round(x/accuracy) * accuracy
}
