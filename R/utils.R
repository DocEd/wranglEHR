#' Setip Dummy DB
#' 
#' Creates an in-memory SQLite database for testing purposes
#'
#'
#' @return an SQLite in-memory database
#' @export
setup_dummy_db <- function() {
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  
  events <- wranglEHR:::.events %>%
    dplyr::mutate(
      datetime = strftime(datetime),
      date = strftime(date, format = "%Y-%m-%d"),
      time = strftime(time, format = "%H:%M:%S")) %>%
    DBI::dbWriteTable(conn, "events", .)
  
  episodes <- wranglEHR:::.episodes %>%
    dplyr::mutate(
      start_date = strftime(start_date)
    ) %>%
    DBI::dbWriteTable(conn, "episodes", .)
  
  provenance <- wranglEHR:::.provenance %>%
    dplyr::mutate(
      date_created = strftime(date_created),
      date_parsed = strftime(date_parsed),
    ) %>%
    DBI::dbWriteTable(conn, "provenance", .)
  
  variables <- DBI::dbWriteTable(conn, "variables", wranglEHR:::.variables)
  
  return(conn)
  
}


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
