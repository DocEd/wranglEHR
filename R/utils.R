#' Setup Dummy DB
#' 
#' Creates an in-memory SQLite database for testing purposes
#'
#' @importFrom DBI dbConnect dbWriteTable
#' @importFrom RSQLite SQLite
#' @importFrom dplyr mutate
#'
#'
#' @return an SQLite in-memory database
#' @export
#' 
#' @examples
#' con <- setup_dummy_db()
#' res <- DBI::dbSendQuery(con, "SELECT * FROM episodes LIMIT 10")
#' DBI::dbFetch(res)
#' DBI::dbClearResult(res)
#' DBI::dbDisconnect(con)
setup_dummy_db <- function() {
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), "")
  
  events <- .events %>%
    dplyr::mutate(
      datetime = strftime(datetime),
      date = strftime(date, format = "%Y-%m-%d"),
      time = strftime(time, format = "%H:%M:%S")) %>%
    DBI::dbWriteTable(conn, "events", .)
  
  episodes <- .episodes %>%
    dplyr::mutate(
      start_date = strftime(start_date)
    ) %>%
    DBI::dbWriteTable(conn, "episodes", .)
  
  provenance <- .provenance %>%
    dplyr::mutate(
      date_created = strftime(date_created),
      date_parsed = strftime(date_parsed),
    ) %>%
    DBI::dbWriteTable(conn, "provenance", .)
  
  variables <- DBI::dbWriteTable(conn, "variables", .variables)
  
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
