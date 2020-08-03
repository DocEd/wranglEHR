#' Extract 1d data from CC-HIC
#'
#' Takes a remote database contection to CC-HIC, a vector of HIC codes (and
#' optionally a vector of labels to rename the codes) and returns a table with 1
#' row per patient and 1 column per data item.
#'
#' @param connection a CC-HIC database connection
#' @param episode_ids an integer vector of episode ids from the CC-HIC DB
#'   that you want to extact. The default (NULL) is to extract all.
#' @param code_names a character vector of CC-HIC codes
#' @param rename_as a character vector of names you want to relabel CC-HIC codes
#'   as, or NULL (the default) if you do not want to relabel.
#'
#' @export
#'
#' @importFrom dplyr collect select mutate filter inner_join full_join if_else
#'   summarise_all select_if tbl
#' @importFrom tidyr pivot_wider
#' @importFrom tibble as_tibble
#' @importFrom purrr reduce
#' @importFrom rlang !!! inform abort warn .data
#'
#' @return A tibble of 1d data
#' @examples
#' db_pth <- system.file("testdata/synthetic_db.sqlite3", package = "inspectEHR")
#' ctn <- connect(sqlite_file = db_pth)
#' hic_codes <- "NIHR_HIC_ICU_0409"
#' new_labels <- "apache_score"
#' extract_demographics(ctn, episode_ids = 13639:13643, hic_codes, new_labels)
#' DBI::dbDisconnect(ctn)
extract_demographics <- function(connection = NULL, episode_ids = NULL,
                                 code_names = as.character(NA),
                                 rename_as = as.character(NA),
                                 debug = FALSE) {

  if (is.null(connection)) {
    if (debug == TRUE) {
      rlang::inform("Starting debugging mode")
      episode_ids <- episodes$episode_id
    } else {
      rlang::abort("no connection supplied")
    }
  }

  if (debug == FALSE) {
    variables <- tbl(connection, "variables")
    events <- tbl(connection, "events")
  }


  if (!is.null(episode_ids) & class(episode_ids) != "integer") {
    rlang::abort(
      "`episode_ids` must be given as NULL (the default) or an
      integer vector of episode ids")
  }

  if (any(!is.na(rename_as))) {
    renaming_len <- length(rename_as) == length(code_names)
    if (!renaming_len) {
      rlang::abort(
        "when renaming variables, you must supply equal length character vectors
        for `code_names` and `rename_as`"
      )
    }
  }

  demographics <- variables %>%
    collect() %>%
    mutate(nas = variables %>%
      select(-.data$code_name, -.data$long_name, -.data$primary_column) %>%
      collect() %>%
      as_tibble() %>%
      apply(1, function(x) sum(!is.na(x)))) %>%
    filter(.data$nas == 1) %>%
    select(.data$code_name, .data$primary_column)

  all_demographic_codes <- demographics$code_name

  if (all(is.na(code_names))) {
    extract_codes <- all_demographic_codes
  } else {
    extract_codes <- all_demographic_codes[all_demographic_codes %in% code_names]
  }

  if (length(unique(code_names)) > length(all_demographic_codes)) {
    rlang::warn(
      "It looks like you are trying to extract non-demographic data.
      Consider using `extract_timevarying()`")
  }

  if (is.null(episode_ids)) {
    tb_base <- events %>%
      select(.data$episode_id, .data$code_name, .data$integer,
             .data$string, .data$real, .data$date, .data$time,
             .data$datetime) %>%
      filter(.data$code_name %in% extract_codes) %>%
      collect()
  } else {
    tb_base <- events %>%
      select(.data$episode_id, .data$code_name, .data$integer,
             .data$string, .data$real, .data$date, .data$time,
             .data$datetime) %>%
      filter(.data$code_name %in% extract_codes,
             .data$episode_id %in% episode_ids) %>%
      collect()
  }

  complete_fields <- tb_base %>%
    select(-.data$episode_id, -.data$code_name) %>%
    dplyr::select_if(~ any(!is.na(.))) %>%
    names()

  tb_segments <- complete_fields %>%
    map(~ extract_demographics_helper(
      tb_base,
      .x,
      demographics))

  db_1 <- reduce(
    tb_segments,
    full_join,
    by = "episode_id"
  )

  if (any(!is.na(rename_as))) {
    replacement_names <- rename[match(names(db_1), code_names)]
    names(db_1) <- if_else(
      is.na(replacement_names), names(db_1), replacement_names)
  }

  if (any(!is.na(rename_as))) {
    lookups <- tibble(codes = code_names,
                      names = code_names)
  } else {
    lookups <- tibble(codes = code_names,
                      names = rename_as)
  }

  attr(db_1, "lookups") <- lookups
  return(db_1)
}


#' @importFrom rlang .data
#' @importFrom dplyr select inner_join filter
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr `%>%`
extract_demographics_helper <- function(tb_base, col_name, demographics) {

    tb_base %>%
      select(.data$code_name, .data$episode_id, col_name) %>%
      inner_join(
        demographics %>%
          filter(.data$primary_column == col_name),
        by = "code_name"
      ) %>%
      select(-.data$primary_column) %>%
      pivot_wider(names_from = "code_name", values_from = col_name)

}
