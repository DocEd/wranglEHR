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
#' @param rename a character vector of names you want to relabel CC-HIC codes
#'   as, or NULL (the default) if you do not want to relabel.
#'
#' @export
#'
#' @importFrom dplyr collect select mutate filter inner_join full_join if_else
#'   summarise_all select_if
#' @importFrom tidyr spread
#' @importFrom tibble as_tibble
#' @importFrom purrr reduce
#' @importFrom rlang !!! inform abort .data
#'
#' @return A tibble of 1d data
#' @examples
#' hic_codes <- "NIHR_HIC_ICU_0409"
#' new_labels <- "apache_score"
#' con <- setup_dummy_db()
#' dtb <- extract_demographics(
#'   connection = con,
#'   episode_ids = 1:10,
#'   code_names = hic_codes,
#'   rename = new_labels)
#' head(dtb)
#' DBI::dbDisconnect(con)
extract_demographics <- function(connection = NULL,
                                 episode_ids = NA_integer_,
                                 code_names = NA_character_,
                                 rename = NA_character_) {

  if (is.null(connection)) {
    abort("You must supply a database connection")
  }

  if (class(episode_ids) != "integer") {
    abort("`episode_ids` must be given as NULL (the default) or
       an integer vector of episode ids")
  }

  code_names <- unique(code_names)
  variables <- tbl(connection, "variables")
  events <- tbl(connection, "events")

  demographics <- variables %>%
    collect() %>%
    mutate(nas = variables %>%
      select(-c(.data$code_name, .data$long_name, .data$primary_column)) %>%
      collect() %>%
      as_tibble() %>%
      apply(1, function(x) sum(!is.na(x)))) %>%
    filter(.data$nas == 1) %>%
    select(.data$code_name, .data$primary_column)

  all_demographic_codes <- demographics$code_name
  non_demo_codes <- setdiff(code_names, all_demographic_codes)


  if (length(non_demo_codes) > 0) {
    abort(
      "You are trying to extract non-1d data.
      Consider using `extract_timevarying()`")
  }

  if (all(is.na(episode_ids))) {
    tb_base <- events %>%
      select(.data$episode_id, .data$code_name, .data$integer,
             .data$string, .data$real, .data$date, .data$time,
             .data$datetime) %>%
      filter(.data$code_name %in% code_names) %>%
      collect()
  } else {
    tb_base <- events %>%
      select(.data$episode_id, .data$code_name, .data$integer,
             .data$string, .data$real, .data$date, .data$time,
             .data$datetime) %>%
      filter(.data$code_name %in% code_names,
             .data$episode_id %in% episode_ids) %>%
      collect()
  }

  complete_fields <- tb_base %>%
    select(-c(.data$episode_id, .data$code_name)) %>%
    select_if(function(x) !(all(is.na(x)))) %>%
    names()

  tb_segments <- vector(mode = "list", length = length(complete_fields))

  for (i in seq_along(complete_fields)) {
    tb_segments[[i]] <- extract_demographics_helper(
      tb_base, complete_fields[i], demographics)
  }

  db_1 <- purrr::reduce(
    tb_segments,
    full_join,
    by = "episode_id",
    .init = tibble::tibble(episode_id = as.integer(NULL))
  )
  
  ## Need to modify column classes if pulling from SQLite
  date_codes <- demographics$code_name[demographics$primary_column == "date"]
  datetime_codes <- demographics$code_name[demographics$primary_column == "datetime"]
  time_codes <- demographics$code_name[demographics$primary_column == "time"]
    
  if (any(names(db_1) %in% date_codes)) {
    
    date_codes <- date_codes[which(date_codes %in% names(db_1))]
    
    db_1 <- db_1 %>%
      mutate(
        across(date_codes, function(x) {
          if (class(x) == "character") {
            return(as.Date(x))
          } else {
            return(x)
          }
        }))
    
  }
  
  if (any(names(db_1) %in% datetime_codes)) {
    
    datetime_codes <- datetime_codes[which(datetime_codes %in% names(db_1))]
  
    db_1 <- db_1 %>%
      mutate(
        across(datetime_codes, function(x) {
          if (class(x) == "character") {
            return(as.POSIXct(x))
          } else {
            return(x)
          }
        }))
    
  }
  
  if (any(names(db_1) %in% time_codes)) {
    
    time_codes <- time_codes[which(time_codes %in% names(db_1))]
    
    db_1 <- db_1 %>%
      mutate(
        across(time_codes, function(x) {
          if (class(x) == "character") {
            return(hms::as_hms(x))
          } else {
            return(x)
          }
        })
    )
    
  }

  if (!is.null(rename)) {
    replacement_names <- rename[match(names(db_1), code_names)]
    names(db_1) <- if_else(
      is.na(replacement_names), names(db_1), replacement_names)
  }

  if (is.null(rename)) {
    lookups <- tibble(codes = code_names,
                      names = code_names)
  } else {
    lookups <- tibble(codes = code_names,
                      names = rename)
  }

  attr(db_1, "lookups") <- lookups
  return(db_1)
}


#' @importFrom rlang .data !! enquo
#' @importFrom dplyr select inner_join filter
#' @importFrom tidyr spread
#' @importFrom magrittr %>%
extract_demographics_helper <- function(tb_base, col_name, demographics) {
  quo_column <- enquo(col_name)

  tb_base %>%
    select(.data$code_name, !!quo_column, .data$episode_id) %>%
    inner_join(
      demographics %>%
        filter(.data$primary_column == !!quo_column),
      by = "code_name"
    ) %>%
    select(-.data$primary_column) %>%
    spread(key = code_name, value = !!quo_column)
}
