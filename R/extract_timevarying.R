#' Extract & Reshape Timevarying Dateitems
#'
#' This is the workhorse function of wranglEHR that transcribes 2d data from
#' CC-HIC to a table with 1 column per dataitem (and any metadata if relevant)
#' and 1 row per time per patient.
#'
#' The time unit is user definable, and set by the \code{cadence} argument. The
#' default behaviour is to produce a table with 1 row per hour per patient. If
#' there are duplicates/conflicts (e.g more than 1 event for a given hour), then
#' only the first result for that hour is returned.  If extracting at a lower
#' cadence than is naturally recorded in the database, one can specify a vector
#' of summary function to the \code{coalesce_rows} argument. These summary
#' functions must *always* return a vector of length 1, in the same data type
#' and must be able to handle vectors entirely of \code{NAs}.
#'
#' Many events inside CC-HIC occur on a greater than hourly basis. Depending
#' upon the chosen analysis, one may which to modify the cadence. 0.5 for
#' example will produce a table with 1 row per 30 minutes per patient.
#'
#' Choose what variables one wishes to extract wisely. This function is quite
#' efficient considering what it needs to do, but it can take a very long time
#' if extracting lots of data. It is a strong recommendation that the database
#' is optimised with indexes prior to using this function. It is sensible to
#' test the extraction with 100 or so patients before committing to a full
#' extraction.
#'
#' It is possible for this function to produce negative time rows (e.g. rows
#' that occurred prior to ICU admission). If, for example a patient had a
#' measure taken in the hours before they were admitted, then this would be
#' added to the table with a negative time value. As a concrete example, if a
#' patient had a sodium measured at 08:00, and they were admitted to the ICU at
#' 20:00 the same day, then the sodium would be displayed at time = -12. This is
#' normal behaviour and it is left to the end user to determine how best they
#' wish to account for this.
#'
#' @param connection a CC-HIC database connection.
#' @param episode_ids an integer vector of episode_ids or NULL. If NULL (the
#'   default) then all episodes are extracted.
#' @param code_names a string vector of CC-HIC codes names to be extracted.
#' @param rename a character vector, of the same length as \code{code_names},
#'   with names to relabel extracted CC-HIC dataitems, or NULL (the default) to
#'   retain the original code names. Given in the same order as
#'   \code{code_names}.
#' @param coalesce_rows a function vector of summary functions to summarise data
#'   that is contributed at a higher frequency than the set \code{cadence}. Must
#'   be the same length, and in the same order as \code{code_names}.
#' @param chunk_size an integer scalar. Chunks the extraction process by this
#'   many episodes to help manage memory constraints. The default (5000) works
#'   well for most desktop computers. If RAM is not a major limitation, setting
#'   this to \code{Inf} may improve performance.
#' @param cadence a numerical scalar >= 0 or the string "timestamp". If a
#'   numerical scalar is used, it will describe the base time unit to build each
#'   row of the extracted table, in divisions of an hour. For example: 1 = 1
#'   hour, 0.5 = 30 mins, 2 = 2 hourly. If cadence = "timestamp", then the
#'   precise datetime will be used to generate the time column. This is likely
#'   to generate a large table, so use cautiously.
#' @param time_boundaries a numeric vector of length 2 containing the start and
#'   end times (in hours) relative to the ICU admission time, for which the data
#'   extraction should occur. For example, \code{c(0, 24)} will return the first
#'   24 hours of data after admission. The default \code{c(-Inf, Inf)} will
#'   return all data.
#'
#' @return sparse tibble with an hourly cadence as rows, and unique data items
#'   as columns. Data items that contain metadata are reallocated to their own
#'   columns.
#'
#' @export
#'
#' @importFrom purrr map imap reduce
#' @importFrom lubridate hours ymd_hms
#' @importFrom rlang inform abort .data !!
#' @importFrom dplyr select distinct collect distinct_at first tbl bind_rows
#'   pull n arrange filter relocate
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#'
#' @examples
#' con <- setup_dummy_db()
#' ctn <- DBI::dbConnect(
#' df <- extract_timevarying(
#'   connection = con,
#'   episode_ids = 1:10,
#'   code_names = "NIHR_HIC_ICU_0108"
#'   )
#' head(df)
extract_timevarying <- function(connection = NULL,
                                episode_ids = NA_integer_,
                                code_names = NA_character_,
                                rename = NA_character_,
                                coalesce_rows = dplyr::first,
                                chunk_size = 5000,
                                cadence = 1,
                                time_boundaries = c(-Inf, Inf)) {

  if (is.null(connection)) {
    abort("You must supply a database connection")
  }

  events <- tbl(connection, "events")
  variables <- tbl(connection, "variables")

  starting <- Sys.time()

  if (class(episode_ids) != "integer") {
    abort("`episode_ids` must be given as NULL (the default) or
       an integer vector of episode ids")
  }

  cadence_pos_num <- class(cadence) == "numeric" && cadence >= 0
  cadence_timestamp <- cadence == "timestamp"

  if (!(cadence_pos_num || cadence_timestamp)) {
    abort("`cadence` must be given as a numeric scalar >= 0
       or the string 'timestamp'")
  }

  if (!(class(time_boundaries) == "numeric" & length(time_boundaries) == 2)) {
    abort("`time_boundaries` must be a numeric vector of length 2")
  }

  if (!(time_boundaries[1] == -Inf | time_boundaries[1] <= 0)) {
    abort("the first `time_boundaries` term must either a numeric scalar <= 0 or
          -Inf")
  }

  if (!(time_boundaries[2] == Inf | time_boundaries[2] >= 0)) {
    abort("the second `time_boundaries` term must either a numeric scalar >= 0 or
          Inf")
  }

  if (!all(code_names %in% .variables$code_name)) {
    abort("You are attempting to extract data items
          that do not exist in the CC-HIC data model")
  }

  if (!(any(code_names %in% "NIHR_HIC_ICU_0411"))) {
    # Add in the episode start datetime
    exons <- append(code_names, "NIHR_HIC_ICU_0411")
  } else {
    exons <- code_names
  }

  #
  params <- tibble(
    code_names = code_names,
    short_names = rename,
    coalesce_rows = c(coalesce_rows)
  )

  if (all(is.na(episode_ids))) {
    episode_groups <- events %>%
      distinct(.data$episode_id) %>%
      collect()
  } else {
    episode_groups <- events %>%
      filter(.data$episode_id %in% episode_ids) %>%
      distinct(.data$episode_id) %>%
      collect()
  }

  mdata <- collect(variables)

  episode_groups <- episode_groups %>%
    mutate(group = as.integer(seq(n()) / chunk_size)) %>%
    split(., .$group) %>%
    map(function(epi_ids) {
      collect_events <- events %>%
        filter(.data$code_name %in% exons,
               .data$episode_id %in% !! epi_ids$episode_id) %>%
        collect()

      map(collect_events %>%
        select(.data$episode_id) %>%
        distinct() %>%
        pull(), process_all,
      events = collect_events,
      metadata = mdata,
      cadence = cadence,
      params = params,
      time_boundaries = time_boundaries
      ) %>%
        bind_rows()
    }) %>%
    bind_rows()

  if (!all(is.na(rename))) {

    for (i in seq_len(nrow(params))) {
      names(episode_groups) <- gsub(
        pattern = params$code_names[i],
        replacement = params$short_names[i],
        x = names(episode_groups)
      )
    }

  }

  episode_groups <- relocate(episode_groups, c(episode_id, time))

  if (all(is.na(rename))) {
    lookups <- tibble(codes = code_names,
                      names = code_names)
  } else {
    lookups <- tibble(codes = code_names,
                      names = rename)
  }

  attr(episode_groups, "lookups") <- lookups
  attr(episode_groups, "cadence") <- cadence

  elapsed_time <- signif(
    as.numeric(
      difftime(
        Sys.time(), starting, units = "hour")), 2)
  inform(paste(elapsed_time, "hours to process"))

  if (requireNamespace("praise", quietly = TRUE)) {
    well_done <-
      praise::praise(
        "${EXCLAMATION}! How ${adjective} was that?!"
        )
    inform(well_done)
  }

  return(episode_groups)
}


#' Process all events
#'
#' @param epi_id an integer vector of episode_ids to process
#' @param events a collected CC-HIC events table in the EAV style
#' @param metadata a collected CC-HIC metadata table
#' @param cadence baseline time cadence for extraction
#' @param params a table containing exactly 3 columns called: code_name,
#'   short_name and coalesce_rows. The coalesce rows column should contain a
#'   summary function which is used when duplicate row level data is present.
#' @param time_boundaries time boundaries of the extraction with respect to ICU
#'   admission times
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter mutate select pull arrange full_join rename
#' @importFrom lubridate hours
#' @importFrom purrr imap reduce
#' @importFrom tibble tibble
process_all <- function(epi_id, events, metadata, cadence,
                        params, time_boundaries) {
  pt <- events %>%
    filter(episode_id == epi_id) %>%
    mutate(datetime = as.POSIXct(datetime))

  start_time <- pt %>%
    filter(code_name == "NIHR_HIC_ICU_0411") %>%
    select(datetime) %>%
    pull()

  if (time_boundaries[1] != -Inf) {
    pull_from <- start_time + hours(time_boundaries[1])
    pt <- filter(pt, datetime >= pull_from)
  }

  if (time_boundaries[2] != Inf) {
    pull_to <- start_time + hours(time_boundaries[2])
    pt <- filter(pt, datetime <= pull_to)
  }

  if (class(cadence) == "numeric") {
    imap(
      pt %>%
        filter(code_name %in% find_2d(metadata)$code_name) %>%
        arrange(code_name) %>%
        split(., .$code_name),
      process_item,
      metadata = metadata,
      start_time = start_time,
      cadence = cadence,
      params = params
      ) %>%
        reduce(
          full_join, by = "diff_time",
          .init = tibble(diff_time = as.numeric(NULL))) %>%
        rename(time = diff_time) %>%
        mutate(episode_id = epi_id) %>%
        arrange(time)
  } else {
    imap(
      pt %>%
        filter(code_name %in% find_2d(metadata)$code_name) %>%
        arrange(code_name) %>%
        split(., .$code_name),
      process_item_timestamp,
      metadata = metadata,
      params = params
      ) %>%
      reduce(full_join, by = "time_stamp",
             .init = tibble(time_stamp = lubridate::ymd_hms(NULL))) %>%
      rename(time = time_stamp) %>%
      mutate(episode_id = epi_id) %>%
      arrange(time)
  }
}


#' Process single item
#'
#' Takes a table in the EAV style for a single patient and a single data item
#' and transforms it into a tidy format
#'
#' @param df a data table with column names as for the EAV style database tables
#'   employed in CC-HIC. The table must contain data only for one dataitem (e.g.
#'   have only a single unique `code_name`) and for a single episode (e.g. have
#'   only a single unique `episode_id`)
#' @param var_name character string of length 1 containing the code_name to be
#'   processed
#' @param metadata a table containing metadata. Can be found within the package
#'   in the `.variables` object
#' @param start_time datetime of the episode start
#' @param cadence numeric scalar indicating the table base resolution
#' @param params a table containing exactly 3 columns called: code_name,
#'   short_name and coalesce_rows. The coalesce rows column should contain a
#'   summary function which is used when duplicate row level data is present.
#'
#' @importFrom magrittr %>% extract2
#' @importFrom dplyr filter select pull mutate distinct rename summarise if_else
#'   group_by across
#' @importFrom rlang !! !!! := .data
process_item <- function(df, var_name, metadata,
                         start_time, cadence, params) {

  stopifnot(!is.na(df$datetime))

  prim_col <- metadata %>%
    filter(code_name == var_name) %>%
    select(primary_column) %>%
    pull()

  meta_names <- find_2d_meta(metadata, var_name)

  summary_func <- params %>%
    filter(code_names == var_name) %>%
    select(coalesce_rows) %>%
    pull() %>%
    extract2(1)

  tb_a <- df %>%
    mutate(
      diff_time = as.numeric(difftime(datetime, start_time, units = "hours")))

  if (cadence > 0) {
    tb_a <- tb_a %>%
      mutate(diff_time = round_any(diff_time, cadence))
  }

  if (length(meta_names) == 0) {
    tb_a <- tb_a %>%
      group_by(diff_time) %>%
      summarise(!!var_name := summary_func(.data[[prim_col]]))
  } else {
    tb_a <- tb_a %>%
      rename(!!var_name := prim_col) %>%
      select(diff_time, !!var_name, !!!meta_names) %>%
      mutate(across(meta_names, function(x) {
            x <- as.character(x)
            if_else(is.na(x), "0", x)
          })) %>%
      # Split over the first meta_name
      split(.[meta_names[1]]) %>%
      map(function(x) {
            append_n <- distinct(x, across(meta_names)) %>%
              pull() %>%
              as.character()

            new_name <- paste(var_name, append_n, sep = "_")
            names(x) <- if_else(names(x) == var_name, new_name, names(x))

            select(x, -c(!!!meta_names)) %>%
              group_by(diff_time) %>%
              summarise(across(new_name, summary_func))
          }) %>%
        reduce(full_join, by = "diff_time",
               .init = tibble(diff_time = as.numeric(NULL)))
  }

  return(tb_a)
}

#' Process episode with a timestamp
#'
#' Process a single episode into a rectangular table with a timestamp column
#' instead of the usual difftime since admission. The timestamp column
#' corresponds to the exact timestamp of the event of interest. This is
#' particularly useful for when you need to combine episodes into spells. Take
#' care around times of clock change. This is similar to setting the `cadence`
#' argument to 0, however the time column returned is an actualy datetime stamp.
#'
#' @importFrom magrittr %>% extract2
#' @importFrom dplyr filter select pull mutate distinct rename summarise if_else
#'   group_by across
#' @importFrom rlang !! !!! := .data
#'
#' @param df a data table with column names as for the EAV style database tables
#'   employed in CC-HIC. The table must contain data only for one dataitem (e.g.
#'   have only a single unique `code_name`) and for a single episode (e.g. have
#'   only a single unique `episode_id`)
#' @param var_name character string of length 1 containing the code_name to be
#'   processed
#' @param metadata a table containing metadata. Can be found within the package
#'   in the `.variables` object
#' @param params a table containing exactly 3 columns called: code_name,
#'   short_name and coalesce_rows. The coalesce rows column should contain a
#'   summary function which is used when duplicate row level data is present.
process_item_timestamp <- function(df, var_name, metadata, params) {
  stopifnot(!is.na(df$datetime))

  prim_col <- metadata %>%
    filter(code_name == var_name) %>%
    select(primary_column) %>%
    pull()

  meta_names <- find_2d_meta(metadata, var_name)

  summary_func <- params %>%
    filter(code_names == var_name) %>%
    select(coalesce_rows) %>%
    pull() %>%
    extract2(1)

  tb_a <- rename(df, time_stamp = .data$datetime)

  if (length(meta_names) == 0) {
    tb_a <- tb_a %>%
      group_by(.data$time_stamp) %>%
      summarise(!!var_name := summary_func(.data[[prim_col]]))
  } else {
    tb_a <- tb_a %>%
      rename(!!var_name := prim_col) %>%
      select(.data$time_stamp, !!var_name, !!!meta_names) %>%
      mutate(across(meta_names, function(x) {
        x <- as.character(x)
        if_else(is.na(x), "0", x)
      })) %>%
      # Split over the first meta_name
      split(.[meta_names[1]]) %>%
      map(function(x) {
        append_n <- distinct(x, across(meta_names)) %>%
          pull() %>%
          as.character()

        new_name <- paste(var_name, append_n, sep = "_")
        names(x) <- if_else(names(x) == var_name, new_name, names(x))

        select(x, -c(!!!meta_names)) %>%
          group_by(.data$time_stamp) %>%
          summarise(across(.data[[new_name]], summary_func))
      }) %>%
      reduce(full_join, by = "time_stamp",
             .init = tibble(time_stamp = as.POSIXct(NULL)))
  }

  return(tb_a)
}

not_na <- function(x) {
  any(!is.na(x))
}

#' @importFrom dplyr mutate select collect
#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
find_2d <- function(metadata) {
  metadata %>%
    mutate(nas = metadata %>%
      select(-code_name, -long_name, -primary_column) %>%
      collect() %>%
      as_tibble() %>%
      apply(1, function(x) sum(!is.na(x)))) %>%
    filter(nas > 1) %>%
    select(code_name, primary_column)
}


#' @importFrom dplyr filter select pull select_if
#' @importFrom rlang !!
#' @importFrom magrittr %>%
find_2d_meta <- function(metadata, c_name) {
  select_row <- metadata %>%
    filter(code_name == c_name)

  prim_col <- select_row %>%
    select(primary_column) %>%
    pull()

  select_row %>%
    select(-code_name, -long_name, -primary_column, -datetime, -!!prim_col) %>%
    select_if(.predicate = not_na) %>%
    names()
}

#' Fill in 2d Table to make a Sparse Table
#'
#' \code{\link{extract_timevarying}} returns a non-sparse table (i.e. rows/hours
#' with no recorded information for a patient are not presented in the table)
#' This function serves to expand the table and fill missing rows with NAs. This
#' is useful when working with packages that expect a regular cadence to the
#' table.
#'
#' @param df a table returned from \code{\link{extract_timevarying}}
#'
#' @importFrom rlang abort .data
#' @importFrom dplyr select bind_rows left_join
#' @importFrom purrr imap
#' @importFrom tibble tibble
#'
#' @return a sparse table
#' @export
expand_missing <- function(df) {

  cadence <- attr(df, "cadence")

  if (cadence == "timestamp") {
    abort("this function is not designed to work on a table with timestamp data")
  }

  df %>%
    select(.data$episode_id, .data$time) %>%
    split(., .$episode_id) %>%
    imap(function(base_table, epi_id) {
      tibble(
        episode_id = as.numeric(epi_id),
        time = seq(
          min(base_table$time, 0),
          max(base_table$time, 0),
          by = cadence
        )
      )
    }) %>%
    bind_rows() %>%
    left_join(df, by = c("episode_id", "time"))
}
