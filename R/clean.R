#' Clean Table
#'
#' This function takes an extracted table from either [extract_demographics()]
#' or [extract_timevarying()] and returns the same table having dealt with out
#' of range values. Currently this will either modify the value to NA, or limit
#' the value at an appropriate max or min, depending upon the user decision.
#'
#' @param tbl a table extracted from the CC-HIC database
#' @param dq_ref a table with ranges to override the default settings. Must take
#'   the following columns:
#'
#'   - code_name: string vector with NIHR code name for the data item as it
#'     appears in your table.
#'   - ranges: the lower and upper bounds of a possible value represented as a
#'     string of the form "\[lower, upper\]"
#' @param action string vector lengh 1 of "NA" or "limit". Currently this
#'   applies the action to all variables in the table.
#'
#' @return a table of the same dimensions as `tbl` but with outlying values
#'   appropriately cleaned.
#'
#' @importFrom tidyr unnest
#' @export
#' @md
clean <- function(tbl = NULL, dq_ref = NULL, action = "NA") {
  tbl <- purrr::imap_dfc(tbl,
                         .f = cleaning_helper,
                         dq_ref = dq_ref,
                         action = action)
}

cleaning_helper <- function(col_vec, col_name,
                            dq_ref = NULL,
                            action = c("NA", "limit")) {

  action <- match.arg(action, c("NA", "limit"))

  if (is.null(dq_ref)) {
    dq <- commonEHR:::qref
  } else {
    dq <- dq_ref
  }

  # Check to see if the data item is present in the dq table.
  dataitem_check <- nrow(dq[dq$code_name == col_name,])
  type_change <- class(col_vec)

  if (dataitem_check != 1) {
    rlang::inform(
      paste0("Cannot clean for column name: ", col_name)
    )
    return(col_vec)
  }

  if (type_change == "integer") {
    change_to <- NA_integer_
  } else if (type_change == "numeric") {
    change_to <- NA_real_
  } else if (type_change == "character") {
    change_to <- NA_character_
  } else {
    rlang::abort("Methods are not yet defined for this data type")
  }

  ## Action on NA
  if (action == "NA") {

    before <- sum(is.na(col_vec))

    if (type_change == "character") {

      pos_vales <- dq %>%
        filter(.data$code_name == {{ col_name }} ) %>%
        select(.data$possible_values) %>%
        tidyr::unnest(cols = c(possible_values)) %>%
        pull()

      # Replace anything not in the set with an NA.
      col_vec <- if_else(col_vec %in% pos_vales, col_vec, change_to)
      after <- sum(is.na(col_vec))
    } else {

      rng <- dq %>%
        filter(.data$code_name == {{ col_name }} ) %>%
        select(.data$ranges) %>%
        pull()

      rng <- commonEHR::parse_range(rng)

      col_vec <- if_else(
        !rng$lower_lim(col_vec, rng$lims[1]) |
        !rng$upper_lim(col_vec, rng$lims[2]),
        change_to, col_vec)

      if (type_change == "integer") {
        col_vec <- as.integer(col_vec)
      }

    after <- sum(is.na(col_vec))

    }

  change <- after - before

  }

  # Action on Limits
  if (action == "limit") {
    before <- col_vec

    if (type_change == "character") {
      rlang::warn(
        paste0(
          "limit action is not defined for: ", col_name)
      )
    } else {

      rng <- dq %>%
        filter(.data$code_name == {{ col_name }} ) %>%
        select(.data$ranges) %>%
        pull()

      rng <- commonEHR::parse_range(rng)

      col_vec <- pmin(col_vec, rng$lims[2])
      col_vec <- pmax(col_vec, rng$lims[1])

      if (type_change == "integer") {
        col_vec <- as.integer(col_vec)
      }
    }

  change <- length(setdiff(before, col_vec))

  }

  rlang::inform(
    paste0("Finished cleaning: ", col_name, ". ", change, " elements modified")
  )
  return(col_vec)

}
