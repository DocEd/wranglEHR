#' Round any
#'
#' rounds a numeric value to any arbitrary degree of precision.
#' defaults to nearest whole integer
#'
#' @param x a numeric vector
#' @param accuracy a numeric value specifying the base for rounding
#'
#' @return a vector of the same length as \code{x} rounded to the defined accuracy
#'
#' @examples
#' round_any(c(1, 1.25, 1.5, 1.75, 2), accuracy = 0.5)
round_any <- function(x, accuracy = 1) {
  round(x / accuracy) * accuracy
}
