% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean.R
\name{clean}
\alias{clean}
\title{Clean Table}
\usage{
clean(tbl = NULL, dq_ref = NULL, action = "NA")
}
\arguments{
\item{tbl}{a table extracted from the CC-HIC database}

\item{dq_ref}{a table with ranges to override the default settings. Must take
the following columns:
\itemize{
\item code_name: string vector with NIHR code name for the data item as it
appears in your table.
\item ranges: the lower and upper bounds of a possible value represented as a
string of the form "[lower, upper]"
}}

\item{action}{string vector lengh 1 of "NA" or "limit". Currently this
applies the action to all variables in the table.}
}
\value{
a table of the same dimensions as \code{tbl} but with outlying values
appropriately cleaned.
}
\description{
This function takes an extracted table from either \code{\link[=extract_demographics]{extract_demographics()}}
or \code{\link[=extract_timevarying]{extract_timevarying()}} and returns the same table having dealt with out
of range values. Currently this will either modify the value to NA, or limit
the value at an appropriate max or min, depending upon the user decision.
}
