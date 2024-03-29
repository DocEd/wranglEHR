
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wranglEHR <a href='https://cc-hic.github.io/inspectEHR/'><img src='logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![Lifecycle
Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/)
[![R-CMD-check](https://github.com/DocEd/wranglEHR/workflows/R-CMD-check/badge.svg)](https://github.com/DocEd/wranglEHR/actions)
<!-- badges: end -->

## Overview

`wranglEHR` is a data wrangling and cleaning tool for CC-HIC. It is
designed to run against the CC-HIC EAV table structure (which at present
exists in PostgreSQL and SQLite flavours). We are about to undergo a
major rewrite to OHDSI CDM version 6, so this package will be in flux.
Please see the `R` vignettes for further details on how to use the
package to perform the most common tasks:

-   `extract_demographics()` produces a table for time invariant
    dataitems.
-   `extract_timevarying()` produces a table for longitudinal dataitems.
-   `clean()` cleans the above tables according to pre-defined
    standards.

This package is designed to work in concert with `inspectEHR` which
provides data quality evaluation for the CC-HIC.

## Installation

``` r
# install directly from github with
remotes::install_github("DocEd/wranglEHR")
library(wranglEHR)
```

## Usage

``` r
# Connect to the database (will use the internal test db)
ctn <- setup_dummy_db()

# Extract static variables. Rename on the fly.
dtb <- extract_demographics(
  connection = ctn,
  episode_ids = 1:10, # specify for episodes
  code_names = c("NIHR_HIC_ICU_0017", "NIHR_HIC_ICU_0019"),
  rename = c("height", "weight")
)

head(dtb)
#> # A tibble: 6 × 2
#>   episode_id height
#>        <int>  <dbl>
#> 1          1  2.34 
#> 2          2  2.01 
#> 3          3  4.00 
#> 4          4 -0.318
#> 5          5  2.44 
#> # … with 1 more row

# Extract time varying variables. Rename on the fly.
ltb <- extract_timevarying(
  ctn,
  episode_ids = 1:10,
  code_names = "NIHR_HIC_ICU_0108",
  rename = "hr")
#> 3e-04 hours to process
#> WEE! How sublime was that?!

head(ltb)
#> # A tibble: 6 × 3
#>   episode_id  time    hr
#>        <int> <dbl> <int>
#> 1          1     0    91
#> 2          1     1    78
#> 3          1     2   102
#> 4          1     3    94
#> 5          1     4    69
#> # … with 1 more row

# Pull out to any arbitrary temporal resolution and custom define the
# behaviour for information recorded at resolution higher than you are sampling.
# only extract the first 24 hours of data

ltb_2 <- extract_timevarying(
  ctn,
  episode_ids = 1:10,
  code_names = "NIHR_HIC_ICU_0108",
  rename = "hr",
  cadence = 2, # 1 row every 2 hours
  coalesce_rows = mean, # use mean to downsample to our 2 hour cadence
  time_boundaries = c(0, 24)
  )
#> 0.00026 hours to process
#> HUZZAH! How cat's meow was that?!

head(ltb_2)
#> # A tibble: 6 × 3
#>   episode_id  time    hr
#>        <int> <dbl> <dbl>
#> 1          1     0  84.5
#> 2          1     2 102  
#> 3          1     4  81.3
#> 4          1     6  80  
#> 5          1     8  80.3
#> # … with 1 more row

## Don't forget to turn the lights out as you leave.
DBI::dbDisconnect(ctn)
```

## Getting help

If you find a bug, please file a minimal reproducible example on
[github](https://github.com/DocEd/wranglEHR/issues).

------------------------------------------------------------------------

1.  <https://www.ohdsi.org/analytic-tools/achilles-for-data-characterization/>
2.  Kahn, Michael G.; Callahan, Tiffany J.; Barnard, Juliana; Bauck,
    Alan E.; Brown, Jeff; Davidson, Bruce N.; Estiri, Hossein; Goerg,
    Carsten; Holve, Erin; Johnson, Steven G.; Liaw, Siaw-Teng;
    Hamilton-Lopez, Marianne; Meeker, Daniella; Ong, Toan C.; Ryan,
    Patrick; Shang, Ning; Weiskopf, Nicole G.; Weng, Chunhua; Zozus,
    Meredith N.; and Schilling, Lisa (2016) “A Harmonized Data Quality
    Assessment Terminology and Framework for the Secondary Use of
    Electronic Health Record Data,” eGEMs (Generating Evidence & Methods
    to improve patient outcomes): Vol. 4: Iss. 1, Article 18.
