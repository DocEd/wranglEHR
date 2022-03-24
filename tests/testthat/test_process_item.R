context("Extract Timevarying: Process item")

library(wranglEHR)

# Without metadata

df <- tibble::tibble(
  code_name = "NIHR_HIC_ICU_0108",
  string = NA_character_,
  string2 = NA_character_,
  string3 = NA_character_,
  datetime = seq.POSIXt(
    from = Sys.time(),
    to = Sys.time() + lubridate::days(10),
    length.out = 2*24*10),
  date = as.Date(NA),
  time = hms::as_hms(NA),
  real = NA_real_,
  integer = as.integer(rnorm(2*24*10, 100, 5)),
  integer2 = NA_integer_,
  episode_id = 1L,
  event_id = 1:(2*24*10)
)

params <- tibble::tibble(
  code_names = "NIHR_HIC_ICU_0108",
  short_names = "hr",
  coalesce_rows = c(dplyr::first)
)

dt <- process_item(
  df = df,
  var_name = "NIHR_HIC_ICU_0108",
  metadata = .variables,
  start_time = Sys.time(),
  cadence = 1,
  params = params)

test_that("Basic procedure completes", {
  expect_identical(names(dt), c("diff_time", "NIHR_HIC_ICU_0108"))
})

dt <- process_item(
  df = df,
  var_name = "NIHR_HIC_ICU_0108",
  metadata = .variables,
  start_time = Sys.time(),
  cadence = 0,
  params = params)

test_that("Unique times handled correctly", {
  expect_identical(names(dt), c("diff_time", "NIHR_HIC_ICU_0108"))
  expect_identical(nrow(dt), nrow(df))
})

dt <- process_item_timestamp(
  df = df,
  var_name = "NIHR_HIC_ICU_0108",
  metadata = .variables,
  params = params)

test_that("Time stamps handled correctly", {
  expect_identical(names(dt), c("time_stamp", "NIHR_HIC_ICU_0108"))
  expect_identical(class(dt$time_stamp), c("POSIXct", "POSIXt"))
  expect_identical(nrow(dt), nrow(df))
})

# With metadata

df <- tibble::tibble(
  code_name = "NIHR_HIC_ICU_0116",
  string = NA_character_,
  string2 = NA_character_,
  string3 = NA_character_,
  datetime = seq.POSIXt(
    from = Sys.time(),
    to = Sys.time() + lubridate::days(10),
    length.out = 2*24*10),
  date = as.Date(NA),
  time = hms::as_hms(NA),
  real = rnorm(2*24*10, 10, 2),
  integer = sample(c(1:5, NA_integer_), 2*24*10, TRUE),
  integer2 = NA_integer_,
  episode_id = 1L,
  event_id = 1:(2*24*10)
)

params <- tibble::tibble(
  code_names = "NIHR_HIC_ICU_0116",
  short_names = "cvp",
  coalesce_rows = c(dplyr::first)
)

dt <- process_item(
  df = df,
  var_name = "NIHR_HIC_ICU_0116",
  metadata = .variables,
  start_time = Sys.time(),
  cadence = 1,
  params = params)

test_that("Basic procedure completes", {
  expect_identical(names(dt), c("diff_time", paste0("NIHR_HIC_ICU_0116_", 0:5)))
})


dt <- process_item(
  df = df,
  var_name = "NIHR_HIC_ICU_0116",
  metadata = .variables,
  start_time = Sys.time(),
  cadence = 0,
  params = params)

test_that("Basic procedure completes", {
  expect_identical(names(dt), c("diff_time", paste0("NIHR_HIC_ICU_0116_", 0:5)))
  expect_identical(nrow(dt), nrow(df))
})

dt <- process_item_timestamp(
  df = df,
  var_name = "NIHR_HIC_ICU_0116",
  metadata = .variables,
  params = params)


test_that("Time stamps handled correctly", {
  expect_identical(names(dt), c("time_stamp", paste0("NIHR_HIC_ICU_0116_", 0:5)))
  expect_identical(class(dt$time_stamp), c("POSIXct", "POSIXt"))
  expect_identical(nrow(dt), nrow(df))
})

