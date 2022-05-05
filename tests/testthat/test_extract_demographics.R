context("Extract Demographics")

connection <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

events <- .events %>%
  mutate(
    datetime = strftime(datetime),
    date = strftime(date, format = "%Y-%m-%d"),
    time = strftime(time, format = "%H:%M:%S")) %>%
  DBI::dbWriteTable(connection, "events", .)

episodes <- .episodes %>%
  mutate(
    start_date = strftime(start_date)
  ) %>%
  DBI::dbWriteTable(connection, "episodes", .)

provenance <- .provenance %>%
  mutate(
    date_created = strftime(date_created),
    date_parsed = strftime(date_parsed),
  ) %>%
  DBI::dbWriteTable(connection, "provenance", .)

variables <- DBI::dbWriteTable(connection, "variables", .variables)

hic_codes <- c("NIHR_HIC_ICU_0017", "NIHR_HIC_ICU_0411")
new_labels <- c("height", "start_date")

dtb <- extract_demographics(
  connection = connection,
  #.debug = TRUE,
  code_names = hic_codes,
  rename = new_labels)

test_that("Table properties are correct", {
  expect_true(tibble::is_tibble(dtb))
  expect_identical(dtb$episode_id, unique(dtb$episode_id))
})

