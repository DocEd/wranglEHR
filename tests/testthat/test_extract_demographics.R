context("Extract Demographics")

con <- setup_dummy_db()

hic_codes <- c("NIHR_HIC_ICU_0017", "NIHR_HIC_ICU_0411")
new_labels <- c("height", "start_date")

dtb <- extract_demographics(
  connection = con,
  code_names = hic_codes,
  rename = new_labels)

test_that("Table properties are correct", {
  expect_true(tibble::is_tibble(dtb))
  expect_identical(dtb$episode_id, unique(dtb$episode_id))
})

DBI::dbDisconnect(con)