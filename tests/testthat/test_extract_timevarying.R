context("Extract Timevarying")

library(wranglEHR)

df <- extract_timevarying(debug = TRUE)

test_that("Table properties are correct", {
  expect_identical(class(df), c("tbl_df", "tbl", "data.frame"))
  expect_identical(dim(df), c(1540L, 6L))
  expect_true(all(
    names(df) %in%
    c("time", "NIHR_HIC_ICU_0108", "NIHR_HIC_ICU_0116_1",
      "NIHR_HIC_ICU_0116_3", "NIHR_HIC_ICU_0126", "episode_id")))
})

df2 <- extract_timevarying(debug = TRUE, cadance = 24)

test_that("Coalese rows works correctly", {
  expect_identical(class(df2), c("tbl_df", "tbl", "data.frame"))
  expect_identical(dim(df2), c(140L, 6L))
  expect_true(all(
    names(df2) %in%
      c("time", "NIHR_HIC_ICU_0108", "NIHR_HIC_ICU_0116_1",
        "NIHR_HIC_ICU_0116_3", "NIHR_HIC_ICU_0126", "episode_id")))
})
