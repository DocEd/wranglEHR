context("Extract Demographics")

library(wranglEHR)

df <- extract_demographics(debug = TRUE)

test_that("Table properties are correct", {
  expect_identical(class(df), c("tbl_df", "tbl", "data.frame"))
  expect_identical(dim(df), c(140, 21))
})


