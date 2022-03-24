hr_vec <- c(-1L, 0L, 1L, 299L, 300L, 301L)
hr_na <- c(NA_integer_, 0L, 1L, 299L, 300L, NA_integer_)
hr_lim <- c(0L, 0L, 1L, 299L, 300L, 300L)

test_that("Cleaning helper works correctly on integers", {
  ## HR default limits are [0, 300]
  expect_identical(
    cleaning_helper(col_vec = hr_vec,
                  col_name = "NIHR_HIC_ICU_0108",
                  action = "NA"),
    hr_na)
  expect_identical(
    cleaning_helper(col_vec = hr_vec,
                    col_name = "NIHR_HIC_ICU_0108",
                    action = "limit"),
    hr_lim)
  expect_identical(
    class(cleaning_helper(col_vec = hr_vec,
                    col_name = "NIHR_HIC_ICU_0108",
                    action = "NA")),
    "integer")
  expect_identical(
    class(cleaning_helper(col_vec = hr_vec,
                          col_name = "NIHR_HIC_ICU_0108",
                          action = "limit")),
    "integer")

})

fi_vec <- c(0.2, 0.21, 0.22, 0.99, 1, 1.01)
fi_na <- c(NA_real_, 0.21, 0.22, 0.99, 1, NA_real_)
fi_lim <- c(0.21, 0.21, 0.22, 0.99, 1, 1)

test_that("Cleaning helper works correctly on real numbers", {
  ## FIO2 default limits are [0.21, 1]
  expect_identical(
    cleaning_helper(col_vec = fi_vec,
                    col_name = "NIHR_HIC_ICU_0150",
                    action = "NA"),
    fi_na)
  expect_identical(
    cleaning_helper(col_vec = fi_vec,
                    col_name = "NIHR_HIC_ICU_0150",
                    action = "limit"),
    fi_lim)
  expect_identical(
    class(cleaning_helper(col_vec = fi_vec,
                          col_name = "NIHR_HIC_ICU_0150",
                          action = "NA")),
    "numeric")
  expect_identical(
    class(cleaning_helper(col_vec = fi_vec,
                          col_name = "NIHR_HIC_ICU_0150",
                          action = "limit")),
    "numeric")

})


eth_vec <- c("A", "B", "C", "D", "water", "air")
eth_na <- c("A", "B", "C", "D", NA_character_, NA_character_)

test_that("Cleaning helper works correctly on strings", {

  expect_identical(
    cleaning_helper(col_vec = eth_vec,
                    col_name = "NIHR_HIC_ICU_0058",
                    action = "NA"),
    eth_na)
  expect_warning(
    cleaning_helper(col_vec = eth_vec,
                    col_name = "NIHR_HIC_ICU_0058",
                    action = "limit"))
  expect_identical(
    class(cleaning_helper(col_vec = eth_vec,
                          col_name = "NIHR_HIC_ICU_0058",
                          action = "NA")),
    "character")
})

df_in <- tibble(
  NIHR_HIC_ICU_0108 = hr_vec,
  NIHR_HIC_ICU_0150 = fi_vec,
  NIHR_HIC_ICU_0058 = eth_vec
)

df_out_na <- tibble(
  NIHR_HIC_ICU_0108 = hr_na,
  NIHR_HIC_ICU_0150 = fi_na,
  NIHR_HIC_ICU_0058 = eth_na
)

df_out_lim <- tibble(
  NIHR_HIC_ICU_0108 = hr_lim,
  NIHR_HIC_ICU_0150 = fi_lim,
  NIHR_HIC_ICU_0058 = eth_vec
)

test_that("Cleaning works correctly on tables", {
  expect_identical(clean(df_in), df_out_na)
  expect_warning(clean(df_in, action = "limit"))
  expect_identical(suppressWarnings(clean(df_in, action = "limit")), df_out_lim)
})
