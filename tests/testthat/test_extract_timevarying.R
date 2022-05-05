
hic_codes <- c("NIHR_HIC_ICU_0108", "NIHR_HIC_ICU_0126")
new_labels <- c("heart_rate", "airway")

con <- setup_dummy_db()

test_that("Errors are captured", {
  expect_error(extract_timevarying())
  expect_error(extract_timevarying(connection = con, code_names = "fish_fingers"))
  expect_error(extract_timevarying(connection = con, 
                                   code_names = hic_codes,
                                   episode_ids = 1,
                                   time_boundaries = c("lemon")))
  expect_error(extract_timevarying(connection = con,
                                   code_names = hic_codes,
                                   episode_ids = 1,
                                   time_boundaries = c(Inf, 6)))
  expect_error(extract_timevarying(connection = con,
                                   code_names = hic_codes,
                                   episode_ids = 1,
                                   time_boundaries = c(-Inf, -6)))
})

ltb1 <- extract_timevarying(connection = con,
                            code_names = hic_codes,
                            rename = new_labels,
                            episode_ids = 1:10)

test_that("Table properties are correct", {
  expect_true(tibble::is_tibble(ltb1))
  expect_identical(
    dplyr::distinct(ltb1, episode_id, time, .keep_all = TRUE),
    ltb1
  )
  expect_identical(names(ltb1)[1:2], c("episode_id", "time"))
})

hic_codes <- c("NIHR_HIC_ICU_0116")
new_labels <- c("cvp")

ltb_meta <- extract_timevarying(connection = con,
                                code_names = hic_codes,
                                rename = new_labels,
                                episode_ids = 1:10)

test_that("Meta-data placed correctly into columns", {
  expect_true(tibble::is_tibble(ltb_meta))
  expect_identical(
    dplyr::distinct(ltb_meta, episode_id, time, .keep_all = TRUE),
    ltb_meta
  )
  expect_identical(names(ltb_meta), c("episode_id", "time", paste0("cvp_", 0:5)))
})


hic_codes <- c("NIHR_HIC_ICU_0108", "NIHR_HIC_ICU_0126")
new_labels <- c("heart_rate", "airway")

ltb_chunk <- extract_timevarying(connection = con,
                                 code_names = hic_codes,
                                 rename = new_labels,
                                 episode_ids = 1:10,
                                 chunk_size = 5)

test_that("Chunking doesn't effect outcome", {
  expect_identical(ltb1, ltb_chunk)
  expect_identical(
    dplyr::distinct(ltb_chunk, episode_id, time, .keep_all = TRUE),
    ltb_chunk
  )
})

hic_codes <- c("NIHR_HIC_ICU_0108", "NIHR_HIC_ICU_0126")
new_labels <- c("heart_rate", "airway")

ltb <- extract_timevarying(connection = con,
                           code_names = hic_codes,
                           rename = new_labels,
                           episode_ids = 1:10,
                           time_boundaries = c(-Inf, 6))

test_that("Time boundaries are respected", {
  expect_lte(max(ltb$time), 6)
  expect_identical(
    dplyr::distinct(ltb, episode_id, time, .keep_all = TRUE),
    ltb
  )
})

ltb_24 <- extract_timevarying(connection = con,
                              code_names = hic_codes,
                              rename = new_labels,
                              episode_ids = 1:10,
                              cadence = 24)

ltb_05 <- extract_timevarying(connection = con,
                              code_names = hic_codes,
                              rename = new_labels,
                              episode_ids = 1:10,
                              cadence = 0.5)

test_that("cadence works as expected", {
  expect_true(all(ltb_24$time %% 24 == 0))
  expect_true(all(ltb_05$time %% 0.5 == 0))
  expect_identical(
    dplyr::distinct(ltb_24, episode_id, time, .keep_all = TRUE),
    ltb_24
  )
  expect_identical(
    dplyr::distinct(ltb_05, episode_id, time, .keep_all = TRUE),
    ltb_05
  )
})


ltb <- extract_timevarying(connection = con,
                           code_names = hic_codes,
                           rename = new_labels,
                           episode_ids = 1:10,
                           cadence = 0)

test_that("time exact extraction works", {
  expect_true(tibble::is_tibble(ltb))
  expect_identical(
    dplyr::distinct(ltb, episode_id, time, .keep_all = TRUE),
    ltb
  )
  expect_identical(names(ltb)[1:2], c("episode_id", "time"))
})

ltb <- extract_timevarying(connection = con,
                           code_names = hic_codes,
                           rename = new_labels,
                           episode_ids = 1:10,
                           cadence = "timestamp")

test_that("timestamp extraction works", {
  expect_true(tibble::is_tibble(ltb))
  expect_identical(
    dplyr::distinct(ltb, episode_id, time, .keep_all = TRUE),
    ltb
  )
  expect_identical(names(ltb)[1:2], c("episode_id", "time"))
})
