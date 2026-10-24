cvd_example_path <- function(file_name) {
  installed <- system.file("extdata", file_name, package = "uniml")
  if (nzchar(installed)) {
    return(installed)
  }
  testthat::test_path("..", "..", "inst", "extdata", file_name)
}

read_cvd_example <- function(file_name) {
  utils::read.csv(
    cvd_example_path(file_name),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

test_that("the cardiovascular development cohort is ready for examples", {
  data <- read_cvd_example("uniml-cvd-mortality.csv")

  expect_equal(nrow(data), 2000L)
  expect_identical(names(data)[[1L]], "id")
  expect_identical(names(data)[[ncol(data)]], "died")
  expect_true(all(data$died %in% c("no", "yes")))
  expect_setequal(unique(data$died), c("no", "yes"))
  expect_identical(anyDuplicated(data$id), 0L)
  expect_true(mean(data$died == "yes") > 0.04)
  expect_true(mean(data$died == "yes") < 0.10)

  expected_signal_columns <- c(
    "age", "lvef", "nt_probnp", "creatinine", "killip_class", "sbp"
  )
  expect_true(all(expected_signal_columns %in% names(data)))
  expect_true(all(c("lab_noise_1", "lab_noise_2", "ward_code") %in% names(data)))
  expect_true(anyNA(data))
})

test_that("the new-admission cohort is independent and comparable", {
  development <- read_cvd_example("uniml-cvd-mortality.csv")
  new_admissions <- read_cvd_example("uniml-cvd-mortality-new.csv")

  expect_equal(nrow(new_admissions), 2000L)
  expect_identical(names(new_admissions), names(development))
  expect_true(all(new_admissions$died %in% c("no", "yes")))
  expect_setequal(unique(new_admissions$died), c("no", "yes"))
  expect_identical(anyDuplicated(new_admissions$id), 0L)
  expect_length(intersect(development$id, new_admissions$id), 0L)
  expect_true(mean(new_admissions$died == "yes") > 0.04)
  expect_true(mean(new_admissions$died == "yes") < 0.10)
  expect_true(anyNA(new_admissions))
})
