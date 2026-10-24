test_that("extract_thresholds returns curve shapes and empty crossings", {
  n <- 80L
  x <- seq(-2, 2, length.out = n)
  shap_values <- x
  result <- list(
    values_matrix_raw = matrix(shap_values, ncol = 1L, dimnames = list(NULL, "x")),
    feature_values_raw = data.frame(x = x)
  )

  thresholds <- extract_thresholds(
    result,
    target = "x",
    min_n = 30L,
    min_unique = 10L,
    main_grid = 200L
  )

  expect_s3_class(thresholds$zero_crossings, "data.frame")
  expect_s3_class(thresholds$paper_ready, "data.frame")
  expect_true(all(
    thresholds$paper_ready$curve_shape %in%
      c("monotone", "U-shaped", "no clear threshold")
  ))
  expect_equal(nrow(thresholds$zero_crossings), 1L)
  expect_true(abs(thresholds$zero_crossings$crossing - 0) < 0.05)
})

test_that("extract_thresholds handles an observed U-shaped curve", {
  n <- 120L
  x <- seq(-2, 2, length.out = n)
  result <- list(
    values_matrix_raw = matrix(
      (x - 1) * (x + 1),
      ncol = 1L,
      dimnames = list(NULL, "x")
    ),
    feature_values_raw = data.frame(x = x)
  )

  thresholds <- extract_thresholds(
    result,
    target = "x",
    min_n = 30L,
    min_unique = 10L,
    main_grid = 500L,
    span = 0.55
  )

  expect_identical(thresholds$paper_ready$curve_shape, "U-shaped")
  expect_equal(nrow(thresholds$zero_crossings), 2L)
})

test_that("extract_thresholds validates small or non-numeric targets", {
  expect_error(
    extract_thresholds(
      list(
        values_matrix_raw = matrix(1:10, ncol = 1L, dimnames = list(NULL, "x")),
        feature_values_raw = data.frame(x = 1:10)
      ),
      target = "x"
    ),
    "at least"
  )
})
