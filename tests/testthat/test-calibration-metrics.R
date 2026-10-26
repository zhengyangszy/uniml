calibration_fixture <- function(n = 400L, seed = 3L) {
  set.seed(seed)
  x <- stats::rnorm(n)
  p_true <- stats::plogis(-0.3 + 1.1 * x)
  list(p = p_true, y = stats::rbinom(n, 1, p_true))
}

test_that("a calibrated model has slope 1 and intercept 0 inside the interval", {
  fixture <- calibration_fixture()
  out <- calibration_metrics(fixture$p, fixture$y, level = 0.95, R = 200)
  expect_true(out$slope_lower < 1 && out$slope_upper > 1)
  expect_true(out$intercept_lower < 0 && out$intercept_upper > 0)
  expect_identical(out$n, 400L)
  expect_true(all(c("calib_intercept", "calib_slope", "calib_large",
                    "intercept_lower", "slope_upper", "large_upper") %in%
                    names(out)))
})

test_that("the slope and the intercept recover known miscalibration", {
  fixture <- calibration_fixture()

  # Over-confident by a factor of two on the logit scale: slope near 2.
  shrunk <- stats::plogis(stats::qlogis(fixture$p) * 0.5)
  miscalibrated <- calibration_metrics(shrunk, fixture$y, level = NULL)
  expect_gt(miscalibrated$calib_slope, 1.6)

  # Systematic over-prediction of 0.8 on the logit scale:
  # calibration-in-the-large near -0.8 while the slope stays near 1.
  shifted <- stats::plogis(stats::qlogis(fixture$p) + 0.8)
  over <- calibration_metrics(shifted, fixture$y, level = NULL)
  expect_lt(over$calib_large, -0.5)
  expect_true(abs(over$calib_slope - 1) < 0.4)
})

test_that("calibration metrics are part of calc_metrics()", {
  fixture <- calibration_fixture()
  metrics <- calc_metrics(fixture$p, fixture$y, threshold = 0.5)
  expect_true(all(c("calib_slope", "calib_intercept", "calib_large") %in%
                    names(metrics)))
  direct <- calibration_metrics(fixture$p, fixture$y, level = NULL)
  expect_equal(metrics$calib_slope, direct$calib_slope, tolerance = 1e-12)
  expect_equal(metrics$calib_large, direct$calib_large, tolerance = 1e-12)
  expect_true("calib_slope" %in% uniml:::.ci_metric_names())
})

test_that("calibration is reported as missing when it cannot be estimated", {
  # Too few events.
  small <- calc_metrics(c(0.1, 0.2, 0.7, 0.8), c(0, 0, 1, 1), threshold = 0.5)
  expect_true(is.na(small$calib_slope))
  expect_true(is.na(small$calib_intercept))
  expect_true(is.na(small$calib_large))

  # Perfect separation: the slope would run to infinity.
  separated <- calibration_metrics(
    c(0.01, 0.02, 0.03, 0.97, 0.98, 0.99),
    c(0, 0, 0, 1, 1, 1),
    level = NULL
  )
  expect_true(is.na(separated$calib_slope))

  # One class only.
  expect_true(is.na(calibration_metrics(stats::runif(50), rep(0, 50),
                                        level = NULL)$calib_slope))
})

test_that("calibration intervals widen when the sample shrinks", {
  big <- calibration_fixture(n = 800L)
  small <- calibration_fixture(n = 40L)
  big_out <- calibration_metrics(big$p, big$y, level = 0.95, R = 200)
  small_out <- calibration_metrics(small$p, small$y, level = 0.95, R = 200)
  big_width <- big_out$slope_upper - big_out$slope_lower
  small_width <- small_out$slope_upper - small_out$slope_lower
  expect_gt(small_width, big_width)
})
