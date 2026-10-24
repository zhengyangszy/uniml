test_that("pool_mi() follows Rubin's rules with Barnard-Rubin degrees of freedom", {
  estimates <- c(0.80, 0.82, 0.79)
  variances <- c(0.0004, 0.0004, 0.0005)

  pooled <- pool_mi(estimates, variances)

  expect_equal(pooled$mean, mean(estimates))
  expect_equal(pooled$within, mean(variances))
  expect_equal(pooled$between, stats::var(estimates))
  expect_equal(
    pooled$total,
    mean(variances) + (1 + 1 / 3) * stats::var(estimates)
  )
  expect_equal(pooled$se, sqrt(pooled$total))

  # Barnard-Rubin df is finite and below the Rubin (1987) df for small m.
  fraction <- (1 + 1 / 3) * pooled$between / pooled$total
  nu_old <- 2 / fraction^2
  expect_true(is.finite(pooled$df))
  expect_lt(pooled$df, nu_old)

  # The t interval is wider than the normal interval for small m.
  z_interval <- pooled$mean + c(-1, 1) * stats::qnorm(0.975) * pooled$se
  expect_lt(pooled$lower, z_interval[[1L]])
  expect_gt(pooled$upper, z_interval[[2L]])
  expect_equal(
    pooled$lower,
    pooled$mean - stats::qt(0.975, pooled$df) * pooled$se
  )
})

test_that("pool_mi() degenerates sensibly", {
  single <- pool_mi(0.75, 0.0009)
  expect_equal(single$mean, 0.75)
  expect_equal(single$se, 0.03)
  expect_true(is.infinite(single$df))
  expect_equal(single$lower, 0.75 - stats::qnorm(0.975) * 0.03)

  constant <- pool_mi(c(0.8, 0.8), c(0.0001, 0.0001))
  expect_equal(constant$mean, 0.8)
  expect_true(is.infinite(constant$df))
  expect_gt(constant$upper, constant$lower)

  zero_variance <- pool_mi(c(0.8, 0.8), c(0, 0))
  expect_equal(zero_variance$lower, 0.8)
  expect_equal(zero_variance$upper, 0.8)
  expect_true(is.na(zero_variance$df))

  expect_error(pool_mi(c(0.8, NA), c(0, 0)), "finite")
  expect_error(pool_mi(0.8, c(0, 0)), "equal length")
  expect_error(pool_mi(c(0.8, 0.9), c(-1, 0)), "must be finite")
})
