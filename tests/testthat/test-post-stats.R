test_that("calc_ci() returns reproducible bootstrap metric intervals", {
  probs <- seq(0.05, 0.95, length.out = 40)
  labels <- rep(c(0, 1), each = 20)

  first <- calc_ci(
    probs,
    labels,
    R = 40,
    seed = 42,
    threshold = 0.5,
    metrics = c("auc", "accuracy")
  )
  second <- calc_ci(
    probs,
    labels,
    R = 40,
    seed = 42,
    threshold = 0.5,
    metrics = c("auc", "accuracy")
  )

  expect_identical(first, second)
  expect_identical(
    names(first),
    c("metric", "mean", "se", "lower", "upper", "n_effective", "formatted")
  )
  expect_identical(first$metric, c("auc", "accuracy"))
  expect_true(all(is.finite(first$mean)))
  expect_true(all(first$lower <= first$upper))
  expect_true(all(first$n_effective > 0L))
  expect_match(first$formatted[[1L]], "^[0-9.]+ \\([0-9.]+-[0-9.]+\\)$")

  expect_error(
    calc_ci(probs, labels, metrics = "unknown"),
    "unsupported"
  )
  expect_error(calc_ci(probs, labels, level = 1), "strictly between")

  expect_warning(
    ineffective <- calc_ci(
      c(0.1, 0.2),
      c(0, 0),
      R = 10,
      threshold = 0.5,
      metrics = "auc"
    ),
    "Fewer than"
  )
  expect_identical(ineffective$n_effective, 0L)
  expect_true(is.na(ineffective$mean))
})

test_that("calibration supports Platt and isotonic cross-fitting", {
  set.seed(11)
  n <- 120
  probs <- stats::runif(n, 0.05, 0.95)
  labels <- stats::rbinom(n, 1, probs)

  platt <- calibrate_probabilities(
    probs,
    labels,
    method = "platt",
    folds = 5,
    seed = 99
  )
  platt_again <- calibrate_probabilities(
    probs,
    labels,
    method = "platt",
    folds = 5,
    seed = 99
  )
  isotonic <- calibrate_probabilities(
    probs,
    labels,
    method = "isotonic",
    folds = 5,
    seed = 99
  )

  expect_s3_class(platt, "uniml_calibration")
  expect_identical(platt$method, "platt")
  expect_length(platt$calibrated, n)
  expect_true(all(platt$calibrated >= 0 & platt$calibrated <= 1))
  expect_equal(platt$raw, probs, tolerance = 0)
  expect_identical(platt$calibrated, platt_again$calibrated)
  expect_false(platt$failed)
  expect_identical(
    names(platt$metrics_after),
    c("brier", "ece", "mce", "calib_slope", "calib_intercept",
      "calib_r2", "n")
  )
  expect_identical(length(platt$parameters$folds), 5L)
  expect_s3_class(isotonic, "uniml_calibration")
  expect_true(all(isotonic$calibrated >= 0 & isotonic$calibrated <= 1))
  expect_output(print(platt), "<uniml_calibration>")

  none <- calibrate_probabilities(probs, labels, method = "none")
  expect_equal(none$calibrated, probs, tolerance = 0)

  expect_error(
    calibrate_probabilities(c(0.1, 0.2), c(0, 0)),
    "both outcome classes"
  )
})

test_that("calibration fits once and evaluates on a differently sized set", {
  set.seed(21)
  train_probs <- stats::runif(100, 0.05, 0.95)
  train_labels <- stats::rbinom(100, 1, train_probs)
  validation_probs <- stats::runif(35, 0.05, 0.95)
  validation_labels <- stats::rbinom(35, 1, validation_probs)

  binary <- structure(
    list(prob = train_probs, truth = train_labels),
    class = c("uniml_binary", "uniml_prediction")
  )
  validation <- calibrate_probabilities(
    binary,
    new_probs = validation_probs,
    new_labels = validation_labels,
    method = "platt",
    bins = 5
  )

  expect_length(validation$calibrated, length(validation_probs))
  expect_true(all(validation$calibrated >= 0 & validation$calibrated <= 1))
  expect_identical(dim(validation$metrics_before), c(1L, 7L))
  expect_identical(dim(validation$metrics_after), c(1L, 7L))
  expect_identical(validation$n, 35L)

  validation_probs[[1L]] <- NA_real_
  validation_labels[[2L]] <- NA
  with_missing <- calibrate_probabilities(
    binary,
    new_probs = validation_probs,
    new_labels = validation_labels,
    method = "platt",
    bins = 5
  )
  expect_length(with_missing$calibrated, length(validation_probs))
  expect_identical(with_missing$metrics_after$n, 33L)
})

test_that("calibration falls back to raw probabilities on fold failure", {
  probs <- c(0.1, 0.9)
  labels <- c(0, 1)
  result <- suppressWarnings(
    calibrate_probabilities(
      probs,
      labels,
      method = "platt",
      folds = 2,
      seed = 1
    )
  )

  expect_true(result$failed)
  expect_equal(result$calibrated, probs, tolerance = 0)
  expect_match(result$message, "raw probabilities")
})

test_that("delong_test() compares models and handles skipped cases", {
  set.seed(31)
  labels <- rep(c(0, 1), each = 50)
  pred_a <- stats::plogis(stats::rnorm(100, ifelse(labels == 1, 0.5, -0.5)))
  pred_b <- stats::plogis(stats::rnorm(100, ifelse(labels == 1, 0.8, -0.8)))
  result <- delong_test(list(lr = pred_a, rf = pred_b), labels)

  expect_identical(result$status, "ok")
  expect_identical(result$n_models, 2L)
  expect_identical(dim(result$p_matrix), c(2L, 2L))
  expect_identical(dimnames(result$p_matrix), list(c("lr", "rf"), c("lr", "rf")))
  expect_true(all(is.na(diag(result$p_matrix))))
  expect_equal(result$p_matrix[1L, 2L], result$p_matrix[2L, 1L])
  expect_true(is.finite(result$p_matrix[1L, 2L]))
  expect_true(result$best_model %in% c("lr", "rf"))
  expect_identical(result$summary$model, c("lr", "rf"))

  skipped <- delong_test(list(lr = pred_a), labels)
  expect_identical(skipped$status, "skipped")
  expect_true(is.na(skipped$summary$auc))

  insufficient <- delong_test(
    list(lr = pred_a[labels == 0], rf = pred_b[labels == 0]),
    labels[labels == 0]
  )
  expect_identical(insufficient$status, "insufficient_classes")
  expect_match(insufficient$message, "both outcome classes")

  expect_error(
    delong_test(list(lr = pred_a, rf = pred_b[-1L]), labels),
    "equal lengths"
  )
})

test_that("hl_test() returns grouped goodness-of-fit results", {
  set.seed(41)
  probs <- stats::runif(200)
  labels <- stats::rbinom(200, 1, probs)
  result <- hl_test(probs, labels, g = 10)

  expect_true(is.finite(result$statistic))
  expect_true(result$n_groups >= 3L)
  expect_true(result$n_groups <= 10L)
  expect_true(result$p_value >= 0 && result$p_value <= 1)
  expect_true(result$method %in% c("ResourceSelection", "custom"))

  custom <- uniml:::.custom_hl(probs, labels, g = 10)
  expect_true(is.finite(custom$statistic))
  expect_identical(custom$n_groups, nrow(custom$groups))

  few_groups <- hl_test(rep(0.5, 20), rep(c(0, 1), 10), g = 10)
  expect_true(is.na(few_groups$statistic))
  expect_identical(few_groups$n_groups, 2L)
  expect_match(few_groups$message, "Fewer than three")

  single_class <- hl_test(stats::runif(20), rep(0, 20), g = 10)
  expect_true(is.na(single_class$statistic))
  expect_identical(single_class$method, "insufficient_data")
})

test_that("hl_test() reports an undefined statistic instead of NaN", {
  # A group whose probabilities are all 0 (or all 1) leaves an expected cell at
  # zero, which is the same condition that breaks ResourceSelection's statistic.
  # Both the package path and the internal fallback must say so.
  probs <- c(rep(0, 10), rep(0.5, 10), rep(0.9, 10))
  labels <- rep(c(0, 1), 15)

  grouped <- suppressWarnings(hl_test(probs, labels, g = 3))
  fallback <- suppressWarnings(uniml:::.custom_hl(probs, labels, 3))

  expect_true(is.na(grouped$statistic))
  expect_true(is.na(grouped$p_value))
  expect_identical(grouped$method, "not_estimable")
  expect_match(grouped$message, "undefined")

  expect_true(is.na(fallback$statistic))
  expect_identical(fallback$method, "not_estimable")
  expect_identical(fallback$message, grouped$message)
})

test_that("dca() calculates model and baseline net benefit", {
  probs <- c(0.1, 0.4, 0.6, 0.9)
  labels <- c(0, 0, 1, 1)
  result <- dca(
    probs,
    labels,
    thresholds = c(0.2, 0.5, 0.8)
  )

  expect_s3_class(result, "uniml_dca")
  expect_identical(nrow(result), 9L)
  expect_identical(
    names(result),
    c(
      "model", "strategy", "threshold", "net_benefit", "n_samples",
      "calibrated"
    )
  )

  model <- result[result$strategy == "model", , drop = FALSE]
  treat_all <- result[result$strategy == "treat_all", , drop = FALSE]
  treat_none <- result[result$strategy == "treat_none", , drop = FALSE]

  expect_equal(model$net_benefit, c(0.4375, 0.5, 0.25), tolerance = 1e-12)
  expect_equal(treat_all$net_benefit, c(0.375, 0, -1.5), tolerance = 1e-12)
  expect_equal(treat_none$net_benefit, rep(0, 3), tolerance = 0)
  expect_identical(unique(result$n_samples), 4L)
  expect_output(print(result), "<uniml_dca>")

  expect_error(
    dca(probs, labels, thresholds = c(0.2, 1)),
    "thresholds"
  )
  expect_error(
    dca(probs, labels, thresholds = c(0.2, NA_real_)),
    "thresholds"
  )
})
