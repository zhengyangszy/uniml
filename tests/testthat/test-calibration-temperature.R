# A three-class cohort drawn from a known multinomial model, so "over-confident"
# and "under-confident" are properties of the data rather than assumptions about
# the code. Raising the true probabilities to a power and renormalising is the
# exact distortion temperature scaling is meant to undo: a model that reports
# `softmax(k * log p)` needs `T = k` to be put right.
temperature_cohort <- function(n = 1500L, seed = 61L) {
  set.seed(seed)
  eta <- cbind(
    a = stats::rnorm(n),
    b = 0.6 * stats::rnorm(n),
    c = -0.4 * stats::rnorm(n)
  )
  prob <- exp(eta) / rowSums(exp(eta))
  dimnames(prob) <- list(NULL, c("a", "b", "c"))
  index <- apply(prob, 1L, function(p) sample.int(3L, 1L, prob = p))
  list(prob = prob, labels = c("a", "b", "c")[index])
}

temperature_sharpen <- function(prob, power) {
  out <- prob^power
  out / rowSums(out)
}

test_that("temperature scaling undoes a known distortion", {
  cohort <- temperature_cohort()
  over <- temperature_sharpen(cohort$prob, 3)
  calibrated <- calibrate_probabilities(over, labels = cohort$labels)

  # `softmax(3 log p)` is repaired by T = 3.
  expect_equal(calibrated$parameters$fitted$temperature, 3, tolerance = 0.15)
  expect_identical(calibrated$method, "temperature")
  expect_true(calibrated$parameters$fitted$converged)
  expect_lte(calibrated$parameters$fitted$nll_after,
             calibrated$parameters$fitted$nll_before)

  before <- calibrated$metrics_before
  after <- calibrated$metrics_after
  expect_lt(after$logloss, before$logloss - 0.2)
  expect_lt(after$ece, before$ece / 5)
  expect_lt(abs(after$level_bias), 0.02)
  # Recalibration must not change what the model decides.
  expect_equal(after$accuracy, before$accuracy, tolerance = 1e-12)
  expect_identical(
    colnames(over)[max.col(over, ties.method = "first")],
    colnames(calibrated$calibrated)[max.col(calibrated$calibrated,
                                             ties.method = "first")]
  )
})

test_that("the fitted scalar follows the direction of the miscalibration", {
  cohort <- temperature_cohort()
  under <- calibrate_probabilities(
    temperature_sharpen(cohort$prob, 0.4),
    labels = cohort$labels
  )
  honest <- calibrate_probabilities(cohort$prob, labels = cohort$labels)

  expect_lt(under$parameters$fitted$temperature, 0.6)
  expect_gt(under$metrics_after$ece, 0)
  expect_gt(honest$parameters$fitted$temperature, 0.85)
  expect_lt(honest$parameters$fitted$temperature, 1.15)
  # The scalar itself is never allowed to be worse than T = 1 on the rows it was
  # fitted on. The cross-fitted comparison is an honest out-of-sample mirror of
  # that, so on data that is already calibrated it can come out a hair worse:
  # each fold's scalar is estimated from the other folds.
  expect_lte(honest$parameters$fitted$nll_after,
             honest$parameters$fitted$nll_before)
  expect_lt(
    abs(honest$metrics_after$logloss - honest$metrics_before$logloss),
    1e-3
  )
  expect_lt(honest$metrics_after$ece, 0.05)
})

test_that("the calibrated matrix keeps its shape and stays a distribution", {
  cohort <- temperature_cohort(n = 600L)
  calibrated <- calibrate_probabilities(
    temperature_sharpen(cohort$prob, 2.5),
    labels = cohort$labels,
    folds = 4L
  )
  expect_identical(dim(calibrated$calibrated), dim(cohort$prob))
  expect_identical(dimnames(calibrated$calibrated), dimnames(cohort$prob))
  expect_true(all(abs(rowSums(calibrated$calibrated) - 1) < 1e-12))
  expect_true(all(calibrated$calibrated >= 0 & calibrated$calibrated <= 1))
  # Cross-fitting estimates one scalar per fold.
  expect_length(calibrated$parameters$folds, 4L)
  expect_true(all(is.finite(calibrated$parameters$folds)))
  expect_false(isTRUE(all.equal(
    calibrated$parameters$folds[[1L]],
    calibrated$parameters$folds[[2L]]
  )))
})

test_that("a scalar fitted on validation improves the test set", {
  cohort <- temperature_cohort(n = 1200L)
  over <- temperature_sharpen(cohort$prob, 3)
  index <- sample.int(nrow(over))
  train <- index[1:600]
  test <- index[601:1200]

  applied <- calibrate_probabilities(
    over[train, , drop = FALSE],
    labels = cohort$labels[train],
    new_probs = over[test, , drop = FALSE],
    new_labels = cohort$labels[test]
  )
  expect_identical(applied$n, 600L)
  expect_lt(applied$metrics_after$logloss, applied$metrics_before$logloss - 0.2)
  expect_lt(applied$metrics_after$ece, applied$metrics_before$ece / 5)
  expect_equal(
    applied$metrics_after$accuracy,
    applied$metrics_before$accuracy,
    tolerance = 1e-12
  )
  # Without labels there is nothing to score, but the handover still works.
  blind <- calibrate_probabilities(
    over[train, , drop = FALSE],
    labels = cohort$labels[train],
    new_probs = over[test, , drop = FALSE]
  )
  expect_null(blind$metrics_before)
  expect_identical(dim(blind$calibrated), c(600L, 3L))
})

test_that("calibration can be switched off", {
  cohort <- temperature_cohort(n = 400L)
  skipped <- calibrate_probabilities(
    cohort$prob,
    labels = cohort$labels,
    method = "none"
  )
  expect_identical(skipped$method, "none")
  expect_identical(skipped$parameters$fitted$temperature, 1)
  expect_identical(skipped$calibrated, skipped$raw)
  expect_match(skipped$message, "skipped")
})

test_that("hard 0/1 predictions are clamped instead of breaking the fit", {
  cohort <- temperature_cohort(n = 300L)
  hard <- cohort$prob
  hard[] <- 0
  hard[cbind(seq_len(nrow(hard)), max.col(cohort$prob))] <- 1
  expect_silent(
    calibrated <- calibrate_probabilities(hard, labels = cohort$labels, folds = 3L)
  )
  expect_true(all(is.finite(calibrated$parameters$fitted$temperature)))
  expect_true(all(abs(rowSums(calibrated$calibrated) - 1) < 1e-12))
})

test_that("multiclass calibration validates its inputs", {
  cohort <- temperature_cohort(n = 300L)
  expect_error(
    calibrate_probabilities(cohort$prob[1:5, ], labels = cohort$labels[1:5]),
    "at least 10"
  )
  expect_error(
    calibrate_probabilities(cohort$prob, labels = rep("a", nrow(cohort$prob))),
    "at least two observed classes"
  )
  expect_error(
    calibrate_probabilities(cbind(no = 0.4, yes = 0.6), labels = c("no", "yes")),
    "binary problem"
  )
  expect_error(calibrate_probabilities(uniml::as_uniml_multiclass(
    cohort$prob, truth = cohort$labels
  )), NA)
  expect_error(
    calibrate_probabilities(cohort$prob, labels = cohort$labels, new_labels = cohort$labels),
    "new_probs"
  )
  expect_error(
    calibrate_probabilities(
      cohort$prob,
      labels = cohort$labels,
      new_probs = cohort$prob[, c("b", "a", "c"), drop = FALSE]
    ),
    "same class columns"
  )
})

test_that("the specification knows which calibration its task type has", {
  multiclass <- uniml_spec(task_type = "multiclass", outcome = "y")
  multiclass <- set_evaluation(multiclass, calibration = "temperature")
  expect_identical(multiclass$evaluation$calibration, "temperature")
  expect_error(
    set_evaluation(multiclass, calibration = "platt"),
    "evaluation\\$calibration"
  )

  binary <- uniml_spec("binary", "y", "yes")
  binary <- set_evaluation(binary, calibration = "platt")
  expect_identical(binary$evaluation$calibration, "platt")
  expect_error(
    set_evaluation(binary, calibration = "temperature"),
    "evaluation\\$calibration"
  )
})
