# A three-class cohort drawn from a known multinomial model, so that "calibrated
# by construction" and "over-confident" are facts about the data rather than
# assumptions about the code.
calibration_cohort <- function(n = 2000L, seed = 41L) {
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

# Raising every probability to a power and renormalising sharpens (power > 1) or
# flattens (power < 1) the predicted distribution without touching the ranking.
sharpen <- function(prob, power) {
  out <- prob^power
  out / rowSums(out)
}

test_that("a model calibrated by construction has a small ECE and no level bias", {
  cohort <- calibration_cohort()
  calibration <- multiclass_calibration(cohort$prob, cohort$labels)
  summary <- calibration$summary
  expect_identical(summary$n_classes, 3L)
  expect_identical(summary$bins, 10L)
  expect_lt(summary$ece, 0.03)
  expect_lt(summary$ece_classwise, 0.05)
  expect_lt(abs(summary$level_bias), 0.03)
  # Confidence cannot sit below the accuracy of the model it belongs to.
  expect_true(summary$mean_confidence >= summary$accuracy - 0.02)
})

test_that("ECE grows with the miscalibration and keeps its sign", {
  cohort <- calibration_cohort()
  calibrated <- multiclass_calibration(cohort$prob, cohort$labels)$summary
  over <- multiclass_calibration(sharpen(cohort$prob, 3), cohort$labels)$summary
  under <- multiclass_calibration(sharpen(cohort$prob, 0.4), cohort$labels)$summary

  expect_gt(over$ece, calibrated$ece + 0.1)
  expect_gt(under$ece, calibrated$ece + 0.05)
  # Over-confidence: the stated probability is above the hit rate.
  expect_gt(over$level_bias, 0)
  expect_lt(under$level_bias, 0)
  # Accuracy is unchanged by a monotone rescaling of the scores.
  expect_equal(over$accuracy, calibrated$accuracy, tolerance = 1e-12)
  expect_equal(under$accuracy, calibrated$accuracy, tolerance = 1e-12)
})

test_that("class-wise ECE is the mean of the per-class values", {
  cohort <- calibration_cohort()
  calibration <- multiclass_calibration(sharpen(cohort$prob, 3), cohort$labels)
  per_class <- calibration$per_class
  expect_identical(per_class$class, c("a", "b", "c"))
  expect_equal(sum(per_class$n), nrow(cohort$prob))
  expect_equal(sum(per_class$prevalence), 1, tolerance = 1e-12)
  expect_equal(
    calibration$summary$ece_classwise,
    mean(per_class$ece),
    tolerance = 1e-12
  )
  expect_true(all(per_class$ece >= 0))
  expect_true(all(per_class$mce >= per_class$ece))
})

test_that("the reliability table covers both conventions without gaps", {
  cohort <- calibration_cohort()
  calibration <- multiclass_calibration(cohort$prob, cohort$labels, bins = 5L)
  reliability <- calibration$reliability
  expect_setequal(unique(reliability$scope), c("top_label", "class"))
  expect_true(all(c("bin", "n", "predicted", "observed") %in% names(reliability)))
  # Every patient lands in exactly one top-label bin.
  top <- reliability[reliability$scope == "top_label", , drop = FALSE]
  expect_equal(sum(top$n), nrow(cohort$prob))
  # Every class covers every patient, so its bins do too.
  for (name in c("a", "b", "c")) {
    rows <- reliability[reliability$scope == "class" & reliability$class == name, ,
                        drop = FALSE]
    expect_equal(sum(rows$n), nrow(cohort$prob))
  }
  expect_true(all(reliability$bin >= 1L & reliability$bin <= 5L))
})

test_that("the positive column reproduces the binary calibration numbers", {
  set.seed(43)
  n <- 500L
  prob <- stats::runif(n)
  event <- stats::rbinom(n, 1, prob)
  binary <- calc_metrics(prob, truth = event)
  matrix_prob <- cbind(no = 1 - prob, yes = prob)
  calibration <- multiclass_calibration(matrix_prob, ifelse(event == 1L, "yes", "no"))

  expect_equal(calibration$per_class$ece[[2L]], binary$ece, tolerance = 1e-12)
  expect_equal(calibration$per_class$mce[[2L]], binary$mce, tolerance = 1e-12)
  # Top-label ECE is a different estimand: confidence in the predicted class,
  # not the probability of the event.
  expect_false(isTRUE(all.equal(calibration$summary$ece, binary$ece)))
})

test_that("calc_metrics() reports the multiclass calibration numbers", {
  cohort <- calibration_cohort(n = 600L)
  metrics <- calc_metrics(sharpen(cohort$prob, 3), truth = cohort$labels)
  expect_true(all(c("ece", "mce", "ece_classwise") %in% names(metrics)))
  expect_true(all(c("ece", "mce") %in% names(metrics$per_class)))
  expect_equal(metrics$ece_classwise, mean(metrics$per_class$ece), tolerance = 1e-12)
  expect_true(metrics$mce >= metrics$ece)

  # The numbers can be requested on their own.
  subset <- calc_metrics(cohort$prob, truth = cohort$labels, metrics = "ece")
  expect_identical(names(subset), "ece")

  intervals <- calc_ci(
    cohort$prob, cohort$labels, R = 25L,
    metrics = c("ece", "ece_classwise")
  )
  expect_setequal(intervals$metric, c("ece", "ece_classwise"))
  expect_true(all(is.finite(intervals$lower)))
  expect_true(all(intervals$lower <= intervals$upper))
})

test_that("plot_calibration() draws both conventions", {
  cohort <- calibration_cohort(n = 600L)
  predictions <- as_uniml_multiclass(sharpen(cohort$prob, 3), truth = cohort$labels)
  top <- plot_calibration(predictions, scope = "top_label")
  class_wise <- plot_calibration(predictions, scope = "class")
  both <- plot_calibration(predictions, scope = "both")
  for (plot in list(top, class_wise, both)) {
    expect_s3_class(plot, "ggplot")
  }
  expect_match(top$labels$caption, "ECE")
  expect_match(top$labels$caption, "Class-wise ECE")
  expect_null(plot_calibration(predictions, annotate = FALSE)$labels$caption)
  # `scope` filters the curves it names and nothing else. Top-label bars can be
  # empty (confidence never falls below 1/n_classes), so only the ordering is
  # fixed here.
  expect_lte(nrow(top$data), 10L)
  expect_gt(nrow(top$data), 0L)
  expect_gt(nrow(class_wise$data), nrow(top$data))
  expect_identical(nrow(both$data), nrow(top$data) + nrow(class_wise$data))
  expect_error(plot_calibration(predictions, scope = "nonsense"), "one of")
})

test_that("multiclass calibration validates its inputs", {
  cohort <- calibration_cohort(n = 300L)
  expect_error(multiclass_calibration(cohort$prob, cohort$labels, bins = 1L), "bins")
  expect_error(
    multiclass_calibration(cohort$prob, rep("a", nrow(cohort$prob))[-1L]),
    "same number of rows"
  )
  expect_error(
    multiclass_calibration(cohort$prob / 2, cohort$labels),
    "sum to 1"
  )
  # A multinomial model has no slope or intercept to report, and the error says
  # where to look instead.
  expect_error(calibration_metrics(cohort$prob, cohort$labels), "calc_metrics")
})
