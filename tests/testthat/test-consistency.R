# Cross-implementation consistency --------------------------------------------
#
# The tests in this file pin `uniml`'s numbers to independent reference
# implementations. A change in either direction (a regression in `uniml` or a
# silently different reference) fails the build instead of only changing the
# reported performance.

# Deliberately rounded probabilities so that the tie handling of `auc()` and
# `pr_auc()` is exercised as well.
consistency_fixture <- function(n = 200L, seed = 2026L) {
  set.seed(seed)
  event <- rep(c(0L, 1L), length.out = n)
  prob <- stats::plogis(stats::rnorm(n, ifelse(event == 1L, 0.6, -0.6), 1.1))
  list(prob = round(prob, 2), event = event)
}

# Mann-Whitney U formulation of the ROC AUC, including the 0.5 weight for ties.
consistency_auc_u <- function(prob, event) {
  positive <- prob[event == 1L]
  negative <- prob[event == 0L]
  comparisons <- outer(positive, negative, "-")
  (sum(comparisons > 0) + 0.5 * sum(comparisons == 0)) /
    (length(positive) * length(negative))
}

test_that("AUC agrees with mlr3measures, pROC, and the Mann-Whitney statistic", {
  fixture <- consistency_fixture()
  metrics <- calc_metrics(fixture$prob, fixture$event, threshold = 0.5)
  truth <- factor(fixture$event, levels = c(0, 1))

  expect_equal(
    metrics$auc,
    mlr3measures::auc(truth, fixture$prob, positive = "1"),
    tolerance = 1e-12
  )
  expect_equal(
    metrics$auc,
    as.numeric(pROC::auc(pROC::roc(fixture$event, fixture$prob, quiet = TRUE))),
    tolerance = 1e-12
  )
  expect_equal(
    metrics$auc,
    consistency_auc_u(fixture$prob, fixture$event),
    tolerance = 1e-12
  )
})

test_that("PR-AUC agrees with PRROC's step-wise integral", {
  skip_if_not_installed("PRROC")
  fixture <- consistency_fixture()
  metrics <- calc_metrics(fixture$prob, fixture$event, threshold = 0.5)

  reference <- PRROC::pr.curve(
    scores.class0 = fixture$prob[fixture$event == 1L],
    scores.class1 = fixture$prob[fixture$event == 0L],
    curve = FALSE
  )

  # PRROC reports the trapezoidal and the Davis-Goadrich integral; `uniml`
  # follows the step-wise curve, which is the trapezoidal variant here.
  expect_equal(
    metrics$pr_auc,
    reference$auc.integral,
    tolerance = 1e-8
  )
  expect_equal(
    metrics$pr_auc,
    mlr3measures::prauc(
      factor(fixture$event, levels = c(0, 1)),
      fixture$prob,
      positive = "1"
    ),
    tolerance = 1e-12
  )
})

test_that("Brier score equals the mean squared probability error", {
  fixture <- consistency_fixture()
  metrics <- calc_metrics(fixture$prob, fixture$event, threshold = 0.5)

  expect_equal(
    metrics$brier,
    mean((fixture$event - fixture$prob)^2),
    tolerance = 1e-12
  )
})

test_that("threshold metrics agree with a direct 2x2 recomputation", {
  fixture <- consistency_fixture()
  threshold <- 0.5
  metrics <- calc_metrics(fixture$prob, fixture$event, threshold = threshold)

  predicted <- fixture$prob >= threshold
  observed <- fixture$event == 1L
  # Recompute the cells from the raw vectors instead of reusing `metrics$tp`.
  cell <- table(
    factor(predicted, levels = c(TRUE, FALSE)),
    factor(observed, levels = c(TRUE, FALSE))
  )
  tp <- cell[1L, 1L]
  fp <- cell[1L, 2L]
  fn <- cell[2L, 1L]
  tn <- cell[2L, 2L]

  sensitivity <- tp / (tp + fn)
  specificity <- tn / (tn + fp)
  ppv <- tp / (tp + fp)
  npv <- tn / (tn + fn)
  total <- tp + tn + fp + fn

  expect_equal(metrics$tp, as.integer(tp), tolerance = 0)
  expect_equal(metrics$tn, as.integer(tn), tolerance = 0)
  expect_equal(metrics$fp, as.integer(fp), tolerance = 0)
  expect_equal(metrics$fn, as.integer(fn), tolerance = 0)
  expect_equal(metrics$accuracy, (tp + tn) / total, tolerance = 1e-12)
  expect_equal(metrics$sensitivity, sensitivity, tolerance = 1e-12)
  expect_equal(metrics$specificity, specificity, tolerance = 1e-12)
  expect_equal(metrics$ppv, ppv, tolerance = 1e-12)
  expect_equal(metrics$npv, npv, tolerance = 1e-12)
  expect_equal(
    metrics$f1,
    2 * tp / (2 * tp + fp + fn),
    tolerance = 1e-12
  )
  expect_equal(
    metrics$f2,
    5 * tp / (5 * tp + 4 * fn + fp),
    tolerance = 1e-12
  )
  expect_equal(
    metrics$youden,
    sensitivity + specificity - 1,
    tolerance = 1e-12
  )
  expect_equal(
    metrics$plr,
    sensitivity / (1 - specificity),
    tolerance = 1e-12
  )
  expect_equal(
    metrics$nlr,
    (1 - sensitivity) / specificity,
    tolerance = 1e-12
  )
  expect_equal(
    metrics$dor,
    (tp * tn) / (fp * fn),
    tolerance = 1e-12
  )
  expect_equal(
    metrics$balanced_accuracy,
    (sensitivity + specificity) / 2,
    tolerance = 1e-12
  )

  observed_agreement <- (tp + tn) / total
  chance_agreement <- (sum(cell[1L, ]) * sum(cell[, 1L]) +
    sum(cell[2L, ]) * sum(cell[, 2L])) / total^2
  expect_equal(
    metrics$kappa,
    (observed_agreement - chance_agreement) / (1 - chance_agreement),
    tolerance = 1e-12
  )
})

test_that("a PredictionClassif scores like its probability vector", {
  set.seed(11)
  data <- data.frame(
    y = factor(rep(c("no", "yes"), each = 30), levels = c("no", "yes")),
    x1 = stats::rnorm(60)
  )
  data$x1 <- data$x1 + ifelse(data$y == "yes", 1, -1)
  spec <- uniml_spec(task_type = "binary", outcome = "y", positive = "yes")
  task <- make_task(data, spec)
  learner <- train_model(task, mlr3::lrn("classif.rpart", predict_type = "prob"))
  prediction <- learner$predict(task)

  # mlr3 keeps the positive class in the first probability column, so the
  # detected class must not be read off the last column.
  expect_identical(colnames(prediction$prob)[[1L]], "yes")
  expect_identical(as_uniml_binary(prediction)$positive, "yes")

  from_prediction <- calc_metrics(prediction, threshold = 0.5)
  from_vector <- calc_metrics(
    unname(prediction$prob[, "yes"]),
    data$y,
    threshold = 0.5,
    positive = "yes"
  )

  expect_equal(from_prediction, from_vector, tolerance = 1e-12)

  # An explicit `positive` still wins and swaps the confusion-matrix view.
  flipped <- calc_metrics(prediction, threshold = 0.5, positive = "no")
  expect_equal(flipped$sensitivity, from_prediction$specificity, tolerance = 1e-12)
  expect_equal(flipped$specificity, from_prediction$sensitivity, tolerance = 1e-12)
  expect_error(
    calc_metrics(prediction, threshold = 0.5, positive = "unknown"),
    "must be one of"
  )
})

test_that("Platt calibration reproduces logistic regression on the logit scale", {
  fixture <- consistency_fixture()
  calibrated <- calibrate_probabilities(
    fixture$prob,
    fixture$event,
    method = "platt",
    folds = 5,
    seed = 7
  )
  logit <- stats::qlogis(pmin(pmax(fixture$prob, 1e-12), 1 - 1e-12))
  reference <- stats::glm(fixture$event ~ logit, family = stats::binomial())
  coefficients <- unname(stats::coef(reference))

  expect_equal(
    calibrated$parameters$fitted$intercept,
    coefficients[[1L]],
    tolerance = 1e-10
  )
  expect_equal(
    calibrated$parameters$fitted$slope,
    coefficients[[2L]],
    tolerance = 1e-10
  )

  # The fitted map is `plogis(a + b * logit(p))`.
  expect_equal(
    calibrated$parameters$fitted$intercept +
      calibrated$parameters$fitted$slope * logit,
    unname(stats::predict(reference)),
    tolerance = 1e-10
  )
})

test_that("isotonic calibration reproduces stats::isoreg", {
  fixture <- consistency_fixture()
  calibrated <- calibrate_probabilities(
    fixture$prob,
    fixture$event,
    method = "isotonic",
    folds = 5,
    seed = 7
  )
  order_index <- order(fixture$prob)
  reference <- stats::isoreg(
    fixture$prob[order_index],
    fixture$event[order_index]
  )

  expect_equal(calibrated$parameters$fitted$x, reference$x, tolerance = 1e-12)
  expect_equal(calibrated$parameters$fitted$y, reference$yf, tolerance = 1e-12)
})

test_that("calibration reports the Brier score of the calibrated probabilities", {
  fixture <- consistency_fixture()
  calibrated <- calibrate_probabilities(
    fixture$prob,
    fixture$event,
    method = "platt",
    folds = 5,
    seed = 7
  )

  expect_equal(
    calibrated$metrics_before$brier,
    mean((fixture$event - fixture$prob)^2),
    tolerance = 1e-12
  )
  expect_equal(
    calibrated$metrics_after$brier,
    mean((fixture$event - calibrated$calibrated)^2),
    tolerance = 1e-12
  )
  expect_true(all(calibrated$calibrated >= 0 & calibrated$calibrated <= 1))
})

test_that("decision-curve net benefit equals the manual formula", {
  fixture <- consistency_fixture()
  thresholds <- c(0.2, 0.35, 0.5, 0.65)
  curve <- dca(fixture$prob, fixture$event, thresholds = thresholds)
  model <- curve[curve$strategy == "model", , drop = FALSE]
  treat_all <- curve[curve$strategy == "treat_all", , drop = FALSE]
  n <- length(fixture$event)

  manual <- vapply(thresholds, function(cutoff) {
    tp <- sum(fixture$prob >= cutoff & fixture$event == 1L)
    fp <- sum(fixture$prob >= cutoff & fixture$event == 0L)
    tp / n - (fp / n) * (cutoff / (1 - cutoff))
  }, numeric(1))
  manual_all <- vapply(thresholds, function(cutoff) {
    prevalence <- sum(fixture$event == 1L) / n
    prevalence - (1 - prevalence) * (cutoff / (1 - cutoff))
  }, numeric(1))

  expect_equal(model$net_benefit, manual, tolerance = 1e-12)
  expect_equal(treat_all$net_benefit, manual_all, tolerance = 1e-12)
})

test_that("DeLong AUCs agree with calc_metrics()", {
  fixture <- consistency_fixture()
  alternative <- stats::plogis(
    stats::qlogis(pmin(pmax(fixture$prob, 1e-12), 1 - 1e-12)) +
      stats::rnorm(length(fixture$prob), 0, 0.4)
  )
  test <- delong_test(
    list(reference = fixture$prob, alternative = alternative),
    fixture$event
  )
  direct <- vapply(
    list(fixture$prob, alternative),
    function(prob) calc_metrics(prob, fixture$event, threshold = 0.5)$auc,
    numeric(1)
  )

  expect_equal(test$summary$auc, direct, tolerance = 1e-10)
})

test_that("the Hosmer-Lemeshow fallback reproduces ResourceSelection", {
  skip_if_not_installed("ResourceSelection")
  fixture <- consistency_fixture(n = 300L)
  grouped <- hl_test(fixture$prob, fixture$event, g = 10)

  expect_identical(grouped$method, "ResourceSelection")
  # Without ResourceSelection the package falls back to its own grouping; the
  # two paths must report the same statistic, otherwise results would depend on
  # the installed packages.
  fallback <- uniml:::.custom_hl(fixture$prob, fixture$event, grouped$n_groups)
  expect_equal(grouped$statistic, fallback$statistic, tolerance = 1e-10)
  expect_identical(fallback$n_groups, grouped$n_groups)
  expect_equal(fallback$p_value, grouped$p_value, tolerance = 1e-10)
})

test_that("bootstrap intervals centre on the analytic estimate", {
  fixture <- consistency_fixture()
  metrics <- calc_metrics(fixture$prob, fixture$event, threshold = 0.5)
  interval <- calc_ci(
    fixture$prob,
    fixture$event,
    R = 400,
    seed = 99,
    threshold = 0.5,
    metrics = c("auc", "brier")
  )

  expect_equal(
    interval$mean[[1L]],
    metrics$auc,
    tolerance = 0.02
  )
  expect_equal(
    interval$mean[[2L]],
    metrics$brier,
    tolerance = 0.01
  )
  expect_lt(interval$lower[[1L]], metrics$auc)
  expect_gt(interval$upper[[1L]], metrics$auc)
})

test_that("CV summaries are the arithmetic mean of the fold estimates", {
  skip_if_not_installed("rpart")
  data <- make_workflow_data()
  split <- make_workflow_split(data)
  result <- run_workflow(
    make_workflow_spec("strict_out"),
    split$train,
    split$validation
  )
  detailed <- result$cv$metrics

  expect_true(nrow(detailed) > 1L)
  for (model in unique(detailed$model)) {
    folds <- detailed[detailed$model == model, , drop = FALSE]
    summary <- result$cv$summary[
      result$cv$summary$model == model & result$cv$summary$metric == "auc",
      ,
      drop = FALSE
    ]
    expect_equal(summary$mean, mean(folds$auc), tolerance = 1e-12)
    expect_equal(
      summary$se,
      stats::sd(folds$auc) / sqrt(nrow(folds)),
      tolerance = 1e-12
    )
    expect_identical(summary$n, nrow(folds))
    expect_identical(summary$df, as.numeric(nrow(folds) - 1L))
  }
})
