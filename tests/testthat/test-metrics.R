test_that("calc_metrics() returns the complete binary metric set", {
  probs <- c(0.1, 0.4, 0.35, 0.8)
  truth <- factor(c("no", "no", "yes", "yes"), levels = c("no", "yes"))

  metrics <- calc_metrics(
    probs,
    truth,
    threshold = 0.5,
    positive = "yes"
  )

  expect_true(all(c(
    "auc", "pr_auc", "brier", "accuracy", "sensitivity", "specificity",
    "ppv", "npv", "f1", "f2", "youden", "plr", "nlr", "dor",
    "balanced_accuracy", "kappa", "tp", "tn", "fp", "fn",
    "threshold", "n_samples"
  ) %in% names(metrics)))
  expect_equal(metrics$auc, 0.75, tolerance = 1e-12)
  expect_equal(metrics$accuracy, 0.75, tolerance = 1e-12)
  expect_equal(metrics$sensitivity, 0.5, tolerance = 1e-12)
  expect_equal(metrics$specificity, 1, tolerance = 1e-12)
  expect_equal(metrics$ppv, 1, tolerance = 1e-12)
  expect_equal(metrics$npv, 2 / 3, tolerance = 1e-12)
  expect_equal(metrics$f1, 2 / 3, tolerance = 1e-12)
  expect_equal(metrics$kappa, 0.5, tolerance = 1e-12)
  expect_identical(metrics$tp, 1L)
  expect_identical(metrics$tn, 2L)
  expect_identical(metrics$fp, 0L)
  expect_identical(metrics$fn, 1L)
  expect_identical(metrics$n_samples, 4L)
})

test_that("calc_metrics() handles single-class inputs", {
  metrics <- calc_metrics(c(0.1, 0.2), c(0, 0), threshold = 0.5)

  expect_true(is.na(metrics$auc))
  expect_true(is.na(metrics$pr_auc))
  expect_identical(metrics$n_samples, 2L)
  expect_true(is.finite(metrics$accuracy))
})

test_that("find_threshold() supports fixed, Youden, and cost methods", {
  probs <- c(0.1, 0.4, 0.6, 0.9)
  labels <- c(0, 0, 1, 1)
  cost <- c(fp = 2, fn = 1)

  expect_identical(
    find_threshold(probs, labels, method = "fixed", fixed_threshold = 0.7),
    0.7
  )
  expect_equal(find_threshold(probs, labels, method = "youden"), 0.6)
  expect_true(is.finite(
    find_threshold(probs, labels, method = "cost", cost = cost)
  ))
  expect_true(is.na(find_threshold(c(0.1, 0.2), c(0, 0), method = "youden")))
})

test_that("calc_metrics() dispatches for numeric and uniml_binary inputs", {
  probs <- c(0.2, 0.8, 0.6, 0.3)
  truth <- factor(c("no", "yes", "yes", "no"), levels = c("no", "yes"))
  binary <- structure(
    list(prob = probs, truth = truth, positive = "yes"),
    class = c("uniml_binary", "uniml_prediction")
  )

  from_numeric <- calc_metrics(probs, truth, threshold = 0.5)
  from_binary <- calc_metrics(binary, threshold = 0.5)

  expect_equal(from_numeric$accuracy, from_binary$accuracy)
  expect_identical(from_binary$n_samples, 4L)
  expect_error(calc_metrics(probs), "truth")
  expect_error(calc_metrics(binary, metrics = "unknown"), "unsupported")
})

test_that("calc_metrics() works with an mlr3 PredictionClassif", {
  set.seed(42)
  data <- data.frame(
    y = factor(rep(c("no", "yes"), each = 20), levels = c("no", "yes")),
    x1 = stats::rnorm(40),
    x2 = stats::rnorm(40)
  )
  data$x1 <- ifelse(data$y == "yes", 1, -1) + data$x1
  spec <- uniml_spec(task_type = "binary", outcome = "y", positive = "yes")
  task <- make_task(data, spec)
  learner <- train_model(
    task,
    mlr3::lrn("classif.rpart", predict_type = "prob")
  )
  prediction <- learner$predict(task)

  metrics <- calc_metrics(prediction, threshold = 0.5)

  expect_s3_class(prediction, "PredictionClassif")
  expect_true("accuracy" %in% names(metrics))
  expect_identical(metrics$n_samples, 40L)
})

test_that("calc_nri_idi() calculates continuous and categorical NRI", {
  truth <- c(0, 1, 1, 0)
  new <- c(0.2, 0.7, 0.8, 0.1)
  reference <- c(0.3, 0.6, 0.7, 0.2)

  continuous <- calc_nri_idi(new, reference, truth)
  categorical <- calc_nri_idi(
    new,
    reference,
    truth,
    method = "categorical",
    threshold_new = 0.5,
    threshold_reference = 0.5
  )

  expect_equal(continuous$nri_events, 1, tolerance = 1e-12)
  expect_equal(continuous$nri_nonevents, 1, tolerance = 1e-12)
  expect_equal(continuous$nri, 2, tolerance = 1e-12)
  expect_equal(continuous$idi, 0.2, tolerance = 1e-12)
  expect_equal(categorical$nri, 0, tolerance = 1e-12)
  expect_true(all(c("nri", "idi", "n") %in% names(continuous)))
  expect_error(
    calc_nri_idi(new, reference, truth, method = "bad"),
    "one of"
  )
})
