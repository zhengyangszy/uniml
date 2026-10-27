multiclass_data <- function(n = 120L, seed = 2026L) {
  set.seed(seed)
  outcome <- factor(
    sample(c("low", "mid", "high"), n, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
    levels = c("low", "mid", "high")
  )
  shift <- c(low = -1, mid = 0, high = 1)[as.character(outcome)]
  data.frame(
    y = outcome,
    x1 = stats::rnorm(n) + shift,
    x2 = stats::rnorm(n),
    # A factor, not a character: mlr3 tasks reject character predictors, and the
    # layer-level API does not encode them for you.
    cat = factor(sample(c("a", "b"), n, replace = TRUE))
  )
}

multiclass_prediction <- function(seed = 7L) {
  set.seed(seed)
  prob <- matrix(
    stats::runif(60L * 3L, 0, 1),
    nrow = 60L,
    dimnames = list(NULL, c("a", "b", "c"))
  )
  prob <- prob / rowSums(prob)
  truth <- sample(colnames(prob), 60L, replace = TRUE)
  as_uniml_multiclass(prob, truth = truth)
}

test_that("a multiclass specification needs no positive class", {
  spec <- uniml_spec(task_type = "multiclass", outcome = "y")
  expect_identical(spec$task_type, "multiclass")
  expect_null(spec$positive)
  expect_true("multiclass" %in% list_task_types(implemented_only = TRUE))

  # A positive class only exists for binary outcomes.
  expect_error(
    uniml_spec(task_type = "multiclass", outcome = "y", positive = "low"),
    "must be NULL"
  )
  # Binary still requires one.
  expect_error(
    uniml_spec(task_type = "binary", outcome = "y"),
    "`positive` must be a single non-missing value"
  )
})

test_that("as_uniml_data() keeps every class and sorts the levels", {
  data <- multiclass_data()
  spec <- uniml_spec(task_type = "multiclass", outcome = "y")
  prepared <- as_uniml_data(data, spec)

  expect_s3_class(prepared, "uniml_data")
  expect_identical(levels(prepared$outcome), c("high", "low", "mid"))
  expect_null(prepared$positive)
  expect_identical(nlevels(prepared$outcome), 3L)
  expect_identical(sum(prepared$role == "outcome"), 1L)

  single <- data[data$y == "low", , drop = FALSE]
  expect_error(
    as_uniml_data(single, spec),
    "at least two non-missing classes"
  )
})

test_that("make_task() builds a multiclass task without a positive class", {
  skip_if_not_installed("rpart")
  data <- multiclass_data()
  spec <- uniml_spec(task_type = "multiclass", outcome = "y")
  task <- make_task(data, spec)

  expect_s3_class(task, "TaskClassif")
  expect_identical(task$class_names, c("low", "mid", "high"))
  expect_true(is.na(task$positive))
  expect_identical(task$target_names, "y")
})

test_that("multiclass metrics match mlr3measures", {
  prediction <- multiclass_prediction()
  metrics <- calc_metrics(prediction)
  truth <- factor(prediction$truth, levels = prediction$classes)

  expect_equal(
    metrics$auc,
    mlr3measures::mauc_au1u(truth, prediction$prob),
    tolerance = 1e-12
  )
  expect_equal(
    metrics$brier,
    mlr3measures::mbrier(truth, prediction$prob),
    tolerance = 1e-12
  )
  expect_equal(
    metrics$logloss,
    mlr3measures::logloss(truth, prediction$prob),
    tolerance = 1e-12
  )
  # Recompute the predicted class independently: argmax of the probability row.
  predicted <- factor(
    prediction$classes[max.col(prediction$prob, ties.method = "first")],
    levels = prediction$classes
  )
  expect_equal(
    metrics$accuracy,
    mlr3measures::acc(truth, predicted),
    tolerance = 1e-12
  )
  expect_identical(metrics$n_samples, 60L)
  expect_identical(metrics$n_classes, 3L)
})

test_that("the confusion table and its per-class rates are self-consistent", {
  prediction <- multiclass_prediction()
  metrics <- calc_metrics(prediction)
  confusion <- metrics$confusion
  per_class <- metrics$per_class

  expect_true(is.matrix(confusion))
  expect_identical(dim(confusion), c(3L, 3L))
  expect_identical(sum(confusion), 60)
  expect_identical(rownames(confusion), prediction$classes)
  expect_identical(colnames(confusion), prediction$classes)

  # Recompute the cells from the raw vectors instead of reusing them.
  predicted <- prediction$classes[
    max.col(prediction$prob, ties.method = "first")
  ]
  reference <- table(
    factor(prediction$truth, levels = prediction$classes),
    factor(predicted, levels = prediction$classes)
  )
  expect_equal(unname(confusion), unname(as.matrix(reference)))

  expect_equal(metrics$accuracy, sum(diag(confusion)) / 60L, tolerance = 1e-12)
  expect_equal(
    metrics$balanced_accuracy,
    mean(diag(confusion) / rowSums(confusion)),
    tolerance = 1e-12
  )
  expect_equal(
    metrics$sensitivity,
    mean(per_class$sensitivity),
    tolerance = 1e-12
  )
  # Macro recall is the balanced accuracy by definition.
  expect_equal(metrics$sensitivity, metrics$balanced_accuracy, tolerance = 1e-12)

  n <- sum(confusion)
  chance <- sum(rowSums(confusion) * colSums(confusion)) / n^2
  expect_equal(
    metrics$kappa,
    (metrics$accuracy - chance) / (1 - chance),
    tolerance = 1e-12
  )

  # One-vs-rest cells add up to the support of each class.
  expect_identical(per_class$class, prediction$classes)
  expect_equal(per_class$tp + per_class$fn, as.numeric(rowSums(confusion)))
  expect_equal(per_class$tp + per_class$fp, as.numeric(colSums(confusion)))
})

test_that("the three multiclass AUCs are three different estimands", {
  set.seed(31)
  n <- 400L
  classes <- c("low", "mid", "high")
  truth <- factor(
    sample(classes, n, replace = TRUE, prob = c(0.7, 0.2, 0.1)),
    levels = classes
  )
  prob <- matrix(stats::runif(n * 3L), nrow = n,
                 dimnames = list(NULL, classes))
  # A weak signal that favours the common class, so macro and micro disagree.
  for (index in seq_along(classes)) {
    prob[, index] <- prob[, index] +
      c(0.25, 0.15, 0.05)[[index]] * (as.character(truth) == classes[[index]])
  }
  prob <- prob / rowSums(prob)
  metrics <- calc_metrics(prob, truth)

  # `auc` is Hand and Till: whatever mlr3measures computes, unchanged.
  expect_equal(
    metrics$auc,
    mlr3measures::mauc_au1u(truth, prob),
    tolerance = 1e-12
  )
  # `per_class$auc` is one-vs-rest, and `auc_ovr` is its unweighted mean.
  expect_length(metrics$per_class$auc, 3L)
  expect_equal(metrics$auc_ovr, mean(metrics$per_class$auc), tolerance = 1e-12)
  # Hand and Till is not the one-vs-rest average; if it ever becomes one, the
  # documentation above is wrong and this test should fail.
  expect_false(isTRUE(all.equal(metrics$auc, metrics$auc_ovr, tolerance = 1e-6)))
  # Micro pools every (patient, class) pair and differs from both.
  expect_true(metrics$micro_auc > 0 && metrics$micro_auc < 1)
  expect_false(isTRUE(all.equal(metrics$micro_auc, metrics$auc, tolerance = 1e-6)))

  # Micro precision, recall, and F1 are all the accuracy; that is why they are
  # not separate columns.
  cells <- metrics$per_class
  micro_precision <- sum(cells$tp) / sum(cells$tp + cells$fp)
  micro_recall <- sum(cells$tp) / sum(cells$tp + cells$fn)
  micro_f1 <- 2 * sum(cells$tp) /
    (2 * sum(cells$tp) + sum(cells$fp) + sum(cells$fn))
  expect_equal(micro_precision, metrics$accuracy, tolerance = 1e-12)
  expect_equal(micro_recall, metrics$accuracy, tolerance = 1e-12)
  expect_equal(micro_f1, metrics$accuracy, tolerance = 1e-12)
  # Micro specificity is its own number.
  expect_equal(
    metrics$micro_specificity,
    sum(cells$tn) / sum(cells$tn + cells$fp),
    tolerance = 1e-12
  )
  expect_false(isTRUE(all.equal(metrics$micro_specificity, metrics$accuracy)))

  expect_true(all(c("auc", "auc_ovr", "micro_auc", "micro_specificity") %in%
                    .multiclass_metric_names()))
})

test_that("multiclass metric input is validated", {
  prediction <- multiclass_prediction()

  broken <- prediction
  broken$prob[1L, ] <- c(0.5, 0.4, 0.4)
  expect_error(calc_metrics(broken), "must sum to 1")

  unnamed <- prediction
  colnames(unnamed$prob) <- NULL
  expect_error(calc_metrics(unnamed), "column names")

  unknown <- prediction
  unknown$truth[1L] <- "z"
  expect_error(calc_metrics(unknown), "missing from")
})

test_that("as_uniml_multiclass() refuses binary input", {
  prob <- cbind(a = c(0.7, 0.2), b = c(0.3, 0.8))
  expect_error(
    as_uniml_multiclass(prob, truth = c("a", "b")),
    "three or more classes"
  )
})

test_that("calc_metrics() picks the multiclass path for a wide prediction", {
  skip_if_not_installed("rpart")
  data <- multiclass_data()
  spec <- uniml_spec(task_type = "multiclass", outcome = "y")
  task <- make_task(data, spec)
  learner <- train_model(task, mlr3::lrn("classif.rpart", predict_type = "prob"))
  prediction <- learner$predict(task)

  metrics <- calc_metrics(prediction)

  expect_true(all(c("accuracy", "kappa", "logloss", "confusion") %in% names(metrics)))
  expect_false("threshold" %in% names(metrics))
  expect_identical(metrics$n_classes, 3L)
  # The binary-only arguments are ignored rather than silently misinterpreted.
  expect_equal(
    calc_metrics(prediction, threshold = 0.3)$accuracy,
    metrics$accuracy,
    tolerance = 0
  )

  bridged <- calc_metrics(as_uniml_multiclass(prediction))
  expect_equal(bridged$accuracy, metrics$accuracy, tolerance = 1e-12)
  expect_equal(bridged$auc, metrics$auc, tolerance = 1e-12)
})

test_that("multiclass plots work and unsupported figures say so", {
  skip_if_not_installed("rpart")
  data <- multiclass_data()
  spec <- uniml_spec(task_type = "multiclass", outcome = "y")
  task <- make_task(data, spec)
  learner <- train_model(task, mlr3::lrn("classif.rpart", predict_type = "prob"))
  prediction <- as_uniml_multiclass(learner$predict(task))

  expect_s3_class(plot_roc(prediction), "ggplot")
  expect_s3_class(plot_confusion(prediction), "ggplot")
  expect_error(plot_calibration(prediction), "no multiclass method yet")
  expect_error(plot_dca(prediction), "not defined for multiclass")
})

test_that("tuning picks a measure that exists for the task's class count", {
  expect_identical(
    uniml:::.tuning_measure_for("auc", n_classes = 3L)$id,
    "classif.mauc_au1u"
  )
  expect_identical(
    uniml:::.tuning_measure_for("brier", n_classes = 3L)$id,
    "classif.mbrier"
  )
  expect_identical(
    uniml:::.tuning_measure_for("auc", n_classes = 2L)$id,
    "classif.auc"
  )
  # Two-class-only ratios are refused rather than silently mistranslated.
  expect_error(
    uniml:::.tuning_measure_for("pr_auc", n_classes = 3L),
    "not defined for multiclass"
  )
  expect_error(
    uniml:::.tuning_measure_for("sensitivity", n_classes = 4L),
    "not defined for multiclass"
  )
})

test_that("tune_model() runs on a multiclass task", {
  skip_if_not_installed("rpart")
  data <- multiclass_data()
  spec <- uniml_spec(task_type = "multiclass", outcome = "y")
  spec <- set_learners(spec, "cart", tune = TRUE)
  task <- make_task(data, spec)
  learners <- resolve_learners(spec, task = task)

  tuned <- tune_model(
    task,
    learners[["cart"]],
    resampling = mlr3::rsmp("cv", folds = 2L),
    tuner = "random_search",
    terminator = mlr3tuning::trm("evals", n_evals = 2L),
    seed = 1
  )

  expect_identical(tuned$status, "tuned")
  expect_identical(tuned$measure$id, "classif.mauc_au1u")
  expect_true(is.finite(tuned$best_metric))
})

test_that("run_workflow() refuses multiclass with a pointer to the layers", {
  data <- multiclass_data()
  spec <- uniml_spec(task_type = "multiclass", outcome = "y")

  expect_error(
    run_workflow(spec, data[1:80, ], data[81:120, ]),
    "binary"
  )
})

test_that("the workflow metric adapter is the only task-type seam", {
  # Binary goes through the registered binary adapter.
  binary_spec <- uniml_spec(task_type = "binary", outcome = "y", positive = "yes")
  values <- .workflow_metric_values(
    probs = c(0.1, 0.4, 0.6, 0.9),
    truth = c("no", "no", "yes", "yes"),
    threshold = 0.5,
    spec = binary_spec,
    metrics = c("auc", "accuracy")
  )
  expect_identical(names(values), c("auc", "accuracy"))
  expect_equal(unname(values$accuracy), 1)

  # A task type without an adapter fails loudly and names the registration
  # function, instead of returning binary numbers for a multiclass prediction.
  multiclass_spec <- uniml_spec(task_type = "multiclass", outcome = "y")
  expect_error(
    .workflow_metric_values(
      probs = c(0.1, 0.4, 0.6, 0.9),
      truth = c("a", "b", "c", "a"),
      threshold = NA_real_,
      spec = multiclass_spec,
      metrics = "auc"
    ),
    "No workflow adapter is registered"
  )
})

test_that("calc_ci() bootstraps the multiclass metrics from a probability matrix", {
  fixture <- multiclass_prediction()
  ci <- calc_ci(fixture$prob, fixture$truth, R = 40L, level = 0.95, seed = 7)

  expect_identical(nrow(ci), length(.multiclass_metric_names()))
  expect_identical(ci$metric, .multiclass_metric_names())
  expect_true(all(c("metric", "mean", "se", "lower", "upper", "n_effective",
                    "formatted") %in% names(ci)))
  expect_true(all(ci$lower <= ci$upper, na.rm = TRUE))
  expect_true(all(ci$n_effective > 0L))
  # The point estimate the interval is built around is the one `calc_metrics()`
  # reports for the same predictions.
  point <- calc_metrics(fixture$prob, fixture$truth)
  expect_equal(ci$mean[ci$metric == "accuracy"], point$accuracy, tolerance = 0.1)

  # Asking for a subset keeps the order and the names.
  few <- calc_ci(fixture$prob, fixture$truth, R = 10L, seed = 1,
                 metrics = c("brier", "auc"))
  expect_identical(few$metric, c("brier", "auc"))
  expect_error(
    calc_ci(fixture$prob, fixture$truth, R = 5L, metrics = "sensitivity_typo"),
    "unsupported"
  )
})

