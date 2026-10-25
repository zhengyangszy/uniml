make_metrics_fixture <- function() {
  data.frame(
    model = c("a", "b", "c", "a", "b", "c"),
    set = rep(c("validation", "test"), each = 3L),
    auc = c(0.70, 0.82, 0.75, 0.72, 0.80, 0.76),
    sensitivity = c(0.60, 0.75, 0.68, 0.62, 0.74, 0.66),
    specificity = c(0.65, 0.78, 0.70, 0.64, 0.77, 0.69),
    f2 = c(0.58, 0.76, 0.67, 0.60, 0.75, 0.68),
    balanced_accuracy = c(0.625, 0.765, 0.690, 0.630, 0.755, 0.675),
    youden = c(0.25, 0.53, 0.38, 0.26, 0.51, 0.35),
    calibrated = FALSE,
    stringsAsFactors = FALSE
  )
}

test_that("rank_models() selects the comparison set with auto semantics", {
  metrics <- make_metrics_fixture()
  auto <- rank_models(metrics, select_on = "auto", best_model_metric = "auc")
  validation <- rank_models(
    metrics,
    select_on = "validation",
    best_model_metric = "auc"
  )

  # `auto` prefers validation; the test set is reporting-only.
  expect_identical(unique(auto$set), "validation")
  expect_identical(unique(validation$set), "validation")
  expect_identical(auto$model[auto$rank == 1L], "b")
  expect_identical(validation$model[validation$rank == 1L], "b")

  test_only <- rank_models(
    metrics[metrics$set == "test", , drop = FALSE],
    select_on = "auto",
    best_model_metric = "auc"
  )
  expect_identical(unique(test_only$set), "test")
})

test_that("rank_models() handles composite, lower-is-better, and missing metrics", {
  metrics <- make_metrics_fixture()
  metrics$brier <- c(0.2, 0.1, 0.15, 0.2, 0.1, 0.15)
  composite <- rank_models(metrics, best_model_metric = "composite")
  brier <- rank_models(metrics, best_model_metric = "brier")
  unavailable <- rank_models(metrics, best_model_metric = "missing_metric")

  expect_identical(composite$model[composite$rank == 1L], "b")
  expect_identical(brier$model[brier$rank == 1L], "b")
  expect_true(all(is.na(unavailable$score)))
  expect_true(all(is.na(unavailable$rank)))

  all_missing <- metrics
  all_missing[, c("auc", "sensitivity", "specificity", "f2",
                  "balanced_accuracy", "youden")] <- NA_real_
  ranked <- rank_models(all_missing, best_model_metric = "composite")
  expect_true(all(is.na(ranked$score)))
  expect_true(all(is.na(ranked$rank)))
})

test_that("rank_models() uses the composite metric set of the task type", {
  metrics <- data.frame(
    model = c("a", "b"),
    set = "validation",
    auc = c(0.90, 0.84),
    accuracy = c(0.70, 0.88),
    balanced_accuracy = c(0.86, 0.80),
    f1 = c(0.66, 0.86),
    stringsAsFactors = FALSE
  )
  composite <- c("auc", "accuracy", "balanced_accuracy", "f1")
  expected <- rowMeans(metrics[, composite])

  ranked <- rank_models(metrics, task_type = "multiclass")
  expect_equal(ranked$score, expected[match(ranked$model, metrics$model)])
  expect_identical(ranked$model[ranked$rank == 1L], "b")

  # The binary set averages a different subset of these columns, which is why
  # the task type has to travel with the metrics.
  binary <- rank_models(metrics)
  expect_identical(binary$model[binary$rank == 1L], "a")

  # A `uniml_result` carries the task type in `data_info`.
  result <- assemble_result(
    data_info = list(task_type = "multiclass", outcome = "y", n_train = 100L),
    metrics = metrics
  )
  expect_equal(rank_models(result)$score, ranked$score)
  expect_identical(result$ranking$score, ranked$score)

  # A registered task type that declares no composite set asks for an explicit
  # metric instead of quietly ranking on the binary one.
  register_task_type(task_type_spec(
    id = "uniml_no_composite_type", mlr3_task = "classif"
  ))
  expect_error(
    rank_models(metrics, task_type = "uniml_no_composite_type"),
    "no composite metric"
  )
})

test_that("rank_models() validates explicit missing comparison sets", {
  metrics <- make_metrics_fixture()
  expect_error(
    rank_models(metrics, select_on = "validation"),
    NA
  )
  metrics <- metrics[metrics$set == "validation", , drop = FALSE]
  expect_error(rank_models(metrics, select_on = "test"), "not present")
  expect_error(rank_models(metrics, select_on = "other"), "select_on")
})

test_that("assemble_result() tolerates missing stages and ranks metrics", {
  result <- assemble_result(
    data_info = list(n = 10L),
    metrics = make_metrics_fixture()
  )

  expect_s3_class(result, "uniml_result")
  expect_null(result$preprocess)
  expect_s3_class(result$ranking, "data.frame")
  expect_identical(result$ranking$model[result$ranking$rank == 1L], "b")
  expect_true(is.list(result$meta$session_info))
})

test_that("export_results() writes the output layout and skips missing stages", {
  root <- tempfile("uniml-export-")
  calibration <- structure(
    list(
      method = "platt",
      calibrated = c(0.2, 0.8),
      raw = c(0.3, 0.7),
      parameters = list(fitted = list(type = "platt")),
      metrics_before = data.frame(brier = 0.2, ece = 0.1),
      metrics_after = data.frame(brier = 0.15, ece = 0.08),
      failed = FALSE,
      n = 2L
    ),
    class = c("uniml_calibration", "list")
  )
  result <- assemble_result(
    data_info = list(n = 10L),
    preprocess = list(var_info = data.frame(column = "x", type = "continuous")),
    selection = list(selected_features = c("x1", "x2")),
    tuning = data.frame(model = "cart", best_metric = 0.8),
    cv = list(summary = data.frame(model = "cart", auc = 0.8)),
    metrics = make_metrics_fixture(),
    ci = data.frame(metric = "auc", mean = 0.8, lower = 0.7, upper = 0.9),
    calibration = calibration,
    tests = list(delong = data.frame(model = "a", auc = 0.8)),
    dca = data.frame(threshold = 0.2, net_benefit = 0.1),
    thresholds = list(
      zero_crossings = data.frame(feature = "x1", crossing = 0.4),
      paper_ready = data.frame(feature = "x1", threshold = 0.4)
    ),
    shap = list(importance = data.frame(feature = "x1", importance = 0.2)),
    models = list(cart = mlr3::lrn("classif.rpart", predict_type = "prob"))
  )
  exported <- export_results(result, dir = root, include_plots = FALSE)

  expect_true(dir.exists(file.path(root, "preprocess")))
  expect_true(file.exists(file.path(root, "result.rds")))
  expect_true(file.exists(file.path(root, "performance", "before_after_calibration.csv")))
  expect_true(file.exists(file.path(root, "performance", "metrics_test.csv")))
  expect_true(file.exists(file.path(root, "models", "final_cart.rds")))
  expect_true(file.exists(file.path(root, "reporting", "tripod_ai.csv")))
  expect_true("shap" %in% names(exported$skipped) == FALSE)
  expect_false("plots" %in% exported$skipped)

  calibration_csv <- utils::read.csv(
    file.path(root, "performance", "before_after_calibration.csv"),
    fileEncoding = "UTF-8"
  )
  expect_true(all(c("stage", "brier", "ece") %in% names(calibration_csv)))
  expect_setequal(calibration_csv$stage, c("before", "after"))

  test_metrics <- utils::read.csv(
    file.path(root, "performance", "metrics_test.csv"),
    fileEncoding = "UTF-8"
  )
  expect_setequal(test_metrics$set, "test")

  manifest <- readLines(file.path(root, "export_manifest.csv"), warn = FALSE)
  expect_true(any(grepl("ranking", manifest, fixed = TRUE)))
  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("export_results() validates arguments and reaches export goals", {
  result <- assemble_result()
  expect_error(export_results(result, dir = 1), "dir")
  expect_error(export_results(result, include_plots = NA), "include_plots")
})
