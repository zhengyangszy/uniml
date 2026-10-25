test_that("task_type_spec() builds and validates descriptors", {
  spec <- task_type_spec(
    id = "binary", mlr3_task = "classif", outcome_class = "factor/0-1",
    positive = TRUE, metrics = c("auc", "brier"),
    default_learners = c("lr", "rf"), supports_shap = TRUE
  )
  expect_s3_class(spec, "uniml_task_type")
  expect_true(validate_uniml_task_type(spec))
  expect_identical(spec$id, "binary")
  expect_true(spec$supports_shap)
  expect_false(spec$supports_dca)
  expect_true(spec$implemented)
  expect_identical(unclass(print(spec)), unclass(spec))

  expect_error(task_type_spec(id = ""), "id")
  expect_error(task_type_spec(id = "x", positive = NA), "positive")
  expect_error(task_type_spec(id = "x", metrics = 1), "metrics")
  expect_error(task_type_spec(id = "x", default_learners = NA_character_),
               "default_learners")
  expect_error(task_type_spec(id = "x", implemented = "yes"), "implemented")
  expect_error(validate_uniml_task_type(list(id = "x")), "uniml_task_type")
})

test_that("default task types are registered", {
  ids <- list_task_types()
  expect_true(all(c("binary", "multiclass", "survival") %in% ids))
  expect_identical(ids, sort(unique(ids)))

  binary <- get_task_type("binary")
  expect_s3_class(binary, "uniml_task_type")
  expect_true(binary$implemented)
  expect_identical(binary$mlr3_task, "classif")
  expect_identical(
    binary$default_learners,
    c("lr", "rf", "xgb", "nnet", "knn", "cart", "nb")
  )
  expect_true(all(c("auc", "pr_auc", "brier", "nri", "idi") %in% binary$metrics))
  expect_true(binary$supports_shap)
  expect_true(binary$supports_dca)
  expect_true(binary$supports_calibration)

  expect_true("binary" %in% list_task_types(implemented_only = TRUE))
  expect_false("multiclass" %in% list_task_types(implemented_only = TRUE))
  expect_false("survival" %in% list_task_types(implemented_only = TRUE))
  expect_false(get_task_type("multiclass")$implemented)
  expect_false(get_task_type("survival")$implemented)

  expect_error(get_task_type("nope"), "Unknown task type")
  expect_error(get_task_type(123), "id")
  expect_error(list_task_types(implemented_only = "yes"), "implemented_only")
})

test_that("registered task types drive spec validation", {
  expect_error(
    uniml_spec(task_type = "multiclass", outcome = "y", positive = "Yes"),
    "task_type"
  )
  expect_error(
    uniml_spec(task_type = "survival", outcome = "y", positive = "Yes"),
    "task_type"
  )

  register_task_type(task_type_spec(
    id = "uniml_test_type", mlr3_task = "classif", implemented = TRUE
  ))
  spec <- uniml_spec(task_type = "uniml_test_type", outcome = "y", positive = "Yes")
  expect_identical(spec$task_type, "uniml_test_type")
  expect_true("uniml_test_type" %in% list_task_types(implemented_only = TRUE))
})

test_that("register_task_type() enforces overwrite", {
  register_task_type(task_type_spec(id = "uniml_dup_type", mlr3_task = "classif"))
  expect_error(
    register_task_type(task_type_spec(id = "uniml_dup_type", mlr3_task = "classif")),
    "already registered"
  )
  replacement <- task_type_spec(
    id = "uniml_dup_type", mlr3_task = "classif", implemented = FALSE
  )
  expect_invisible(register_task_type(replacement, overwrite = TRUE))
  expect_false(get_task_type("uniml_dup_type")$implemented)
})

test_that("analysis generics dispatch by task type", {
  binary <- structure(
    list(
      prob = c(0.2, 0.8, 0.6, 0.3),
      truth = factor(c("no", "yes", "yes", "no"), levels = c("no", "yes")),
      positive = "yes"
    ),
    class = c("uniml_binary", "uniml_prediction")
  )
  other <- structure(list(), class = "uniml_other")

  expect_false(is.null(getS3method("calc_metrics", "uniml_binary", optional = TRUE)))
  expect_false(is.null(getS3method("calibrate_probabilities", "uniml_binary",
                                   optional = TRUE)))
  expect_false(is.null(getS3method("dca", "uniml_binary", optional = TRUE)))
  expect_false(is.null(getS3method("explain_shap", "uniml_binary", optional = TRUE)))

  expect_identical(calc_metrics(binary, threshold = 0.5)$n_samples, 4L)
  expect_s3_class(
    calibrate_probabilities(binary, method = "none"),
    "uniml_calibration"
  )
  expect_s3_class(dca(binary), "uniml_dca")
  expect_error(explain_shap(binary), "trained mlr3 learners")

  expect_error(calc_metrics(other), "no method")
  expect_error(calibrate_probabilities(other), "no method")
  expect_error(dca(other), "no method")
  expect_error(explain_shap(other), "no method")
})

test_that("plotting generics dispatch by task type", {
  other <- structure(list(), class = "uniml_other")
  performance_generics <- c(
    "plot_roc", "plot_calibration", "plot_dca", "plot_confusion"
  )
  shap_generics <- c(
    "plot_shap_importance", "plot_shap_beeswarm", "plot_shap_dependence",
    "plot_shap_waterfall", "plot_shap_force"
  )

  for (generic in c(performance_generics, shap_generics)) {
    expect_false(
      is.null(getS3method(generic, "uniml_binary", optional = TRUE)),
      info = generic
    )
    expect_error(do.call(generic, list(other)), "no method", info = generic)
  }

  predictions <- structure(
    list(
      prob = c(0.2, 0.8, 0.6, 0.3),
      truth = factor(c("no", "yes", "yes", "no"), levels = c("no", "yes")),
      positive = "yes"
    ),
    class = c("uniml_binary", "uniml_prediction")
  )
  for (generic in performance_generics) {
    expect_true(
      inherits(do.call(generic, list(predictions)), "ggplot"),
      info = generic
    )
  }

  skip_if_not_installed("shapviz")
  shap_fixture <- shapviz::shapviz(
    matrix(
      c(0.4, -0.2, 0.1, 0.3, -0.5, 0.2, 0.6, 0.1, -0.3, 0.2, 0.5, -0.1),
      nrow = 4L,
      dimnames = list(NULL, c("x1", "x2", "x3"))
    ),
    data.frame(x1 = 1:4, x2 = c(0, 1, 0, 1), x3 = c(2, 4, 6, 8))
  )
  for (generic in shap_generics) {
    expect_true(inherits(do.call(generic, list(shap_fixture)), "ggplot"),
                info = generic)
  }
  for (generic in shap_generics) {
    expect_error(
      do.call(generic, list(structure(list(), class = c("uniml_binary", "uniml_prediction")))),
      "binary",
      info = generic
    )
  }
})
