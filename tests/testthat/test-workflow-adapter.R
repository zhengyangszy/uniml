adapter_fixture <- function() {
  list(
    prepare = function(data, spec, name = "data", levels = NULL,
                        require_two = FALSE) {
      .workflow_prepare_data(data, spec, name, levels = levels, require_two = require_two)
    },
    make_task = function(data, spec, features) {
      make_task(data[, c(features, spec$outcome), drop = FALSE], spec)
    },
    predict = function(learner, task, spec) {
      predicted <- .workflow_probabilities(learner, task, spec$positive)
      list(prediction = predicted$prediction, value = predicted$prob)
    },
    pool = function(values) .workflow_mean_prob(values),
    metric_names = function(spec) .ci_metric_names(),
    metrics = function(value, truth, spec, metrics, context = list()) {
      .calc_binary_metrics(
        probs = value,
        truth = truth,
        threshold = context$threshold,
        positive = spec$positive,
        metrics = metrics
      )
    }
  )
}

test_that("the binary adapter declares every hook the engine needs", {
  adapter <- workflow_adapter("binary")
  expect_identical(adapter$task_type, "binary")
  expect_true(all(.workflow_adapter_hooks() %in% names(adapter)))
  for (hook in .workflow_adapter_hooks()) {
    expect_true(is.function(adapter[[hook]]), info = hook)
  }
  expect_identical(adapter$metric_names(NULL), .ci_metric_names())

  expect_error(workflow_adapter("nope"), "No workflow adapter is registered")
  expect_error(workflow_adapter(1), "task_type")
})

test_that("register_workflow_adapter() validates and stores adapters", {
  adapter <- adapter_fixture()

  expect_error(
    register_workflow_adapter("uniml_test_adapter", adapter[setdiff(
      names(adapter), "metrics"
    )]),
    "missing hook"
  )
  broken <- adapter
  broken$pool <- "not a function"
  expect_error(register_workflow_adapter("uniml_test_adapter", broken), "must be a function")
  expect_error(register_workflow_adapter("uniml_test_adapter", "not a list"), "must be a list")

  register_workflow_adapter("uniml_test_adapter", adapter)
  expect_identical(workflow_adapter("uniml_test_adapter")$task_type, "uniml_test_adapter")
  expect_error(
    register_workflow_adapter("uniml_test_adapter", adapter),
    "already registered"
  )
  expect_invisible(register_workflow_adapter("uniml_test_adapter", adapter, overwrite = TRUE))
  expect_error(
    register_workflow_adapter("uniml_test_adapter", adapter, overwrite = "yes"),
    "overwrite"
  )
})

test_that("workflow_engine() fits models and stops before scoring", {
  skip_if_not_installed("rpart")
  data <- make_workflow_data()
  split <- make_workflow_split(data)
  spec <- make_workflow_spec()

  engine <- workflow_engine(spec, split$train, split$validation, split$test)

  expect_identical(engine$adapter$task_type, "binary")
  expect_s3_class(engine$task, "TaskClassif")
  expect_true(length(engine$states) >= 1L)
  expect_true("cart" %in% names(engine$states))
  state <- engine$states[["cart"]][[1L]]
  expect_true(all(c("tuned", "learner", "task", "preprocessor", "selection",
                    "values", "imputation") %in% names(state)))
  expect_setequal(names(state$values), c("train", "validation", "test"))
  expect_identical(length(state$values$validation$value), nrow(split$validation))
  expect_identical(
    length(state$values$validation$truth),
    nrow(split$validation)
  )
  expect_true(all(state$values$validation$value >= 0))
  expect_true(all(state$values$validation$value <= 1))

  # The engine returns predictions, not metrics.
  expect_false("metrics" %in% names(engine))
  expect_s3_class(.failure_table(engine$failures), "data.frame")

  # A missing adapter hook is rejected before any fitting starts.
  expect_error(
    workflow_engine(spec, split$train, split$validation, adapter = list()),
    "workflow adapter"
  )
})

test_that("workflow_engine() uses the adapter supplied to it", {
  skip_if_not_installed("rpart")
  data <- make_workflow_data()
  split <- make_workflow_split(data)
  spec <- make_workflow_spec()

  seen <- new.env(parent = emptyenv())
  seen$predict <- 0L
  adapter <- adapter_fixture()
  original_predict <- adapter$predict
  adapter$predict <- function(learner, task, spec) {
    seen$predict <- seen$predict + 1L
    original_predict(learner, task, spec)
  }
  register_workflow_adapter("uniml_spy_adapter", adapter)

  engine <- workflow_engine(
    spec,
    split$train,
    split$validation,
    adapter = workflow_adapter("uniml_spy_adapter")
  )
  expect_gt(seen$predict, 0L)
  expect_identical(engine$adapter$task_type, "uniml_spy_adapter")
  expect_true(length(engine$states) >= 1L)
})
