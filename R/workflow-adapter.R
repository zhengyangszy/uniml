# L9a task-type adapter -------------------------------------------------------
#
# The workflow engine fits models and predicts data sets; it never decides what a
# prediction means or how it becomes a number. Those operations, plus the data
# preparation and the mlr3 task, belong to the task type and are supplied through
# an adapter object.
#
# `uniml` registers the binary adapter below. `unimlsurv` registers the survival
# one with `register_workflow_adapter()`, which is why the engine can be reused
# without editing it.

.workflow_adapters <- new.env(parent = emptyenv())

.workflow_adapter_hooks <- function() {
  c("prepare", "make_task", "predict", "pool", "metric_names", "metrics")
}

#' Register a workflow adapter for a task type
#'
#' The workflow engine ([workflow_engine()]) is shared across task types. What a
#' task type contributes is an adapter: how its data frame is prepared, how its
#' mlr3 task is built, how a learner's prediction is extracted, how predictions
#' from several imputations are pooled, and how a prediction becomes metrics.
#'
#' An adapter is a list with these elements:
#'
#' * `prepare(data, spec, name, levels, require_two)`: returns
#'   `list(data, outcome, levels)`, like the internal binary preparation does.
#' * `make_task(data, spec)`: returns an mlr3 task.
#' * `predict(learner, task, spec)`: returns `list(prediction, value)`, where
#'   `value` is the object the metrics are computed from (a numeric vector for
#'   binary outcomes, a matrix of survival probabilities for survival ones).
#' * `pool(values)`: combines a list of `value` objects, one per imputation.
#' * `metric_names(spec)`: the metric names the workflow reports.
#' * `metrics(value, truth, spec, metrics, context)`: returns a named list.
#'   `context` carries stage-specific information such as the threshold.
#'
#' @param task_type Task-type id, such as `"binary"`.
#' @param adapter A list of adapter functions. See Details.
#' @param overwrite Whether to replace an existing registration.
#'
#' @return Invisibly returns `adapter`.
#' @export
#' @examples
#' hooks <- names(workflow_adapter("binary"))
#' hooks
register_workflow_adapter <- function(task_type, adapter, overwrite = FALSE) {
  .assert_scalar_character(task_type, "task_type")
  .assert_scalar_logical(overwrite, "overwrite")
  if (!is.list(adapter)) {
    .abort("`adapter` must be a list of functions.")
  }
  missing_hooks <- setdiff(.workflow_adapter_hooks(), names(adapter))
  if (length(missing_hooks) > 0L) {
    .abort(sprintf(
      "The workflow adapter for \"%s\" is missing hook(s): %s.",
      task_type,
      paste(missing_hooks, collapse = ", ")
    ))
  }
  for (hook in .workflow_adapter_hooks()) {
    if (!is.function(adapter[[hook]])) {
      .abort(sprintf("The workflow adapter hook \"%s\" must be a function.", hook))
    }
  }
  adapter$task_type <- task_type
  if (exists(task_type, envir = .workflow_adapters, inherits = FALSE) && !overwrite) {
    .abort(sprintf(
      "A workflow adapter for \"%s\" is already registered; set `overwrite = TRUE` to replace it.",
      task_type
    ))
  }
  assign(task_type, adapter, envir = .workflow_adapters)
  invisible(adapter)
}

#' Get the workflow adapter of a task type
#'
#' @param task_type Task-type id, such as `"binary"`.
#'
#' @return The registered adapter list.
#' @export
#' @examples
#' workflow_adapter("binary")$task_type
workflow_adapter <- function(task_type = "binary") {
  .assert_scalar_character(task_type, "task_type")
  if (!exists(task_type, envir = .workflow_adapters, inherits = FALSE)) {
    .abort(sprintf(
      "No workflow adapter is registered for task type \"%s\". Registered task types: %s.",
      task_type,
      paste(sort(ls(envir = .workflow_adapters)), collapse = ", ")
    ))
  }
  get(task_type, envir = .workflow_adapters, inherits = FALSE)
}

.workflow_binary_adapter <- function() {
  list(
    prepare = function(data, spec, name = "data", levels = NULL,
                        require_two = identical(name, "train")) {
      prepared <- .workflow_prepare_data(
        data,
        spec,
        name,
        levels = levels,
        require_two = require_two
      )
      if (identical(name, "train") && !spec$positive %in% prepared$levels) {
        .abort(sprintf(
          "`positive` = \"%s\" is not present in the training outcome.",
          spec$positive
        ))
      }
      prepared
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

.register_default_workflow_adapters <- function() {
  register_workflow_adapter("binary", .workflow_binary_adapter())
}

.register_default_workflow_adapters()
