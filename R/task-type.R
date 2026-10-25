# L1 task-type layer ---------------------------------------------------------
#
# Task-type descriptors state how a task type plugs into the generic layers of
# uniml. Later layers dispatch on the task type through S3 rather than through
# scattered `if (task_type == "binary")` branches.

#' Task-type descriptor
#'
#' A descriptor records which mlr3 backend, outcome shape, metrics, learners,
#' and optional capabilities (`SHAP` / `DCA` / calibration) a task type
#' provides.
#'
#' @param id Task-type id, such as `"binary"`.
#' @param mlr3_task mlr3 task backend, such as `"classif"`.
#' @param outcome_class Human-readable outcome shape.
#' @param positive Whether the positive outcome class must be specified.
#' @param metrics Supported metric names.
#' @param default_learners Default learner keys.
#' @param supports_shap Whether SHAP explanations are supported.
#' @param supports_dca Whether decision-curve analysis is supported.
#' @param supports_calibration Whether probability calibration is supported.
#' @param implemented Whether the task type is implemented.
#'
#' @return A `uniml_task_type` object.
#' @export
#' @examples
#' task_type_spec(id = "binary", mlr3_task = "classif", metrics = "auc")
task_type_spec <- function(id, mlr3_task = NULL, outcome_class = NULL,
                           positive = TRUE, metrics = character(),
                           default_learners = character(),
                           supports_shap = FALSE, supports_dca = FALSE,
                           supports_calibration = FALSE, implemented = TRUE) {
  out <- structure(
    list(
      id = id,
      mlr3_task = mlr3_task,
      outcome_class = outcome_class,
      positive = positive,
      metrics = metrics,
      default_learners = default_learners,
      supports_shap = supports_shap,
      supports_dca = supports_dca,
      supports_calibration = supports_calibration,
      implemented = implemented
    ),
    class = c("uniml_task_type", "list")
  )
  validate_uniml_task_type(out)
  out
}

#' Validate a task-type descriptor
#'
#' @param x A `uniml_task_type` object.
#'
#' @return Invisibly returns `TRUE` when valid.
#' @examples
#' validate_uniml_task_type(get_task_type("binary"))
#' @export
validate_uniml_task_type <- function(x) {
  if (!inherits(x, "uniml_task_type")) {
    .abort("`x` must be a `uniml_task_type` object.")
  }
  required <- c(
    "id", "mlr3_task", "outcome_class", "positive", "metrics",
    "default_learners", "supports_shap", "supports_dca",
    "supports_calibration", "implemented"
  )
  missing_fields <- setdiff(required, names(x))
  if (length(missing_fields) > 0L) {
    .abort(sprintf(
      "Missing task-type field(s): %s.",
      paste(missing_fields, collapse = ", ")
    ))
  }
  .assert_scalar_character(x$id, "id")
  if (!is.null(x$mlr3_task)) {
    .assert_scalar_character(x$mlr3_task, "mlr3_task")
  }
  if (!is.null(x$outcome_class)) {
    .assert_scalar_character(x$outcome_class, "outcome_class")
  }
  .assert_scalar_logical(x$positive, "positive")
  .assert_character_vector(x$metrics, "metrics")
  .assert_character_vector(x$default_learners, "default_learners")
  .assert_scalar_logical(x$supports_shap, "supports_shap")
  .assert_scalar_logical(x$supports_dca, "supports_dca")
  .assert_scalar_logical(x$supports_calibration, "supports_calibration")
  .assert_scalar_logical(x$implemented, "implemented")
  invisible(TRUE)
}

#' Print a task-type descriptor
#'
#' @param x A `uniml_task_type` object.
#' @param ... Additional arguments passed to methods.
#'
#' @return Invisibly returns `x`.
#' @export
print.uniml_task_type <- function(x, ...) {
  validate_uniml_task_type(x)
  cat("<uniml_task_type>\n")
  cat(sprintf("id: %s (implemented: %s)\n", x$id, x$implemented))
  cat(sprintf("mlr3_task: %s\n", if (is.null(x$mlr3_task)) "none" else x$mlr3_task))
  cat(sprintf("metrics: %d\n", length(x$metrics)))
  cat(sprintf(
    "default_learners: %s\n",
    if (length(x$default_learners)) paste(x$default_learners, collapse = ", ") else "none"
  ))
  invisible(x)
}

# Registry -------------------------------------------------------------------

.uniml_task_types <- new.env(parent = emptyenv())

#' Register a task type
#'
#' @param spec A `uniml_task_type` object created by [task_type_spec()].
#' @param overwrite Whether to replace an existing registration.
#'
#' @return Invisibly returns `spec`.
#' @export
#' @examples
#' register_task_type(task_type_spec(id = "toy", mlr3_task = "classif"))
#' list_task_types()
register_task_type <- function(spec, overwrite = FALSE) {
  validate_uniml_task_type(spec)
  .assert_scalar_logical(overwrite, "overwrite")
  if (exists(spec$id, envir = .uniml_task_types, inherits = FALSE) && !overwrite) {
    .abort(sprintf(
      "Task type \"%s\" is already registered; set `overwrite = TRUE` to replace it.",
      spec$id
    ))
  }
  assign(spec$id, spec, envir = .uniml_task_types)
  invisible(spec)
}

#' Get a task-type descriptor
#'
#' @param id Task-type id.
#'
#' @return The matching `uniml_task_type` object.
#' @export
#' @examples
#' get_task_type("binary")
get_task_type <- function(id) {
  .assert_scalar_character(id, "id")
  if (!exists(id, envir = .uniml_task_types, inherits = FALSE)) {
    .abort(sprintf(
      "Unknown task type \"%s\". Registered task types: %s.",
      id,
      paste(list_task_types(), collapse = ", ")
    ))
  }
  get(id, envir = .uniml_task_types, inherits = FALSE)
}

#' List registered task types
#'
#' @param implemented_only Whether to return only implemented task types.
#'
#' @return A character vector of task-type ids.
#' @export
#' @examples
#' list_task_types()
#' list_task_types(implemented_only = TRUE)
list_task_types <- function(implemented_only = FALSE) {
  .assert_scalar_logical(implemented_only, "implemented_only")
  ids <- sort(ls(envir = .uniml_task_types))
  if (isTRUE(implemented_only)) {
    ids <- ids[vapply(
      ids,
      function(id) isTRUE(get(id, envir = .uniml_task_types, inherits = FALSE)$implemented),
      logical(1)
    )]
  }
  ids
}

# Dispatch helpers -----------------------------------------------------------

.no_method <- function(fun, x) {
  .abort(sprintf(
    "`%s()` has no method for an object of class \"%s\".",
    fun,
    paste(class(x), collapse = "/")
  ))
}

.not_implemented <- function(fun, task_type) {
  .abort(sprintf(
    "`%s()` is not implemented yet for task type \"%s\"; it is filled in by a later milestone.",
    fun,
    task_type
  ))
}

#' Calculate performance metrics
#'
#' S3 generic that dispatches metric calculation by task type. Method bodies
#' are added in later milestones.
#'
#' @param x An object whose class identifies the task type, such as
#'   `"uniml_binary"`.
#' @param ... Method-specific arguments.
#'
#' @return The method return value.
#' @references
#' Steyerberg EW (2019). *Clinical Prediction Models: A Practical Approach to
#' Development, Validation, and Updating*, 2nd edition. Springer.
#' doi:10.1007/978-3-030-16399-0
#'
#' Robin X, Turck N, Hainard A, et al. (2011). pROC: an open-source package for
#' R and S+ to analyze and compare ROC curves. *BMC Bioinformatics*, 12, 77.
#' doi:10.1186/1471-2105-12-77
#' @export
calc_metrics <- function(x, ...) {
  UseMethod("calc_metrics")
}

#' @rdname calc_metrics
#' @export
calc_metrics.default <- function(x, ...) {
  .no_method("calc_metrics", x)
}

#' Calibrate predicted probabilities
#'
#' S3 generic that dispatches probability calibration by task type. Method
#' bodies are added in later milestones.
#'
#' @param x An object whose class identifies the task type, such as
#'   `"uniml_binary"`.
#' @param ... Method-specific arguments.
#'
#' @return The method return value.
#' @references
#' Platt J (1999). Probabilistic outputs for support vector machines and
#' comparisons to regularized likelihood methods. In: *Advances in Large Margin
#' Classifiers*. MIT Press.
#'
#' Zadrozny B, Elkan C (2002). Transforming classifier scores into accurate
#' multiclass probability estimates. *Proceedings of the 8th ACM SIGKDD*,
#' 694-699. doi:10.1145/775047.775109
#'
#' Niculescu-Mizil A, Caruana R (2005). Predicting good probabilities with
#' supervised learning. *Proceedings of the 22nd ICML*, 625-632.
#' doi:10.1145/1102351.1102430
#'
#' Van Calster B, McLernon DJ, van Smeden M, et al. (2019). Calibration: the
#' Achilles heel of predictive analytics. *BMC Medicine*, 17, 230.
#' doi:10.1186/s12916-019-1466-7
#' @examples
#' probs <- c(0.05, 0.20, 0.35, 0.55, 0.70, 0.90)
#' labels <- c(0, 0, 0, 1, 1, 1)
#' calibrated <- calibrate_probabilities(probs, labels = labels, method = "platt")
#' calibrated$method
#' calibrated$calibrated
#' @export
calibrate_probabilities <- function(x, ...) {
  UseMethod("calibrate_probabilities")
}

#' @rdname calibrate_probabilities
#' @export
calibrate_probabilities.default <- function(x, ...) {
  .no_method("calibrate_probabilities", x)
}

#' @rdname calibrate_probabilities
#' @export
calibrate_probabilities.uniml_binary <- function(x, ...) {
  .calibrate_probabilities_uniml_binary(x, ...)
}

#' Decision-curve analysis
#'
#' S3 generic that dispatches decision-curve analysis by task type. Method
#' bodies are added in later milestones.
#'
#' @param x An object whose class identifies the task type, such as
#'   `"uniml_binary"`.
#' @param ... Method-specific arguments.
#'
#' @return The method return value.
#' @references
#' Vickers AJ, Elkin EB (2006). Decision curve analysis: a novel method for
#' evaluating prediction models. *Medical Decision Making*, 26(6), 565-574.
#' doi:10.1177/0272989X06295361
#'
#' Vickers AJ, van Calster B, Steyerberg EW (2016). Net benefit approaches to
#' the evaluation of prediction models, molecular markers, and diagnostic tests.
#' *BMJ*, 352, i6. doi:10.1136/bmj.i6
#' @examples
#' probs <- c(0.05, 0.20, 0.35, 0.55, 0.70, 0.90)
#' labels <- c(0, 0, 0, 1, 1, 1)
#' curve <- dca(probs, labels = labels)
#' head(curve)
#' @export
dca <- function(x, ...) {
  UseMethod("dca")
}

#' @rdname dca
#' @export
dca.default <- function(x, ...) {
  .no_method("dca", x)
}

#' @rdname dca
#' @export
dca.uniml_binary <- function(x, ...) {
  .dca_uniml_binary(x, ...)
}

#' Compute SHAP explanations
#'
#' S3 generic that dispatches SHAP computation by task type. Method bodies are
#' added in later milestones.
#'
#' @param x An object whose class identifies the task type, such as
#'   `"uniml_binary"`.
#' @param ... Method-specific arguments.
#'
#' @return The method return value.
#' @references
#' Lundberg SM, Lee SI (2017). A unified approach to interpreting model
#' predictions. *Advances in Neural Information Processing Systems*, 30.
#'
#' Lundberg SM, Erion G, Chen H, et al. (2020). From local explanations to
#' global understanding with explainable AI for trees. *Nature Machine
#' Intelligence*, 2(1), 56-67. doi:10.1038/s42256-019-0138-9
#'
#' Covert I, Lee SI (2021). Improving KernelSHAP: practical Shapley value
#' estimation using linear regression. *Proceedings of AISTATS*, PMLR 130,
#' 3457-3465.
#'
#' Molnar C (2022). *Interpretable Machine Learning*, 2nd edition.
#' https://christophm.github.io/interpretable-ml-book/
#' @examples
#' \dontrun{
#' result <- run_workflow(spec, train, validation)
#' explained <- explain_shap(result, model = "best", n_explain = 100)
#' plot_shap_importance(explained)
#' }
#' @export
explain_shap <- function(x, ...) {
  UseMethod("explain_shap")
}

#' @rdname explain_shap
#' @export
explain_shap.default <- function(x, ...) {
  .no_method("explain_shap", x)
}

#' @rdname explain_shap
#' @export
explain_shap.uniml_binary <- function(x, ...) {
  .explain_shap_uniml_binary(x, ...)
}

# Plotting generics ----------------------------------------------------------

# Documented in R/plot-performance.R.
#' @export
#' @rdname plot_roc
plot_roc <- function(x, ...) {
  UseMethod("plot_roc")
}

# Documented in R/plot-performance.R.
#' @exportS3Method plot_roc default
plot_roc.default <- function(x, ...) {
  .no_method("plot_roc", x)
}

# Documented in R/plot-performance.R.
#' @export
#' @rdname plot_calibration
plot_calibration <- function(x, ...) {
  UseMethod("plot_calibration")
}

# Documented in R/plot-performance.R.
#' @exportS3Method plot_calibration default
plot_calibration.default <- function(x, ...) {
  .no_method("plot_calibration", x)
}

# Documented in R/plot-performance.R.
#' @export
#' @rdname plot_dca
plot_dca <- function(x, ...) {
  UseMethod("plot_dca")
}

# Documented in R/plot-performance.R.
#' @exportS3Method plot_dca default
plot_dca.default <- function(x, ...) {
  .no_method("plot_dca", x)
}

# Documented in R/plot-performance.R.
#' @export
#' @rdname plot_confusion
plot_confusion <- function(x, ...) {
  UseMethod("plot_confusion")
}

# Documented in R/plot-performance.R.
#' @exportS3Method plot_confusion default
plot_confusion.default <- function(x, ...) {
  .no_method("plot_confusion", x)
}

# Documented in R/plot-shap.R.
#' @export
#' @rdname plot_shap_importance
plot_shap_importance <- function(x, ...) {
  UseMethod("plot_shap_importance")
}

# Documented in R/plot-shap.R.
#' @exportS3Method plot_shap_importance default
plot_shap_importance.default <- function(x, ...) {
  .no_method("plot_shap_importance", x)
}

# Documented in R/plot-shap.R.
#' @export
#' @rdname plot_shap_beeswarm
plot_shap_beeswarm <- function(x, ...) {
  UseMethod("plot_shap_beeswarm")
}

# Documented in R/plot-shap.R.
#' @exportS3Method plot_shap_beeswarm default
plot_shap_beeswarm.default <- function(x, ...) {
  .no_method("plot_shap_beeswarm", x)
}

# Documented in R/plot-shap.R.
#' @export
#' @rdname plot_shap_dependence
plot_shap_dependence <- function(x, ...) {
  UseMethod("plot_shap_dependence")
}

# Documented in R/plot-shap.R.
#' @exportS3Method plot_shap_dependence default
plot_shap_dependence.default <- function(x, ...) {
  .no_method("plot_shap_dependence", x)
}

# Documented in R/plot-shap.R.
#' @export
#' @rdname plot_shap_waterfall
plot_shap_waterfall <- function(x, ...) {
  UseMethod("plot_shap_waterfall")
}

# Documented in R/plot-shap.R.
#' @exportS3Method plot_shap_waterfall default
plot_shap_waterfall.default <- function(x, ...) {
  .no_method("plot_shap_waterfall", x)
}

# Documented in R/plot-shap.R.
#' @export
#' @rdname plot_shap_force
plot_shap_force <- function(x, ...) {
  UseMethod("plot_shap_force")
}

# Documented in R/plot-shap.R.
#' @exportS3Method plot_shap_force default
plot_shap_force.default <- function(x, ...) {
  .no_method("plot_shap_force", x)
}

# Default registrations ------------------------------------------------------

.register_default_task_types <- function() {
  register_task_type(task_type_spec(
    id = "binary",
    mlr3_task = "classif",
    outcome_class = "factor/0-1",
    positive = TRUE,
    metrics = c(
      "auc", "pr_auc", "accuracy", "sensitivity", "specificity",
      "ppv", "npv", "f1", "f2", "youden", "plr", "nlr", "dor",
      "balanced_accuracy", "kappa", "brier", "nri", "idi"
    ),
    default_learners = c("lr", "rf", "xgb", "nnet", "knn", "cart", "nb"),
    supports_shap = TRUE,
    supports_dca = TRUE,
    supports_calibration = TRUE,
    implemented = TRUE
  ))

  register_task_type(task_type_spec(
    id = "multiclass",
    mlr3_task = "classif",
    outcome_class = "factor",
    positive = FALSE,
    implemented = FALSE
  ))

  register_task_type(task_type_spec(
    id = "survival",
    mlr3_task = "surv",
    outcome_class = "time/status",
    positive = FALSE,
    implemented = FALSE
  ))

  invisible(TRUE)
}

.register_default_task_types()
