.abort <- function(...) {
  stop(..., call. = FALSE)
}

.assert_scalar_character <- function(x, name, allow_empty = FALSE) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    .abort(sprintf("`%s` must be a single non-missing character value.", name))
  }
  if (!allow_empty && !nzchar(x)) {
    .abort(sprintf("`%s` must not be empty.", name))
  }
  invisible(TRUE)
}

.assert_scalar_logical <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    .abort(sprintf("`%s` must be a single non-missing logical value.", name))
  }
  invisible(TRUE)
}

.assert_scalar_number <- function(x, name, integer = FALSE, min = -Inf, max = Inf) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x)) {
    .abort(sprintf("`%s` must be a single finite numeric value.", name))
  }
  if (integer && abs(x - round(x)) > .Machine$double.eps^0.5) {
    .abort(sprintf("`%s` must be an integer value.", name))
  }
  if (x < min || x > max) {
    .abort(sprintf("`%s` must be between %s and %s.", name, format(min), format(max)))
  }
  invisible(TRUE)
}

.assert_choice <- function(x, name, choices) {
  .assert_scalar_character(x, name)
  if (!x %in% choices) {
    .abort(sprintf(
      "`%s` must be one of: %s.",
      name,
      paste(sprintf("\"%s\"", choices), collapse = ", ")
    ))
  }
  invisible(TRUE)
}

.assert_character_vector <- function(x, name, allow_null = FALSE, choices = NULL) {
  if (is.null(x)) {
    if (allow_null) {
      return(invisible(TRUE))
    }
    .abort(sprintf("`%s` must not be NULL.", name))
  }
  if (!is.character(x) || anyNA(x) || any(!nzchar(x))) {
    .abort(sprintf("`%s` must be a character vector without missing or empty values.", name))
  }
  if (!is.null(choices) && any(!x %in% choices)) {
    .abort(sprintf(
      "`%s` contains unsupported values: %s.",
      name,
      paste(setdiff(x, choices), collapse = ", ")
    ))
  }
  invisible(TRUE)
}

.allowed_task_types <- function() {
  if (exists("list_task_types", mode = "function", inherits = TRUE)) {
    value <- tryCatch(
      get("list_task_types", mode = "function", inherits = TRUE)(implemented_only = TRUE),
      error = function(e) NULL
    )
    if (length(value) > 0L) {
      return(as.character(value))
    }
  }
  "binary"
}

.allowed_leakage_steps <- function() {
  c("imputation", "scaling", "encoding", "selection", "balance", "tuning")
}

# Step-level tuning arguments that may be forwarded from the specification to
# the preprocessing engine. Anything outside these lists is rejected instead of
# being silently stored and ignored.
.allowed_step_args <- function(step) {
  switch(
    step,
    impute = c("m", "max_iter", "seed"),
    scale = c("vars"),
    encode = c("max_levels"),
    balance = c("p", "n_neighbors", "dup_size", "seed"),
    .abort(sprintf("Unknown preprocessing step \"%s\".", step))
  )
}

.allowed_selection_args <- function(method) {
  switch(
    method,
    univariate = c("p_threshold"),
    lasso = c("lambda", "nfolds", "seed", "alpha", "type_measure", "standardize"),
    boruta = c("maxRuns", "num_threads", "seed", "tentative"),
    rf_importance = c("top_k", "seed"),
    rfe = c("n_features", "learner", "seed"),
    multivariate = c("method", "p_threshold"),
    .abort(sprintf("Unknown feature-selection method \"%s\".", method))
  )
}

.allowed_calibration_args <- function() {
  c("folds", "bins", "fit")
}

.assert_named_list <- function(x, name) {
  if (is.null(x)) {
    return(invisible(TRUE))
  }
  if (!is.list(x) || (length(x) > 0L && (is.null(names(x)) || any(!nzchar(names(x)))))) {
    .abort(sprintf("`%s` must be a named list.", name))
  }
  invisible(TRUE)
}

.assert_step_args <- function(args, allowed, name) {
  .assert_named_list(args, name)
  unknown <- setdiff(names(args), allowed)
  if (length(unknown) > 0L) {
    .abort(sprintf(
      "`%s` contains unsupported argument(s): %s. Allowed: %s.",
      name,
      paste(unknown, collapse = ", "),
      paste(allowed, collapse = ", ")
    ))
  }
  invisible(TRUE)
}

.assert_labels <- function(labels) {
  if (is.null(labels)) {
    return(invisible(TRUE))
  }
  if (!is.character(labels) || is.null(names(labels)) ||
      any(!nzchar(names(labels))) || anyNA(labels) || any(!nzchar(labels))) {
    .abort("`labels` must be NULL or a non-missing named character vector.")
  }
  if (anyDuplicated(names(labels))) {
    .abort("`labels` must not contain duplicate names.")
  }
  invisible(TRUE)
}

.assert_cost <- function(cost) {
  if (!is.numeric(cost) || length(cost) != 2L ||
      is.null(names(cost)) || !all(c("fp", "fn") %in% names(cost)) ||
      anyNA(cost) || any(!is.finite(cost)) || any(cost < 0)) {
    .abort("`cost` must be a named numeric vector with non-negative `fp` and `fn`.")
  }
  invisible(TRUE)
}

.new_uniml_spec <- function(...) {
  structure(list(...), class = c("uniml_spec", "list"))
}

.default_spec_values <- function() {
  list(
    leakage_policy = "custom",
    in_cv = c("selection", "balance", "tuning"),
    nested_tuning = FALSE,
    seed = 123,
    ordered_as = "ordinal",
    outcome_na_action = "drop",
    threshold_method = "youden",
    cost = c(fp = 1, fn = 1),
    fixed_threshold = NULL,
    reference_model = NULL,
    select_on = "validation",
    best_model_metric = "composite",
    verbose = 1L,
    n_workers = 1L,
    future_strategy = "multisession",
    preprocess = list(
      impute = "median_mode",
      m = 5L,
      scale = "zscore",
      encode = "onehot",
      balance = "none",
      ordered_as = "ordinal",
      outcome_na_action = "drop",
      impute_args = list(),
      scale_args = list(),
      encode_args = list(),
      balance_args = list()
    ),
    selection = list(
      methods = "lasso",
      lambda = "1se",
      p_threshold = 0.2,
      maxRuns = 50L,
      multivariate = "stepwise",
      combine = "intersect",
      representation = "full_levels",
      k = NULL,
      cor_threshold = NULL,
      vif_threshold = NULL,
      args = list()
    ),
    learners = list(
      learners = character(),
      tune = FALSE,
      params = NULL,
      search_space = NULL
    ),
    tuning = list(
      tuner = "random_search",
      folds = 5L,
      repeats = 1L,
      evals = 30L,
      terminator = "evals",
      seconds = NULL,
      metric = "auc",
      nested = FALSE,
      seed = 123
    ),
    evaluation = list(
      metrics = "all",
      ci = "bootstrap",
      threshold_method = "youden",
      cost = c(fp = 1, fn = 1),
      fixed_threshold = NULL,
      reference_model = NULL,
      best_model = "composite",
      calibration = "none",
      calibration_args = list(folds = 5L, bins = 10L, fit = "out_of_fold"),
      tests = c("delong", "hl"),
      dca = TRUE,
      select_on = "validation"
    ),
    explain = list(
      model = "best",
      plots = c("importance", "beeswarm", "dependence", "waterfall"),
      thresholds = NULL
    ),
    output = list(
      verbose = 1L,
      n_workers = 1L,
      future_strategy = "multisession",
      dir = "output"
    )
  )
}

#' Construct a uniml specification
#'
#' The specification is the top-level configuration object used by uniml.
#' It stores variable roles and the default settings for each later stage.
#'
#' @param task_type Task type. M2 supports `"binary"`.
#' @param outcome Outcome column name.
#' @param positive Positive outcome class. Required.
#' @param id Optional ID column name.
#' @param exclude Optional character vector of columns excluded from modelling.
#' @param labels Optional named character vector mapping column names to display labels.
#' @param ... Named specification fields, such as `leakage_policy` or `seed`.
#'
#' @return A `uniml_spec` object.
#' @examples
#' spec <- uniml_spec(
#'   task_type = "binary",
#'   outcome = "died",
#'   positive = "yes",
#'   id = "id"
#' )
#' spec
#' @export
uniml_spec <- function(task_type, outcome, positive, id = NULL, exclude = NULL,
                       labels = NULL, ...) {
  if (missing(task_type) || missing(outcome) || missing(positive)) {
    .abort("`task_type`, `outcome`, and `positive` must be supplied.")
  }

  values <- .default_spec_values()
  dots <- list(...)
  if (length(dots) > 0L) {
    if (is.null(names(dots)) || any(!nzchar(names(dots)))) {
      .abort("All arguments in `...` must be named.")
    }
    unknown <- setdiff(names(dots), names(values))
    if (length(unknown) > 0L) {
      .abort(sprintf("Unknown specification field(s): %s.", paste(unknown, collapse = ", ")))
    }
    values[names(dots)] <- dots
  }

  values$task_type <- task_type
  values$outcome <- outcome
  values$positive <- positive
  values$id <- id
  values$exclude <- exclude
  values$labels <- labels

  if (identical(values$leakage_policy, "strict_in") &&
      !"in_cv" %in% names(dots)) {
    values$in_cv <- .allowed_leakage_steps()
  }
  if (identical(values$leakage_policy, "strict_out") &&
      !"in_cv" %in% names(dots)) {
    values$in_cv <- character()
  }

  values$preprocess$ordered_as <- values$ordered_as
  values$preprocess$outcome_na_action <- values$outcome_na_action
  values$tuning$nested <- values$nested_tuning
  values$tuning$seed <- values$seed
  values$evaluation$threshold_method <- values$threshold_method
  values$evaluation$cost <- values$cost
  values$evaluation$fixed_threshold <- values$fixed_threshold
  values$evaluation$reference_model <- values$reference_model
  values$evaluation$best_model <- values$best_model_metric
  values$evaluation$select_on <- values$select_on
  values$output$verbose <- values$verbose
  values$output$n_workers <- values$n_workers
  values$output$future_strategy <- values$future_strategy

  out <- .new_uniml_spec(
    task_type = values$task_type,
    outcome = values$outcome,
    positive = values$positive,
    id = values$id,
    exclude = values$exclude,
    labels = values$labels,
    leakage_policy = values$leakage_policy,
    in_cv = values$in_cv,
    nested_tuning = values$nested_tuning,
    seed = values$seed,
    ordered_as = values$ordered_as,
    outcome_na_action = values$outcome_na_action,
    threshold_method = values$threshold_method,
    cost = values$cost,
    fixed_threshold = values$fixed_threshold,
    reference_model = values$reference_model,
    select_on = values$select_on,
    best_model_metric = values$best_model_metric,
    verbose = values$verbose,
    n_workers = values$n_workers,
    future_strategy = values$future_strategy,
    preprocess = values$preprocess,
    selection = values$selection,
    learners = values$learners,
    tuning = values$tuning,
    evaluation = values$evaluation,
    explain = values$explain,
    output = values$output
  )
  validate_uniml_spec(out)
  out
}

#' Validate a uniml specification
#'
#' @param x A `uniml_spec` object.
#'
#' @return Invisibly returns `TRUE` when valid.
#' @export
validate_uniml_spec <- function(x) {
  if (!inherits(x, "uniml_spec")) {
    .abort("`x` must be a `uniml_spec` object.")
  }

  required <- c(
    "task_type", "outcome", "positive", "id", "exclude", "labels",
    "leakage_policy", "in_cv", "nested_tuning", "seed", "ordered_as",
    "outcome_na_action", "threshold_method", "cost", "fixed_threshold",
    "reference_model", "select_on", "best_model_metric", "verbose",
    "n_workers", "future_strategy", "preprocess", "selection", "learners",
    "tuning", "evaluation", "explain", "output"
  )
  missing_fields <- setdiff(required, names(x))
  if (length(missing_fields) > 0L) {
    .abort(sprintf("Missing specification field(s): %s.", paste(missing_fields, collapse = ", ")))
  }

  .assert_scalar_character(x$task_type, "task_type")
  if (!x$task_type %in% .allowed_task_types()) {
    .abort(sprintf(
      "`task_type` must be one of: %s.",
      paste(sprintf("\"%s\"", .allowed_task_types()), collapse = ", ")
    ))
  }
  .assert_scalar_character(x$outcome, "outcome")
  if (length(x$positive) != 1L || is.na(x$positive)) {
    .abort("`positive` must be a single non-missing value.")
  }

  .assert_character_vector(x$id, "id", allow_null = TRUE)
  .assert_character_vector(x$exclude, "exclude", allow_null = TRUE)
  if (anyDuplicated(c(x$id, x$exclude)) > 0L) {
    .abort("`id` and `exclude` must not overlap or contain duplicates.")
  }
  if (x$outcome %in% c(x$id, x$exclude)) {
    .abort("The outcome column must not appear in `id` or `exclude`.")
  }
  .assert_labels(x$labels)

  .assert_choice(x$leakage_policy, "leakage_policy", c("strict_in", "strict_out", "custom"))
  .assert_character_vector(x$in_cv, "in_cv", allow_null = FALSE, choices = .allowed_leakage_steps())
  .assert_scalar_logical(x$nested_tuning, "nested_tuning")
  .assert_scalar_number(x$seed, "seed", integer = TRUE)
  .assert_choice(x$ordered_as, "ordered_as", c("ordinal", "numeric"))
  .assert_choice(x$outcome_na_action, "outcome_na_action", c("drop", "keep"))
  .assert_choice(x$threshold_method, "threshold_method", c("youden", "cost", "fixed"))
  .assert_cost(x$cost)
  if (!is.null(x$fixed_threshold)) {
    .assert_scalar_number(x$fixed_threshold, "fixed_threshold", min = 0, max = 1)
  }
  if (identical(x$threshold_method, "fixed") && is.null(x$fixed_threshold)) {
    .abort("`fixed_threshold` is required when `threshold_method = \"fixed\"`.")
  }
  if (!is.null(x$reference_model)) {
    .assert_scalar_character(x$reference_model, "reference_model")
  }
  .assert_choice(x$select_on, "select_on", c("auto", "validation", "test"))
  .assert_scalar_character(x$best_model_metric, "best_model_metric")
  .assert_scalar_number(x$verbose, "verbose", integer = TRUE, min = 0, max = 2)
  .assert_scalar_number(x$n_workers, "n_workers", integer = TRUE, min = 1)
  .assert_choice(
    x$future_strategy,
    "future_strategy",
    c("sequential", "multisession", "multicore", "multiprocess", "cluster")
  )

  .assert_character_vector(x$preprocess$impute, "preprocess$impute",
                           choices = c("median_mode", "mice", "missRanger",
                                       "missForest", "none"))
  .assert_scalar_number(x$preprocess$m, "preprocess$m", integer = TRUE, min = 1)
  .assert_choice(x$preprocess$scale, "preprocess$scale", c("zscore", "minmax", "none"))
  .assert_choice(x$preprocess$encode, "preprocess$encode", c("onehot", "onehot_drop", "none"))
  .assert_choice(x$preprocess$balance, "preprocess$balance", c("none", "rose", "smote", "both"))
  .assert_step_args(x$preprocess$impute_args, .allowed_step_args("impute"),
                    "preprocess$impute_args")
  .assert_step_args(x$preprocess$scale_args, .allowed_step_args("scale"),
                    "preprocess$scale_args")
  .assert_step_args(x$preprocess$encode_args, .allowed_step_args("encode"),
                    "preprocess$encode_args")
  .assert_step_args(x$preprocess$balance_args, .allowed_step_args("balance"),
                    "preprocess$balance_args")

  .assert_character_vector(
    x$selection$methods,
    "selection$methods",
    choices = c(
      "univariate", "logistic", "single", "lasso", "boruta", "rf",
      "rf_importance", "importance", "rfe", "multivariate"
    )
  )
  .assert_choice(x$selection$lambda, "selection$lambda", c("1se", "min"))
  .assert_scalar_number(x$selection$p_threshold, "selection$p_threshold", min = 0, max = 1)
  .assert_scalar_number(x$selection$maxRuns, "selection$maxRuns", integer = TRUE, min = 11)
  .assert_choice(x$selection$multivariate, "selection$multivariate", c("stepwise", "pvalue"))
  .assert_choice(x$selection$combine, "selection$combine", c("intersect", "union"))
  .assert_choice(
    x$selection$representation,
    "selection$representation",
    c("full_levels", "selected_levels")
  )
  if (!is.null(x$selection$k)) {
    .assert_scalar_number(x$selection$k, "selection$k", integer = TRUE, min = 1)
  }
  if (!is.null(x$selection$cor_threshold)) {
    .assert_scalar_number(
      x$selection$cor_threshold,
      "selection$cor_threshold",
      min = 0,
      max = 1
    )
  }
  if (!is.null(x$selection$vif_threshold)) {
    .assert_scalar_number(
      x$selection$vif_threshold,
      "selection$vif_threshold",
      min = 1
    )
  }
  .assert_named_list(x$selection$args, "selection$args")
  if (length(x$selection$args) > 0L) {
    unknown_methods <- setdiff(
      names(x$selection$args),
      c("univariate", "lasso", "boruta", "rf_importance", "rfe", "multivariate")
    )
    if (length(unknown_methods) > 0L) {
      .abort(sprintf(
        "`selection$args` contains unknown method name(s): %s.",
        paste(unknown_methods, collapse = ", ")
      ))
    }
    for (method in names(x$selection$args)) {
      .assert_step_args(
        x$selection$args[[method]],
        .allowed_selection_args(method),
        sprintf("selection$args$%s", method)
      )
    }
  }

  if (!is.list(x$learners) || is.null(x$learners$learners)) {
    .abort("`learners` must be a list created by the learner specification.")
  }
  .assert_scalar_logical(x$learners$tune, "learners$tune")
  if  (is.character(x$learners$learners)) {
    .assert_character_vector(x$learners$learners, "learners$learners")
  } else if (!is.list(x$learners$learners)) {
    .abort("`learners$learners` must be a character vector or a list.")
  }

  .assert_choice(x$tuning$tuner, "tuning$tuner",
                 c("random_search", "grid_search", "bayesian"))
  .assert_scalar_number(x$tuning$folds, "tuning$folds", integer = TRUE, min = 2)
  .assert_scalar_number(x$tuning$repeats, "tuning$repeats", integer = TRUE, min = 1)
  .assert_scalar_number(x$tuning$evals, "tuning$evals", integer = TRUE, min = 1)
  .assert_choice(x$tuning$terminator, "tuning$terminator", c("evals", "seconds"))
  if (!is.null(x$tuning$seconds)) {
    .assert_scalar_number(x$tuning$seconds, "tuning$seconds", min = 1)
  }
  if (identical(x$tuning$terminator, "seconds") && is.null(x$tuning$seconds)) {
    .abort("`tuning$seconds` is required when `terminator = \"seconds\"`.")
  }
  .assert_scalar_character(x$tuning$metric, "tuning$metric")
  .assert_scalar_logical(x$tuning$nested, "tuning$nested")
  .assert_scalar_number(x$tuning$seed, "tuning$seed", integer = TRUE)

  .assert_character_vector(x$evaluation$metrics, "evaluation$metrics")
  .assert_choice(x$evaluation$ci, "evaluation$ci", c("bootstrap", "none"))
  .assert_choice(
    x$evaluation$calibration,
    "evaluation$calibration",
    c("platt", "platt_segment", "isotonic", "none")
  )
  .assert_step_args(
    x$evaluation$calibration_args,
    .allowed_calibration_args(),
    "evaluation$calibration_args"
  )
  if (!is.null(x$evaluation$calibration_args$fit)) {
    .assert_choice(
      x$evaluation$calibration_args$fit,
      "evaluation$calibration_args$fit",
      c("out_of_fold", "in_sample")
    )
  }
  if (!is.null(x$evaluation$calibration_args$folds)) {
    .assert_scalar_number(
      x$evaluation$calibration_args$folds,
      "evaluation$calibration_args$folds",
      integer = TRUE,
      min = 2
    )
  }
  if (!is.null(x$evaluation$calibration_args$bins)) {
    .assert_scalar_number(
      x$evaluation$calibration_args$bins,
      "evaluation$calibration_args$bins",
      integer = TRUE,
      min = 2
    )
  }
  .assert_character_vector(x$evaluation$tests, "evaluation$tests",
                           choices = c("delong", "hl", "nri", "idi"))
  .assert_scalar_logical(x$evaluation$dca, "evaluation$dca")

  .assert_scalar_character(x$explain$model, "explain$model")
  .assert_character_vector(x$explain$plots, "explain$plots",
                           choices = c("importance", "beeswarm", "dependence",
                                       "waterfall", "force"))
  if (!is.null(x$explain$thresholds)) {
    settings <- x$explain$thresholds
    .assert_named_list(settings, "explain$thresholds")
    unknown <- setdiff(
      names(settings),
      c("targets", "interaction", "min_n", "min_unique", "span")
    )
    if (length(unknown) > 0L) {
      .abort(sprintf(
        "`explain$thresholds` contains unsupported field(s): %s.",
        paste(unknown, collapse = ", ")
      ))
    }
    .assert_character_vector(settings$targets, "explain$thresholds$targets",
                             allow_null = TRUE)
    if (!is.null(settings$interaction)) {
      .assert_scalar_character(settings$interaction,
                               "explain$thresholds$interaction")
    }
    if (!is.null(settings$min_n)) {
      .assert_scalar_number(settings$min_n, "explain$thresholds$min_n",
                            integer = TRUE, min = 1)
    }
    if (!is.null(settings$min_unique)) {
      .assert_scalar_number(settings$min_unique, "explain$thresholds$min_unique",
                            integer = TRUE, min = 1)
    }
    if (!is.null(settings$span)) {
      .assert_scalar_number(settings$span, "explain$thresholds$span",
                            min = 0, max = 1)
    }
  }

  .assert_scalar_number(x$output$verbose, "output$verbose", integer = TRUE, min = 0, max = 2)
  .assert_scalar_number(x$output$n_workers, "output$n_workers", integer = TRUE, min = 1)
  .assert_choice(
    x$output$future_strategy,
    "output$future_strategy",
    c("sequential", "multisession", "multicore", "multiprocess", "cluster")
  )
  .assert_scalar_character(x$output$dir, "output$dir")

  invisible(TRUE)
}

#' Print a uniml specification
#'
#' @param x A `uniml_spec` object.
#' @param ... Additional arguments passed to methods.
#'
#' @return Invisibly returns `x`.
#' @export
print.uniml_spec <- function(x, ...) {
  validate_uniml_spec(x)
  cat("<uniml_spec>\n")
  cat(sprintf("task_type: %s\n", x$task_type))
  cat(sprintf("outcome: %s\n", x$outcome))
  cat(sprintf("positive: %s\n", as.character(x$positive)))
  cat(sprintf("leakage_policy: %s\n", x$leakage_policy))
  cat(sprintf("in_cv: %s\n", if (length(x$in_cv)) paste(x$in_cv, collapse = ", ") else "none"))
  cat(sprintf("selection: %s\n", paste(x$selection$methods, collapse = ", ")))
  cat(sprintf("verbose: %s, n_workers: %s\n", x$verbose, x$n_workers))
  invisible(x)
}

#' Convert data to a uniml data object
#'
#' @param data A data frame.
#' @param spec A `uniml_spec` object.
#'
#' @return A `uniml_data` object.
#' @examples
#' spec <- uniml_spec("binary", "y", "yes", id = "id")
#' data <- data.frame(
#'   id = 1:4,
#'   y = factor(c("no", "no", "yes", "yes"), levels = c("no", "yes")),
#'   x1 = c(0.1, 0.4, 0.6, 0.9)
#' )
#' as_uniml_data(data, spec)
#' @export
as_uniml_data <- function(data, spec) {
  validate_uniml_spec(spec)
  if (!is.data.frame(data)) {
    .abort("`data` must be a data frame.")
  }
  data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  if (is.null(names(data)) || any(!nzchar(names(data))) || anyDuplicated(names(data))) {
    .abort("`data` must have unique, non-empty column names.")
  }
  missing_columns <- setdiff(c(spec$outcome, spec$id, spec$exclude), names(data))
  if (length(missing_columns) > 0L) {
    .abort(sprintf("Column(s) not found in `data`: %s.", paste(missing_columns, collapse = ", ")))
  }

  raw_outcome <- data[[spec$outcome]]
  present <- unique(as.character(raw_outcome[!is.na(raw_outcome)]))
  if (length(present) != 2L) {
    .abort("The binary outcome must contain exactly two non-missing classes.")
  }
  positive <- as.character(spec$positive)
  if (!positive %in% present) {
    .abort("`positive` must match one of the observed outcome classes.")
  }
  other <- setdiff(present, positive)
  if (length(other) != 1L) {
    .abort("The binary outcome must contain exactly one class other than `positive`.")
  }

  keep <- if (identical(spec$outcome_na_action, "drop")) !is.na(raw_outcome) else rep(TRUE, nrow(data))
  dropped <- sum(!keep)
  data <- data[keep, , drop = FALSE]
  raw_outcome <- data[[spec$outcome]]
  outcome <- factor(as.character(raw_outcome), levels = c(positive, other))

  role <- rep("feature", ncol(data))
  names(role) <- names(data)
  role[spec$outcome] <- "outcome"
  if (length(spec$id) > 0L) {
    role[spec$id] <- "id"
  }
  if (length(spec$exclude) > 0L) {
    role[spec$exclude] <- "exclude"
  }
  features <- data[, role == "feature", drop = FALSE]
  if (ncol(features) == 0L) {
    .abort("At least one feature column is required.")
  }

  feature_warnings <- character()
  all_na <- names(features)[vapply(features, function(z) all(is.na(z)), logical(1))]
  constant <- names(features)[vapply(features, function(z) {
    values <- z[!is.na(z)]
    length(values) > 0L && length(unique(values)) <= 1L
  }, logical(1))]
  duplicate <- names(features)[duplicated(as.list(features))]
  if (length(all_na) > 0L) {
    feature_warnings <- c(feature_warnings, sprintf(
      "All-NA feature column(s): %s.",
      paste(all_na, collapse = ", ")
    ))
  }
  if (length(constant) > 0L) {
    feature_warnings <- c(feature_warnings, sprintf(
      "Constant feature column(s): %s.",
      paste(constant, collapse = ", ")
    ))
  }
  if (length(duplicate) > 0L) {
    feature_warnings <- c(feature_warnings, sprintf(
      "Duplicate feature column(s): %s.",
      paste(duplicate, collapse = ", ")
    ))
  }

  types <- data.frame(
    column = names(data),
    class = vapply(data, function(z) paste(class(z), collapse = "/"), character(1)),
    role = unname(role),
    stringsAsFactors = FALSE
  )
  out <- structure(
    list(
      data = data,
      features = features,
      outcome = outcome,
      outcome_name = spec$outcome,
      positive = positive,
      id = spec$id,
      exclude = spec$exclude,
      role = role,
      types = types,
      spec = spec,
      warnings = feature_warnings,
      meta = list(
        n = nrow(data),
        n_features = ncol(features),
        n_dropped = dropped
      )
    ),
    class = c("uniml_data", "list")
  )
  validate_uniml_data(out)
  if (length(feature_warnings) > 0L) {
    warning(paste(feature_warnings, collapse = "\n"), call. = FALSE)
  }
  out
}

#' Validate a uniml data object
#'
#' @param x A `uniml_data` object.
#'
#' @return Invisibly returns `TRUE` when valid.
#' @export
validate_uniml_data <- function(x) {
  if (!inherits(x, "uniml_data")) {
    .abort("`x` must be a `uniml_data` object.")
  }
  required <- c(
    "data", "features", "outcome", "outcome_name", "positive", "id",
    "exclude", "role", "types", "spec", "warnings", "meta"
  )
  missing_fields <- setdiff(required, names(x))
  if (length(missing_fields) > 0L) {
    .abort(sprintf("Missing data field(s): %s.", paste(missing_fields, collapse = ", ")))
  }
  if (!is.data.frame(x$data) || !is.data.frame(x$features)) {
    .abort("`data` and `features` must be data frames.")
  }
  validate_uniml_spec(x$spec)
  if (length(x$outcome) != nrow(x$data)) {
    .abort("`outcome` must have one value per row in `data`.")
  }
  if (!is.factor(x$outcome) || nlevels(x$outcome) != 2L) {
    .abort("`outcome` must be a two-level factor.")
  }
  if (!identical(names(x$role), names(x$data))) {
    .abort("`role` names must match the columns in `data`.")
  }
  if (any(!unname(x$role) %in% c("id", "outcome", "feature", "exclude"))) {
    .abort("`role` contains unsupported values.")
  }
  if (sum(x$role == "outcome") != 1L || x$role[x$outcome_name] != "outcome") {
    .abort("`role` must mark exactly the outcome column.")
  }
  if (ncol(x$features) != sum(x$role == "feature")) {
    .abort("`features` must contain exactly the columns marked as features.")
  }
  if (!is.character(x$warnings) || !is.list(x$meta)) {
    .abort("`warnings` must be character and `meta` must be a list.")
  }
  invisible(TRUE)
}

#' Print a uniml data object
#'
#' @param x A `uniml_data` object.
#' @param ... Additional arguments passed to methods.
#'
#' @return Invisibly returns `x`.
#' @export
print.uniml_data <- function(x, ...) {
  validate_uniml_data(x)
  cat("<uniml_data>\n")
  cat(sprintf("rows: %d\n", x$meta$n))
  cat(sprintf("features: %d\n", x$meta$n_features))
  cat(sprintf("outcome: %s (positive = %s)\n", x$outcome_name, x$positive))
  if (length(x$warnings) > 0L) {
    cat(sprintf("warnings: %d recorded\n", length(x$warnings)))
  }
  invisible(x)
}

.subset_uniml_data <- function(x, index) {
  out <- x
  out$data <- x$data[index, , drop = FALSE]
  out$features <- x$features[index, , drop = FALSE]
  out$outcome <- x$outcome[index]
  out$meta$n <- nrow(out$data)
  out$meta$n_features <- ncol(out$features)
  validate_uniml_data(out)
  out
}

.split_names <- function(prop) {
  if (length(prop) == 2L) {
    return(c("train", "validation"))
  }
  if (length(prop) == 3L) {
    return(c("train", "validation", "test"))
  }
  .abort("`prop` must have length 2 or 3.")
}

.normalise_prop <- function(prop) {
  if (!is.numeric(prop) || anyNA(prop) || any(!is.finite(prop)) ||
      any(prop <= 0) || abs(sum(prop) - 1) > 1e-8) {
    .abort("`prop` must contain positive finite proportions that sum to 1.")
  }
  if (!is.null(names(prop)) && any(nzchar(names(prop)))) {
    expected <- .split_names(prop)
    if (any(!names(prop) %in% expected) || !all(expected %in% names(prop))) {
      .abort("Named `prop` must contain train, validation, and optionally test.")
    }
    prop <- prop[expected]
  } else {
    names(prop) <- .split_names(prop)
  }
  prop
}

.allocate_split <- function(n, prop) {
  counts <- floor(n * prop)
  counts[1] <- counts[1] + n - sum(counts)
  counts
}

.positive_rate <- function(outcome) {
  if (length(outcome) == 0L) {
    return(NA_real_)
  }
  mean(!is.na(outcome) & as.character(outcome) == levels(outcome)[1])
}

#' Split uniml data into train, validation, and optional test sets
#'
#' @param data A `uniml_data` object created by [as_uniml_data()].
#' @param prop Numeric vector of length 2 or 3 with proportions summing to 1.
#' @param stratify Whether to stratify the split by outcome class.
#' @param seed Random seed.
#'
#' @return A `uniml_split` object.
#' @examples
#' spec <- uniml_spec("binary", "y", "yes")
#' data <- data.frame(
#'   y = factor(rep(c("no", "yes"), 20), levels = c("no", "yes")),
#'   x1 = stats::rnorm(40)
#' )
#' split <- make_split(as_uniml_data(data, spec), prop = c(0.7, 0.3),
#'   stratify = TRUE, seed = 1)
#' names(split)
#' @export
make_split <- function(data, prop, stratify = FALSE, seed = 123) {
  validate_uniml_data(data)
  .assert_scalar_logical(stratify, "stratify")
  .assert_scalar_number(seed, "seed", integer = TRUE)
  prop <- .normalise_prop(prop)
  if (anyNA(data$outcome)) {
    .abort("`make_split()` requires non-missing outcome values; use `outcome_na_action = \"drop\"`.")
  }
  n <- nrow(data$data)
  counts <- .allocate_split(n, prop)
  if (any(counts <= 0L)) {
    .abort("Each split must contain at least one row.")
  }

  set.seed(seed)
  if (isFALSE(stratify)) {
    indices <- sample(seq_len(n), n)
    split_indices <- list(
      train = indices[seq_len(counts[1])],
      validation = indices[(counts[1] + 1L):(counts[1] + counts[2])]
    )
    if (length(counts) == 3L) {
      split_indices$test <- indices[(counts[1] + counts[2] + 1L):n]
    }
  } else {
    strata <- split(seq_len(n), as.character(data$outcome))
    split_indices <- lapply(seq_along(counts), function(i) integer())
    names(split_indices) <- names(counts)
    for (rows in strata) {
      local_counts <- floor(length(rows) * prop)
      local_counts[1] <- local_counts[1] + length(rows) - sum(local_counts)
      local_order <- sample(seq_along(rows), length(rows))
      cursor <- 0L
      for (i in seq_along(local_counts)) {
        if (local_counts[i] > 0L) {
          selected <- local_order[(cursor + 1L):(cursor + local_counts[i])]
          split_indices[[i]] <- c(split_indices[[i]], rows[selected])
        }
        cursor <- cursor + local_counts[i]
      }
    }
    split_indices <- lapply(split_indices, sample)
  }

  out <- structure(
    list(
      train = .subset_uniml_data(data, split_indices$train),
      validation = .subset_uniml_data(data, split_indices$validation),
      test = if (!is.null(split_indices$test)) .subset_uniml_data(data, split_indices$test) else NULL,
      meta = list(
        prop = prop,
        seed = seed,
        stratify = stratify,
        sizes = vapply(split_indices, length, integer(1)),
        positive_rate = vapply(
          split_indices,
          function(index) .positive_rate(data$outcome[index]),
          numeric(1)
        )
      )
    ),
    class = c("uniml_split", "list")
  )
  validate_uniml_split(out)
  if (stratify && any(vapply(
    list(out$train, out$validation, out$test),
    function(part) {
      !is.null(part) && length(unique(part$outcome)) < 2L
    },
    logical(1)
  ))) {
    warning("At least one split set does not contain both outcome classes.", call. = FALSE)
  }
  out
}

#' Validate a uniml split
#'
#' @param x A `uniml_split` object.
#'
#' @return Invisibly returns `TRUE` when valid.
#' @export
validate_uniml_split <- function(x) {
  if (!inherits(x, "uniml_split")) {
    .abort("`x` must be a `uniml_split` object.")
  }
  if (!inherits(x$train, "uniml_data") || !inherits(x$validation, "uniml_data")) {
    .abort("`train` and `validation` must be uniml data objects.")
  }
  if (!is.null(x$test) && !inherits(x$test, "uniml_data")) {
    .abort("`test` must be NULL or a uniml data object.")
  }
  if (!is.list(x$meta)) {
    .abort("`meta` must be a list.")
  }
  invisible(TRUE)
}

#' Print a uniml split
#'
#' @param x A `uniml_split` object.
#' @param ... Additional arguments passed to methods.
#'
#' @return Invisibly returns `x`.
#' @export
print.uniml_split <- function(x, ...) {
  validate_uniml_split(x)
  cat("<uniml_split>\n")
  cat(sprintf("seed: %s, stratify: %s\n", x$meta$seed, x$meta$stratify))
  for (part in names(x$meta$sizes)) {
    cat(sprintf(
      "%s: n = %d, positive rate = %.3f\n",
      part,
      x$meta$sizes[[part]],
      x$meta$positive_rate[[part]]
    ))
  }
  invisible(x)
}

#' Construct a uniml result container
#'
#' @param ... Named result components.
#'
#' @return A `uniml_result` object.
#' @export
new_uniml_result <- function(...) {
  fields <- list(
    data_info = NULL,
    preprocess = NULL,
    selection = NULL,
    tuning = NULL,
    cv = NULL,
    metrics = NULL,
    ci = NULL,
    calibration = NULL,
    tests = NULL,
    dca = NULL,
    ranking = NULL,
    shap = NULL,
    thresholds = NULL,
    plots = NULL,
    models = NULL,
    meta = list()
  )
  dots <- list(...)
  if (length(dots) > 0L) {
    if (is.null(names(dots)) || any(!nzchar(names(dots)))) {
      .abort("All result components in `...` must be named.")
    }
    unknown <- setdiff(names(dots), names(fields))
    if (length(unknown) > 0L) {
      .abort(sprintf("Unknown result component(s): %s.", paste(unknown, collapse = ", ")))
    }
    fields[names(dots)] <- dots
  }
  out <- structure(fields, class = c("uniml_result", "list"))
  validate_uniml_result(out)
  out
}

#' Validate a uniml result
#'
#' @param x A `uniml_result` object.
#'
#' @return Invisibly returns `TRUE` when valid.
#' @export
validate_uniml_result <- function(x) {
  if (!inherits(x, "uniml_result")) {
    .abort("`x` must be a `uniml_result` object.")
  }
  expected <- c(
    "data_info", "preprocess", "selection", "tuning", "cv", "metrics",
    "ci", "calibration", "tests", "dca", "ranking", "shap", "thresholds",
    "plots", "models", "meta"
  )
  if (!all(expected %in% names(x))) {
    .abort("`x` is missing one or more result components.")
  }
  if (!is.list(x$meta)) {
    .abort("`meta` must be a list.")
  }
  invisible(TRUE)
}

#' Print a uniml result
#'
#' @param x A `uniml_result` object.
#' @param ... Additional arguments passed to methods.
#'
#' @return Invisibly returns `x`.
#' @export
print.uniml_result <- function(x, ...) {
  validate_uniml_result(x)
  cat("<uniml_result>\n")
  for (name in names(x)) {
    value <- x[[name]]
    if (is.null(value)) {
      next
    }
    if (is.data.frame(value)) {
      label <- sprintf("%s (%d x %d)", name, nrow(value), ncol(value))
    } else if (is.list(value)) {
      label <- sprintf("%s (list, %d elements)", name, length(value))
    } else {
      label <- sprintf("%s (%s)", name, class(value)[1L])
    }
    cat(sprintf("%s\n", label))
  }
  invisible(x)
}

#' Summarise a uniml result
#'
#' @param object A `uniml_result` object.
#' @param ... Additional arguments passed to methods.
#'
#' @return A summary list.
#' @export
summary.uniml_result <- function(object, ...) {
  validate_uniml_result(object)
  populated <- names(object)[!vapply(object, is.null, logical(1))]
  structure(
    list(
      class = class(object),
      populated = populated,
      empty = setdiff(names(object), populated)
    ),
    class = "summary.uniml_result"
  )
}

#' @export
print.summary.uniml_result <- function(x, ...) {
  cat("<summary.uniml_result>\n")
  cat(sprintf("populated: %s\n", if (length(x$populated)) paste(x$populated, collapse = ", ") else "none"))
  cat(sprintf("empty: %s\n", if (length(x$empty)) paste(x$empty, collapse = ", ") else "none"))
  invisible(x)
}

.validate_spec_object <- function(spec) {
  validate_uniml_spec(spec)
  invisible(TRUE)
}

.merge_named_list <- function(x, values) {
  if (length(values) == 0L) {
    return(x)
  }
  if (is.null(names(values)) || any(!nzchar(names(values)))) {
    .abort("Additional arguments must be named.")
  }
  x[names(values)] <- values
  x
}

#' Set preprocessing options
#'
#' @param spec A `uniml_spec` object.
#' @param impute Imputation method.
#' @param m Number of imputations when applicable.
#' @param scale Scaling method.
#' @param encode Encoding method.
#' @param ordered_as Ordered-factor handling.
#' @param outcome_na_action How to handle missing outcome values.
#' @param balance Class-balance method.
#' @param impute_args Named list forwarded to [fit_imputer()].
#' @param scale_args Named list forwarded to [fit_scale()].
#' @param encode_args Named list forwarded to [fit_encode()].
#' @param balance_args Named list forwarded to [fit_balance()].
#' @param ... Additional preprocessing fields.
#'
#' @return Updated `uniml_spec`.
#' @examples
#' spec <- uniml_spec("binary", "y", "yes")
#' spec <- set_preprocess(
#'   spec,
#'   impute = "median_mode",
#'   scale = "zscore",
#'   encode = "onehot"
#' )
#' spec$preprocess$impute
#' @export
set_preprocess <- function(spec, impute = NULL, m = NULL, scale = NULL, encode = NULL,
                           ordered_as = NULL, outcome_na_action = NULL, balance = NULL,
                           impute_args = NULL, scale_args = NULL, encode_args = NULL,
                           balance_args = NULL, ...) {
  .validate_spec_object(spec)
  .assert_step_args(impute_args, .allowed_step_args("impute"), "impute_args")
  .assert_step_args(scale_args, .allowed_step_args("scale"), "scale_args")
  .assert_step_args(encode_args, .allowed_step_args("encode"), "encode_args")
  .assert_step_args(balance_args, .allowed_step_args("balance"), "balance_args")
  values <- list(
    impute = impute,
    m = m,
    scale = scale,
    encode = encode,
    ordered_as = ordered_as,
    outcome_na_action = outcome_na_action,
    balance = balance,
    impute_args = impute_args,
    scale_args = scale_args,
    encode_args = encode_args,
    balance_args = balance_args
  )
  values <- values[!vapply(values, is.null, logical(1))]
  spec$preprocess <- .merge_named_list(spec$preprocess, values)
  spec$preprocess <- .merge_named_list(spec$preprocess, list(...))

  if (!is.null(impute)) {
    spec$preprocess$impute <- impute
  }
  if (!is.null(m)) {
    spec$preprocess$m <- m
  }
  if (!is.null(scale)) {
    spec$preprocess$scale <- scale
  }
  if (!is.null(encode)) {
    spec$preprocess$encode <- encode
  }
  if (!is.null(balance)) {
    spec$preprocess$balance <- balance
  }
  if (!is.null(ordered_as)) {
    spec$ordered_as <- ordered_as
  }
  if (!is.null(outcome_na_action)) {
    spec$outcome_na_action <- outcome_na_action
  }
  spec$preprocess$ordered_as <- spec$ordered_as
  spec$preprocess$outcome_na_action <- spec$outcome_na_action
  validate_uniml_spec(spec)
  spec
}

#' Set feature-selection options
#'
#' @param spec A `uniml_spec` object.
#' @param methods Feature-selection method names.
#' @param lambda LASSO lambda rule.
#' @param p_threshold Univariate p-value threshold.
#' @param maxRuns Boruta maximum runs.
#' @param multivariate Multivariable selection method.
#' @param combine How selected feature sets are combined.
#' @param representation Feature representation used for modelling.
#' @param k Optional target number of features for RFE or importance selection.
#' @param cor_threshold Optional absolute-correlation threshold for collinearity removal.
#' @param vif_threshold Optional VIF threshold for collinearity removal.
#' @param args Named list of method-specific arguments, e.g.
#'   `list(lasso = list(nfolds = 5), boruta = list(tentative = TRUE))`.
#' @param ... Additional selection fields.
#'
#' @return Updated `uniml_spec`.
#' @examples
#' spec <- uniml_spec("binary", "y", "yes")
#' spec <- set_feature_selection(
#'   spec,
#'   methods = c("univariate", "lasso"),
#'   combine = "intersect"
#' )
#' spec$selection$methods
#' @export
set_feature_selection <- function(spec, methods = NULL, lambda = NULL, p_threshold = NULL,
                                  maxRuns = NULL, multivariate = NULL, combine = NULL,
                                  representation = NULL, k = NULL, cor_threshold = NULL,
                                  vif_threshold = NULL, args = NULL, ...) {
  .validate_spec_object(spec)
  .assert_named_list(args, "args")
  if (length(args) > 0L) {
    unknown <- setdiff(names(args), c(
      "univariate", "lasso", "boruta", "rf_importance", "rfe", "multivariate"
    ))
    if (length(unknown) > 0L) {
      .abort(sprintf(
        "`args` contains unknown method name(s): %s.",
        paste(unknown, collapse = ", ")
      ))
    }
    for (method in names(args)) {
      .assert_step_args(
        args[[method]],
        .allowed_selection_args(method),
        sprintf("args$%s", method)
      )
    }
  }
  values <- list(
    methods = methods,
    lambda = lambda,
    p_threshold = p_threshold,
    maxRuns = maxRuns,
    multivariate = multivariate,
    combine = combine,
    representation = representation,
    k = k,
    cor_threshold = cor_threshold,
    vif_threshold = vif_threshold,
    args = args
  )
  values <- values[!vapply(values, is.null, logical(1))]
  spec$selection <- .merge_named_list(spec$selection, values)
  spec$selection <- .merge_named_list(spec$selection, list(...))
  validate_uniml_spec(spec)
  spec
}

#' Set learners
#'
#' @param spec A `uniml_spec` object.
#' @param learners Learner keys or learner objects.
#' @param tune Whether to tune the configured learners.
#' @param params Optional learner parameters.
#' @param search_space Optional parameter search space.
#'
#' @return Updated `uniml_spec`.
#' @examples
#' spec <- uniml_spec("binary", "y", "yes")
#' spec <- set_learners(spec, c("lr", "cart"), tune = FALSE)
#' spec
#' @export
set_learners <- function(spec, learners, tune = TRUE, params = NULL, search_space = NULL) {
  .validate_spec_object(spec)
  if (!(is.character(learners) || is.list(learners)) || length(learners) == 0L) {
    .abort("`learners` must be a non-empty character vector or list.")
  }
  if (is.character(learners) && (anyNA(learners) || any(!nzchar(learners)))) {
    .abort("Character learner keys must not be missing or empty.")
  }
  .assert_scalar_logical(tune, "tune")
  spec$learners <- list(
    learners = learners,
    tune = tune,
    params = params,
    search_space = search_space
  )
  validate_uniml_spec(spec)
  spec
}

#' Set the data-leakage policy
#'
#' @param spec A `uniml_spec` object.
#' @param mode Leakage policy mode.
#' @param in_cv Steps fitted inside cross-validation.
#'
#' @return Updated `uniml_spec`.
#' @examples
#' spec <- uniml_spec("binary", "y", "yes")
#' spec <- set_leakage_policy(
#'   spec,
#'   mode = "strict_in",
#'   in_cv = c("selection", "balance")
#' )
#' spec$leakage_policy
#' spec$in_cv
#' @export
set_leakage_policy <- function(spec, mode = "custom",
                               in_cv = c("selection", "balance", "tuning")) {
  .validate_spec_object(spec)
  .assert_choice(mode, "mode", c("strict_in", "strict_out", "custom"))
  if (identical(mode, "strict_in") && missing(in_cv)) {
    in_cv <- .allowed_leakage_steps()
  }
  if (identical(mode, "strict_out") && missing(in_cv)) {
    in_cv <- character()
  }
  .assert_character_vector(in_cv, "in_cv", choices = .allowed_leakage_steps())
  spec$leakage_policy <- mode
  spec$in_cv <- in_cv
  validate_uniml_spec(spec)
  spec
}

#' Set tuning options
#'
#' @param spec A `uniml_spec` object.
#' @param tuner Tuning strategy.
#' @param folds Number of cross-validation folds.
#' @param repeats Number of cross-validation repeats.
#' @param evals Number of tuning evaluations.
#' @param metric Tuning metric.
#' @param nested Whether tuning is nested.
#' @param seed Random seed.
#' @param terminator Tuning terminator: `"evals"` or `"seconds"`.
#' @param seconds Wall-clock budget used when `terminator = "seconds"`.
#' @param ... Additional tuning fields.
#'
#' @return Updated `uniml_spec`.
#' @examples
#' spec <- uniml_spec("binary", "y", "yes")
#' spec <- set_tuning(
#'   spec,
#'   tuner = "random_search",
#'   folds = 5,
#'   evals = 20,
#'   metric = "auc"
#' )
#' spec
#' @export
set_tuning <- function(spec, tuner = "random_search", folds = 5, repeats = 1,
                       evals = 30, metric = "auc", nested = FALSE, seed = 123,
                       terminator = "evals", seconds = NULL, ...) {
  .validate_spec_object(spec)
  .assert_choice(terminator, "terminator", c("evals", "seconds"))
  if (!is.null(seconds)) {
    .assert_scalar_number(seconds, "seconds", min = 1)
  }
  values <- list(
    tuner = tuner,
    folds = folds,
    repeats = repeats,
    evals = evals,
    terminator = terminator,
    seconds = seconds,
    metric = metric,
    nested = nested,
    seed = seed
  )
  spec$tuning <- .merge_named_list(spec$tuning, values)
  spec$tuning <- .merge_named_list(spec$tuning, list(...))
  spec$nested_tuning <- nested
  spec$seed <- seed
  spec$tuning$nested <- nested
  spec$tuning$seed <- seed
  validate_uniml_spec(spec)
  spec
}

#' Set evaluation options
#'
#' @param spec A `uniml_spec` object.
#' @param metrics Metrics to calculate.
#' @param ci Confidence-interval method.
#' @param threshold_method Classification-threshold method.
#' @param cost Cost vector for cost-based thresholds.
#' @param fixed_threshold Fixed threshold when requested.
#' @param reference_model Reference model for NRI or IDI.
#' @param best_model Best-model selection metric.
#' @param calibration Calibration method.
#' @param calibration_args Named list with `folds`, `bins`, and `fit`
#'   (`"out_of_fold"` or `"in_sample"`).
#' @param tests Post-model statistical tests.
#' @param dca Whether to calculate decision-curve analysis.
#' @param select_on Data set used for model comparison.
#' @param ... Additional evaluation fields.
#'
#' @return Updated `uniml_spec`.
#' @examples
#' spec <- uniml_spec("binary", "y", "yes")
#' spec <- set_evaluation(
#'   spec,
#'   ci = "bootstrap",
#'   calibration = "platt",
#'   select_on = "validation"
#' )
#' spec
#' @export
set_evaluation <- function(spec, metrics = "all", ci = "bootstrap",
                           threshold_method = "youden", cost = c(fp = 1, fn = 1),
                           fixed_threshold = NULL, reference_model = NULL,
                           best_model = "composite", calibration = "none",
                           calibration_args = NULL, tests = c("delong", "hl"),
                           dca = TRUE, select_on = "validation", ...) {
  .validate_spec_object(spec)
  .assert_step_args(
    calibration_args,
    .allowed_calibration_args(),
    "calibration_args"
  )
  if (!is.null(calibration_args)) {
    existing <- spec$evaluation$calibration_args
    if (is.null(existing)) {
      existing <- list(folds = 5L, bins = 10L, fit = "out_of_fold")
    }
    existing[names(calibration_args)] <- calibration_args
    calibration_args <- existing
  }
  values <- list(
    metrics = metrics,
    ci = ci,
    threshold_method = threshold_method,
    cost = cost,
    fixed_threshold = fixed_threshold,
    reference_model = reference_model,
    best_model = best_model,
    calibration = calibration,
    calibration_args = calibration_args,
    tests = tests,
    dca = dca,
    select_on = select_on
  )
  values <- values[!vapply(values, is.null, logical(1))]
  spec$evaluation <- .merge_named_list(spec$evaluation, values)
  spec$evaluation <- .merge_named_list(spec$evaluation, list(...))
  spec$evaluation$best_model <- best_model
  spec <- .merge_named_list(spec, list(
    threshold_method = threshold_method,
    cost = cost,
    fixed_threshold = fixed_threshold,
    reference_model = reference_model,
    best_model_metric = best_model,
    select_on = select_on
  ))
  validate_uniml_spec(spec)
  spec
}

#' Set explanation options
#'
#' @param spec A `uniml_spec` object.
#' @param model Model to explain.
#' @param plots SHAP plots to produce: any of `"importance"`, `"beeswarm"`,
#'   `"dependence"`, `"waterfall"`, and `"force"`.
#' @param thresholds Optional list configuring SHAP zero-crossing thresholds:
#'   `targets` (character vector), `interaction`, `min_n`, `min_unique`, `span`.
#' @param ... Additional explanation fields.
#'
#' @return Updated `uniml_spec`.
#' @examples
#' spec <- uniml_spec("binary", "y", "yes")
#' spec <- set_explain(spec, model = "best", plots = c("importance", "beeswarm"))
#' spec
#' @export
set_explain <- function(spec, model = "best",
                        plots = c("importance", "beeswarm", "dependence",
                                  "waterfall"),
                        thresholds = NULL, ...) {
  .validate_spec_object(spec)
  spec$explain <- .merge_named_list(
    spec$explain,
    list(
      model = model,
      plots = plots,
      thresholds = thresholds
    )
  )
  spec$explain <- .merge_named_list(spec$explain, list(...))
  validate_uniml_spec(spec)
  spec
}

#' Set display labels
#'
#' @param spec A `uniml_spec` object.
#' @param mapping A named character vector mapping column names to labels.
#'
#' @return Updated `uniml_spec`.
#' @examples
#' spec <- uniml_spec("binary", "y", "yes")
#' spec <- set_labels(spec, c(age = "Age (years)"))
#' spec
#' @export
set_labels <- function(spec, mapping = NULL) {
  .validate_spec_object(spec)
  .assert_labels(mapping)
  spec <- .merge_named_list(spec, list(labels = mapping))
  validate_uniml_spec(spec)
  spec
}

#' Set output options
#'
#' @param spec A `uniml_spec` object.
#' @param verbose Verbosity level from 0 to 2.
#' @param n_workers Number of workers.
#' @param future_strategy Future backend strategy.
#' @param dir Output directory.
#' @param ... Additional output fields.
#'
#' @return Updated `uniml_spec`.
#' @examples
#' spec <- uniml_spec("binary", "y", "yes")
#' spec <- set_output(spec, dir = tempfile("uniml-out-"))
#' spec
#' @export
set_output <- function(spec, verbose = 1, n_workers = 1, future_strategy = "multisession",
                       dir = "output", ...) {
  .validate_spec_object(spec)
  spec$output <- .merge_named_list(
    spec$output,
    list(
      verbose = verbose,
      n_workers = n_workers,
      future_strategy = future_strategy,
      dir = dir
    )
  )
  spec$output <- .merge_named_list(spec$output, list(...))
  spec$verbose <- verbose
  spec$n_workers <- n_workers
  spec$future_strategy <- future_strategy
  validate_uniml_spec(spec)
  spec
}
