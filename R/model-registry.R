# L4 learner registry --------------------------------------------------------

.uniml_learner_registry <- new.env(parent = emptyenv())

.uniml_default_learner_keys <- function() {
  c("lr", "rf", "xgb", "nnet", "knn", "cart", "nb")
}

.uniml_optional_learner_keys <- function() {
  c("log_reg", "svm", "lda", "qda")
}

.is_param_set <- function(x) {
  inherits(x, "ParamSet")
}

.assert_learner <- function(learner, name = "learner") {
  if (!inherits(learner, "Learner")) {
    .abort(sprintf("`%s` must be an mlr3 learner.", name))
  }
  invisible(TRUE)
}

.set_probability_predict_type <- function(learner) {
  if ("prob" %in% learner$predict_types) {
    learner$predict_type <- "prob"
  }
  learner
}

.set_safe_learner_defaults <- function(learner, seed = 123L) {
  values <- list()
  ids <- learner$param_set$ids()

  if ("seed" %in% ids) {
    values$seed <- as.integer(seed)
  }
  if ("num.threads" %in% ids) {
    values$num.threads <- 1L
  }
  if ("nthread" %in% ids) {
    values$nthread <- 1L
  }
  if ("num_threads" %in% ids) {
    values$num_threads <- 1L
  }
  if (length(values) > 0L) {
    learner$param_set$set_values(.values = values)
  }
  learner
}

.require_learner_dictionaries <- function() {
  .check_packages(c("mlr3", "mlr3learners"))
  invisible(TRUE)
}

# Keys that used to be built in and now need a non-CRAN learner package. They
# get an explicit migration hint instead of the generic "unknown learner" error.
.retired_learner_hints <- function() {
  c(
    lgb = "classif.lightgbm",
    randomForest = "classif.randomForest",
    gbm = "classif.gbm"
  )
}

.retired_learner_hint <- function(key) {
  target <- .retired_learner_hints()[[key]]
  if (is.null(target)) {
    return(sprintf("Unknown learner key \"%s\".", key))
  }
  sprintf(
    paste(
      "The \"%s\" key is no longer built in because its mlr3 learner lives in",
      "'mlr3extralearners', which is not on CRAN. Install and load that",
      "package, then pass the mlr3 learner key directly:",
      "set_learners(spec, \"%s\")."
    ),
    key,
    target
  )
}

.make_default_learner <- function(key, seed = 123L) {
  .require_learner_dictionaries()
  learner <- switch(
    key,
    lr = mlr3::lrn(
      "classif.cv_glmnet",
      predict_type = "prob",
      nfolds = 5L,
      standardize = TRUE,
      type.measure = "deviance"
    ),
    rf = mlr3::lrn(
      "classif.ranger",
      predict_type = "prob",
      importance = "permutation",
      replace = FALSE,
      respect.unordered.factors = "order"
    ),
    xgb = mlr3::lrn(
      "classif.xgboost",
      predict_type = "prob",
      verbose = 0L,
      nthread = 1L
    ),
    nnet = mlr3::lrn(
      "classif.nnet",
      predict_type = "prob",
      trace = FALSE,
      MaxNWts = 2000L
    ),
    knn = mlr3::lrn(
      "classif.kknn",
      predict_type = "prob",
      kernel = "gaussian"
    ),
    cart = mlr3::lrn(
      "classif.rpart",
      predict_type = "prob",
      model = TRUE,
      xval = 10L
    ),
    nb = mlr3::lrn(
      "classif.naive_bayes",
      predict_type = "prob",
      laplace = 1
    ),
    .abort(sprintf("Unknown default learner key \"%s\".", key))
  )
  .set_probability_predict_type(learner)
  .set_safe_learner_defaults(learner, seed = seed)
}

.make_optional_learner <- function(key, seed = 123L) {
  .require_learner_dictionaries()
  learner <- switch(
    key,
    log_reg = mlr3::lrn(
      "classif.log_reg",
      predict_type = "prob"
    ),
    svm = mlr3::lrn(
      "classif.svm",
      predict_type = "prob",
      type = "C-classification",
      kernel = "radial"
    ),
    lda = mlr3::lrn(
      "classif.lda",
      predict_type = "prob"
    ),
    qda = mlr3::lrn(
      "classif.qda",
      predict_type = "prob"
    ),
    .abort(sprintf("Unknown optional learner key \"%s\".", key))
  )
  .set_probability_predict_type(learner)
  .set_safe_learner_defaults(learner, seed = seed)
}

.default_learner_search_space <- function(key, task, n_train = NULL) {
  if (!inherits(task, "TaskClassif")) {
    .abort("Dynamic learner search spaces require an mlr3 classification task.")
  }
  n_features <- task$n_features
  n_rows <- task$nrow
  if (is.null(n_train)) {
    n_train <- n_rows
  }
  .assert_scalar_number(n_train, "n_train", integer = TRUE, min = 1)

  switch(
    key,
    lr = paradox::ps(
      alpha = paradox::p_dbl(lower = 0.2, upper = 0.8),
      s = paradox::p_fct(levels = "lambda.1se")
    ),
    rf = {
      mtry_max <- max(1L, min(floor(n_features / 2), n_features))
      mtry_max <- max(3L, mtry_max)
      mtry_max <- min(mtry_max, n_features)
      mtry_lower <- max(2L, floor(n_features / 5))
      mtry_upper <- mtry_max
      if (mtry_lower > mtry_upper) {
        mtry_lower <- 1L
        mtry_upper <- min(5L, n_features)
      }
      paradox::ps(
        num.trees = paradox::p_int(lower = 100L, upper = 300L),
        mtry = paradox::p_int(lower = mtry_lower, upper = mtry_upper),
        min.node.size = paradox::p_int(lower = 5L, upper = 20L),
        sample.fraction = paradox::p_dbl(lower = 0.5, upper = 0.7),
        splitrule = paradox::p_fct(levels = "gini"),
        max.depth = paradox::p_int(lower = 5L, upper = 10L),
        regularization.factor = paradox::p_dbl(lower = 0.1, upper = 1)
      )
    },
    xgb = paradox::ps(
      nrounds = paradox::p_int(lower = 50L, upper = 150L),
      eta = paradox::p_dbl(lower = 0.01, upper = 0.1),
      max_depth = paradox::p_int(lower = 3L, upper = 6L),
      subsample = paradox::p_dbl(lower = 0.6, upper = 0.8),
      colsample_bytree = paradox::p_dbl(lower = 0.6, upper = 0.8),
      lambda = paradox::p_dbl(lower = 1, upper = 3),
      alpha = paradox::p_dbl(lower = 0, upper = 1)
    ),
    nnet = paradox::ps(
      size = paradox::p_int(lower = 3L, upper = 10L),
      decay = paradox::p_dbl(lower = 0.001, upper = 0.05),
      maxit = paradox::p_int(lower = 150L, upper = 300L)
    ),
    knn = {
      k_upper <- min(5000L, max(1L, as.integer(n_train)))
      k_lower <- min(30L, k_upper)
      paradox::ps(
        k = paradox::p_int(lower = k_lower, upper = k_upper),
        distance = paradox::p_int(lower = 1L, upper = 4L),
        kernel = paradox::p_fct(
          levels = c("triangular", "gaussian", "optimal")
        )
      )
    },
    cart = paradox::ps(
      minsplit = paradox::p_int(lower = 20L, upper = 40L),
      minbucket = paradox::p_int(lower = 7L, upper = 15L),
      cp = paradox::p_dbl(lower = 0.001, upper = 0.03),
      maxdepth = paradox::p_int(lower = 3L, upper = 7L)
    ),
    nb = paradox::ps(
      laplace = paradox::p_dbl(lower = 0.5, upper = 3)
    ),
    NULL
  )
}

.register_builtin_learner <- function(key) {
  assign(
    key,
    list(
      source = "builtin",
      factory = function(seed = 123L) .make_default_learner(key, seed = seed),
      search_space = NULL
    ),
    envir = .uniml_learner_registry
  )
  invisible(TRUE)
}

.register_default_learners <- function() {
  for (key in .uniml_default_learner_keys()) {
    .register_builtin_learner(key)
  }
  invisible(TRUE)
}

.register_optional_learners <- function() {
  for (key in .uniml_optional_learner_keys()) {
    assign(
      key,
      list(
        source = "optional",
        factory = function(seed = 123L) .make_optional_learner(key, seed = seed),
        search_space = NULL
      ),
      envir = .uniml_learner_registry
    )
  }
  invisible(TRUE)
}

.learner_setting_for <- function(value, aliases, learner, name) {
  if (is.null(value)) {
    return(NULL)
  }
  if (identical(name, "search_space")) {
    if (.is_param_set(value)) {
      return(value)
    }
    if (is.function(value)) {
      return(value)
    }
  }
  if (!is.list(value) || .is_param_set(value)) {
    .abort(sprintf("`learners$%s` has an unsupported value.", name))
  }
  if (length(value) == 0L) {
    return(NULL)
  }

  value_names <- names(value)
  if (!is.null(value_names) && any(value_names %in% aliases)) {
    index <- which(value_names %in% aliases)[1L]
    return(value[[index]])
  }

  if (identical(name, "search_space")) {
    .abort(sprintf(
      "`learners$search_space` must be a ParamSet, a builder function, or a named list keyed by learner."
    ))
  }
  value
}

.learner_alias <- function(key, learner, item_name = NULL) {
  unique(c(item_name, key, learner$id))
}

.apply_learner_params <- function(learner, params) {
  if (is.null(params) || length(params) == 0L) {
    return(learner)
  }
  if (!is.list(params) || is.null(names(params)) || any(!nzchar(names(params)))) {
    .abort("Learner parameters must be a non-empty named list.")
  }
  learner$param_set$set_values(.values = params)
  learner
}

.resolve_learner_search_space <- function(entry, key, task, params, n_train = NULL) {
  explicit <- entry$search_space
  if (is.null(explicit) && identical(entry$source, "builtin")) {
    if (is.null(task)) {
      return(NULL)
    }
    return(.default_learner_search_space(key, task, n_train = n_train))
  }
  if (is.null(explicit)) {
    return(NULL)
  }
  if (is.function(explicit)) {
    if (is.null(task)) {
      return(explicit)
    }
    return(explicit(task, params = params))
  }
  explicit
}

.make_learner_spec <- function(key, learner, params, search_space, tune,
                               source, item_name = NULL, metric = "auc") {
  structure(
    list(
      key = key,
      learner = learner,
      params = params,
      search_space = search_space,
      tune = tune,
      source = source,
      metric = metric,
      aliases = .learner_alias(key, learner, item_name = item_name)
    ),
    class = c("uniml_learner_spec", "list")
  )
}

#' Register a learner for use by `resolve_learners()`
#'
#' Registers an mlr3 learner and an optional parameter search space. Registering
#' an existing key replaces the previous entry.
#'
#' @param key Single character learner key.
#' @param learner An mlr3 learner object.
#' @param search_space Optional `paradox::ParamSet` used for tuning.
#'
#' @return Invisibly returns the registered learner specification.
#' @export
#' @examples
#' \dontrun{
#' register_learner(
#'   "rpart_custom",
#'   mlr3::lrn("classif.rpart", predict_type = "prob"),
#'   paradox::ps(cp = paradox::p_dbl(0.001, 0.1))
#' )
#' }
register_learner <- function(key, learner, search_space = NULL) {
  .assert_scalar_character(key, "key")
  .assert_learner(learner)
  if (!is.null(search_space) && !.is_param_set(search_space) &&
      !is.function(search_space)) {
    .abort("`search_space` must be NULL, a ParamSet, or a builder function.")
  }
  entry <- list(
    source = "custom",
    learner = learner$clone(deep = TRUE),
    search_space = search_space
  )
  assign(key, entry, envir = .uniml_learner_registry)
  invisible(.make_learner_spec(
    key = key,
    learner = entry$learner$clone(deep = TRUE),
    params = NULL,
    search_space = search_space,
    tune = !is.null(search_space),
    source = "custom",
    metric = "auc"
  ))
}

.resolve_learner_item <- function(item, item_name, spec, task, n_train = NULL) {
  if (is.character(item)) {
    if (length(item) != 1L || is.na(item) || !nzchar(item)) {
      .abort("Character learner entries must be single non-empty keys.")
    }
    key <- item
  } else if (inherits(item, "Learner")) {
    key <- if (!is.null(item_name) && nzchar(item_name)) item_name else item$id
  } else {
    .abort("Each learner must be an mlr3 learner key or learner object.")
  }

  .require_learner_dictionaries()
  entry <- if (exists(key, envir = .uniml_learner_registry, inherits = FALSE)) {
    get(key, envir = .uniml_learner_registry, inherits = FALSE)
  } else {
    NULL
  }

  if (is.character(item) && is.null(entry)) {
    if (!mlr3::mlr_learners$has(key)) {
      if (key %in% names(.retired_learner_hints())) {
        .abort(.retired_learner_hint(key))
      }
      .abort(sprintf("Unknown mlr3 learner key \"%s\".", key))
    }
    entry <- list(
      source = "external",
      learner = mlr3::lrn(key),
      search_space = NULL
    )
  }
  if (inherits(item, "Learner")) {
    entry <- list(
      source = "custom",
      learner = item,
      search_space = NULL
    )
  }
  if (is.null(entry)) {
    .abort(sprintf("Could not resolve learner \"%s\".", key))
  }

  learner <- if (!is.null(entry$learner)) {
    entry$learner$clone(deep = TRUE)
  } else {
    entry$factory(seed = spec$tuning$seed)
  }
  if (!identical(learner$task_type, "classif")) {
    .abort(sprintf(
      "Learner \"%s\" has task type \"%s\"; only classification learners are supported in M6.",
      key,
      learner$task_type
    ))
  }
  .set_probability_predict_type(learner)
  .set_safe_learner_defaults(learner, seed = spec$tuning$seed)

  aliases <- .learner_alias(key, learner, item_name = item_name)
  params <- .learner_setting_for(
    spec$learners$params,
    aliases = aliases,
    learner = learner,
    name = "params"
  )
  learner <- .apply_learner_params(learner, params)

  explicit_search_space <- .learner_setting_for(
    spec$learners$search_space,
    aliases = aliases,
    learner = learner,
    name = "search_space"
  )
  search_space <- if (is.null(explicit_search_space)) {
    entry$search_space
  } else {
    explicit_search_space
  }
  entry_for_space <- list(
    source = entry$source,
    search_space = search_space
  )
  resolved_space <- .resolve_learner_search_space(
    entry_for_space,
    key = key,
    task = task,
    params = params,
    n_train = n_train
  )

  tune <- isTRUE(spec$learners$tune) &&
    (identical(entry$source, "builtin") || !is.null(search_space))

  .make_learner_spec(
    key = key,
    learner = learner,
    params = params,
    search_space = resolved_space,
    tune = tune,
    source = entry$source,
    item_name = item_name,
    metric = spec$tuning$metric
  )
}

.resolve_learner_items <- function(spec) {
  learners <- spec$learners$learners
  if (is.character(learners)) {
    return(lapply(learners, function(key) list(item = key, name = NULL)))
  }
  if (!is.list(learners)) {
    .abort("`spec$learners$learners` must be a character vector or list.")
  }
  item_names <- names(learners)
  lapply(seq_along(learners), function(index) {
    name <- if (!is.null(item_names) && nzchar(item_names[index])) {
      item_names[index]
    } else {
      NULL
    }
    list(item = learners[[index]], name = name)
  })
}

#' Resolve learners from a uniml specification
#'
#' Converts learner keys and learner objects in `spec` into learner
#' specifications containing an mlr3 learner, parameter values, search space,
#' and tuning flag.
#'
#' @param spec A `uniml_spec` object.
#' @param task Optional mlr3 classification task used to resolve task-dependent
#'   search spaces such as `mtry`.
#' @param n_train Optional number of rows available to a learner during a
#'   training fold. Used to keep sample-size-dependent search spaces safe.
#'
#' @return A list of `uniml_learner_spec` objects.
#' @export
#' @examples
#' \dontrun{
#' spec <- uniml_spec(task_type = "binary", outcome = "y", positive = "yes")
#' spec <- set_learners(spec, c("lr", "rf"))
#' resolve_learners(spec)
#' }
resolve_learners <- function(spec, task = NULL, n_train = NULL) {
  validate_uniml_spec(spec)
  if (!is.null(task) && !inherits(task, "TaskClassif")) {
    .abort("`task` must be NULL or an mlr3 classification task.")
  }
  if (!is.null(n_train)) {
    .assert_scalar_number(n_train, "n_train", integer = TRUE, min = 1)
    if (is.null(task)) {
      .abort("`n_train` requires a classification `task`.")
    }
    n_train <- min(as.integer(n_train), task$nrow)
  }
  items <- .resolve_learner_items(spec)
  if (length(items) == 0L) {
    .abort("At least one learner must be configured.")
  }
  resolved <- lapply(items, function(item) {
    .resolve_learner_item(
      item$item,
      item$name,
      spec = spec,
      task = task,
      n_train = n_train
    )
  })
  names(resolved) <- vapply(resolved, `[[`, character(1), "key")
  if (anyDuplicated(names(resolved)) > 0L) {
    .abort("Learner keys must be unique after resolution.")
  }
  resolved
}

#' Build an mlr3 classification task
#'
#' @param data A data frame or `uniml_data` object.
#' @param spec A `uniml_spec` object.
#'
#' @return An `mlr3::TaskClassif`.
#' @export
#' @examples
#' data <- data.frame(
#'   y = factor(c("no", "yes", "no", "yes")),
#'   x = c(0, 1, 0.5, 0.8)
#' )
#' spec <- uniml_spec(task_type = "binary", outcome = "y", positive = "yes")
#' make_task(data, spec)
make_task <- function(data, spec) {
  validate_uniml_spec(spec)
  if (inherits(data, "TaskClassif")) {
    task <- data$clone(deep = TRUE)
    if (!identical(task$target_names, spec$outcome)) {
      .abort("`spec$outcome` does not match the target name in `task`.")
    }
    expected_positive <- if (is.null(spec$positive)) NA_character_ else as.character(spec$positive)
    observed_positive <- if (length(task$positive) == 0L) NA_character_ else as.character(task$positive)
    if (!identical(observed_positive, expected_positive)) {
      .abort("`spec$positive` does not match the positive class in `task`.")
    }
    return(task)
  }

  if (inherits(data, "uniml_data")) {
    validate_uniml_data(data)
    if (!identical(data$outcome_name, spec$outcome)) {
      .abort("`spec$outcome` does not match `data$outcome_name`.")
    }
    task_data <- data$features
    task_data[[spec$outcome]] <- data$outcome
  } else {
    if (!is.data.frame(data)) {
      .abort("`data` must be a data frame, `uniml_data`, or `TaskClassif`.")
    }
    if (!spec$outcome %in% names(data)) {
      .abort(sprintf("Outcome column \"%s\" is not present in `data`.", spec$outcome))
    }
    if (anyNA(data[[spec$outcome]])) {
      .abort("`make_task()` requires a non-missing outcome.")
    }
    feature_names <- setdiff(names(data), c(spec$outcome, spec$id, spec$exclude))
    if (length(feature_names) == 0L) {
      .abort("At least one feature column is required.")
    }
    task_data <- data[, feature_names, drop = FALSE]
    task_data[[spec$outcome]] <- data[[spec$outcome]]
  }

  outcome_values <- as.character(task_data[[spec$outcome]])
  outcome_levels <- if (is.factor(task_data[[spec$outcome]])) {
    levels(task_data[[spec$outcome]])
  } else {
    unique(outcome_values)
  }
  task_data[[spec$outcome]] <- factor(outcome_values, levels = outcome_levels)
  task <- mlr3::as_task_classif(
    task_data,
    target = spec$outcome,
    # A multiclass task has no positive class; passing character(0) here makes
    # mlr3 treat the outcome as binary.
    positive = if (is.null(spec$positive)) NULL else as.character(spec$positive)
  )
  task$id <- "uniml_task"
  task
}

.register_default_learners()
.register_optional_learners()
