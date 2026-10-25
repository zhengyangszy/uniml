# L9 workflow orchestration --------------------------------------------------
#
# `run_workflow()` chains preprocessing, feature selection, tuning, cross
# validation, calibration, and reporting into a single `uniml_result`.
# `predict_uniml()` reuses that fitted state to score new data without ever
# refitting on the new data.

#' Prepare raw workflow data
#'
#' Drops excluded columns, applies the configured outcome-NA action, and
#' standardises the outcome to a two-level factor.
#'
#' @param data A data frame.
#' @param spec A `uniml_spec` object.
#' @param name Dataset label used in error messages.
#' @param levels Optional outcome levels to reuse.
#' @param require_two Whether both outcome classes are required.
#'
#' @return A list with the prepared `data`, the `outcome` column name, and the
#'   levels of the outcome.
#' @keywords internal
.workflow_prepare_data <- function(data, spec, name = "data", levels = NULL,
                                   require_two = TRUE) {
  if (!is.data.frame(data)) {
    .abort(sprintf("`%s` must be a data frame.", name))
  }
  has_outcome <- spec$outcome %in% names(data)
  if (!has_outcome) {
    .abort(sprintf("`%s` does not contain the outcome column \"%s\".", name, spec$outcome))
  }
  drop <- intersect(unique(c(spec$id, spec$exclude)), names(data))
  drop <- setdiff(drop, spec$outcome)
  if (length(drop) > 0L) {
    data <- data[, setdiff(names(data), drop), drop = FALSE]
  }
  values <- data[[spec$outcome]]
  if (identical(spec$outcome_na_action, "drop")) {
    keep <- !is.na(values)
    if (!all(keep)) {
      data <- data[keep, , drop = FALSE]
      values <- data[[spec$outcome]]
    }
  } else if (anyNA(values)) {
    .abort(sprintf("`%s` has missing outcome values and `outcome_na_action` is \"keep\".", name))
  }
  if (nrow(data) == 0L) {
    .abort(sprintf("`%s` has no rows with a non-missing outcome.", name))
  }
  observed <- sort(unique(as.character(values)))
  if (is.null(levels)) {
    levels <- observed
  } else {
    levels <- as.character(levels)
    unseen <- setdiff(observed, levels)
    if (length(unseen) > 0L) {
      .abort(sprintf(
        "`%s` contains outcome levels that were not present in the training data: %s.",
        name, paste(unseen, collapse = ", ")
      ))
    }
  }
  if (require_two && length(levels) < 2L) {
    .abort(sprintf("`%s` must contain two outcome classes.", name))
  }
  data[[spec$outcome]] <- factor(as.character(values), levels = levels)
  rownames(data) <- NULL
  list(data = data, outcome = spec$outcome, levels = levels)
}

.workflow_prepare_features <- function(data, spec, name = "newdata") {
  if (!is.data.frame(data)) {
    .abort(sprintf("`%s` must be a data frame.", name))
  }
  drop <- intersect(unique(c(spec$id, spec$exclude)), names(data))
  drop <- setdiff(drop, spec$outcome)
  if (length(drop) > 0L) {
    data <- data[, setdiff(names(data), drop), drop = FALSE]
  }
  if (nrow(data) == 0L) {
    .abort(sprintf("`%s` has no rows.", name))
  }
  rownames(data) <- NULL
  data
}

.workflow_fit_preprocessor <- function(data, spec) {
  preprocess <- spec$preprocess
  args <- list(
    data = data,
    impute = preprocess$impute,
    scale = preprocess$scale,
    encode = preprocess$encode,
    balance = preprocess$balance,
    m = preprocess$m,
    outcome = spec$outcome,
    ordered_as = spec$ordered_as,
    seed = spec$seed,
    verbose = spec$verbose
  )
  extra <- list(
    impute = preprocess$impute_args,
    scale = preprocess$scale_args,
    encode = preprocess$encode_args,
    balance = preprocess$balance_args
  )
  for (step in names(extra)) {
    values <- extra[[step]]
    if (length(values) == 0L) {
      next
    }
    # Step-level arguments intentionally win over the pipeline defaults; only
    # the structural arguments (data/outcome) are never overridable.
    values <- values[setdiff(names(values), c("data", "outcome"))]
    args[names(values)] <- values
  }
  do.call(fit_preprocessor, args)
}

.workflow_imputation_count <- function(spec, preprocessor) {
  if (!identical(spec$preprocess$impute, "mice")) {
    return(1L)
  }
  step <- preprocessor$steps$impute
  if (is.null(step) || is.null(step$params$mice)) {
    return(1L)
  }
  m <- tryCatch(as.integer(step$params$mice$m), error = function(e) 1L)
  if (length(m) != 1L || is.na(m) || m < 1L) {
    return(1L)
  }
  m
}

# Rebuild the downstream preprocessing steps (scale/encode/balance) on one
# completed imputation. The shared mice imputer is reused so that held-out data
# are imputed with the same fitted model; the imputation index is stored on the
# step so later transforms stay on this completed data set.
.workflow_completed_preprocessor <- function(completed, spec, full_preprocessor,
                                             imputation = 1L) {
  current <- completed
  impute_step <- full_preprocessor$steps$impute
  impute_step$params$imputation_index <- as.integer(imputation)
  scale_step <- fit_scale(
    current,
    method = full_preprocessor$steps$scale$method,
    outcome = spec$outcome,
    ordered_as = spec$ordered_as,
    verbose = spec$verbose
  )
  current <- apply_scale(scale_step, current)
  encode_step <- fit_encode(
    current,
    method = full_preprocessor$steps$encode$method,
    outcome = spec$outcome,
    ordered_as = spec$ordered_as,
    verbose = spec$verbose
  )
  current <- apply_encode(encode_step, current)
  params <- full_preprocessor$steps$balance$params
  balance_step <- fit_balance(
    current,
    method = full_preprocessor$steps$balance$method,
    outcome = spec$outcome,
    p = params$p,
    n_neighbors = params$n_neighbors,
    dup_size = params$dup_size,
    seed = spec$seed,
    verbose = spec$verbose
  )
  current <- balance_step$data
  preprocessor <- .new_preprocessor(list(
    impute = impute_step,
    scale = scale_step,
    encode = encode_step,
    balance = balance_step
  ))
  preprocessor$var_info <- full_preprocessor$var_info
  preprocessor$train <- current
  preprocessor
}

# One preprocessor per completed imputation. Single-imputation methods return
# the already-fitted preprocessor unchanged so behaviour is identical.
.workflow_imputation_preprocessors <- function(train_data, spec, full_preprocessor) {
  m <- .workflow_imputation_count(spec, full_preprocessor)
  if (m <= 1L) {
    return(list(full_preprocessor))
  }
  completed <- .apply_imputer_all(full_preprocessor$steps$impute, train_data)
  if (length(completed) <= 1L) {
    return(list(full_preprocessor))
  }
  lapply(seq_along(completed), function(index) {
    .workflow_completed_preprocessor(
      completed[[index]],
      spec,
      full_preprocessor,
      imputation = index
    )
  })
}

.workflow_feature_lists <- function(selections) {
  lapply(selections, function(selection) as.character(selection$selected_features))
}

# Majority vote across imputations; fall back to the union when no feature is
# selected by more than half of the imputations.
.workflow_pool_features <- function(feature_lists) {
  union_features <- unique(unlist(feature_lists, use.names = FALSE))
  if (length(union_features) == 0L) {
    return(character())
  }
  m <- length(feature_lists)
  counts <- vapply(union_features, function(feature) {
    sum(vapply(feature_lists, function(x) feature %in% x, logical(1)))
  }, integer(1))
  majority <- union_features[counts > m / 2]
  if (length(majority) == 0L) {
    union_features
  } else {
    majority
  }
}

.workflow_pool_selection <- function(selections) {
  pooled <- selections[[1L]]
  pooled$selected_features <- .workflow_pool_features(.workflow_feature_lists(selections))
  pooled
}

.workflow_param_signature <- function(params) {
  if (length(params) == 0L) {
    return("")
  }
  keys <- sort(names(params))
  paste(
    vapply(keys, function(key) {
      paste0(key, "=", paste(format(params[[key]]), collapse = ","))
    }, character(1)),
    collapse = ";"
  )
}

# Most frequent best-parameter set across imputations (first wins on ties).
.workflow_pool_params <- function(params_list) {
  signatures <- vapply(params_list, .workflow_param_signature, character(1))
  counts <- table(signatures)
  best <- names(counts)[which.max(counts)]
  params_list[[which(signatures == best)[1L]]]
}

.workflow_pool_metric_rows <- function(rows) {
  combined <- do.call(rbind, rows)
  out <- combined[1L, , drop = FALSE]
  numeric_cols <- names(combined)[vapply(combined, is.numeric, logical(1))]
  for (name in numeric_cols) {
    value <- mean(combined[[name]], na.rm = TRUE)
    out[[name]] <- if (is.nan(value)) NA_real_ else value
  }
  rownames(out) <- NULL
  out
}

# Pooled probability across imputations (mean), matching one row per sample.
.workflow_mean_prob <- function(probabilities) {
  if (length(probabilities) == 1L) {
    return(as.numeric(probabilities[[1L]]))
  }
  Reduce(`+`, lapply(probabilities, as.numeric)) / length(probabilities)
}

# Strict Rubin's rules on per-imputation bootstrap intervals: the pooled mean
# is the average estimate, the total variance combines the mean within-
# imputation variance (bootstrap SE squared) with the between-imputation
# variance, and the interval uses the Barnard-Rubin degrees of freedom.
.workflow_pool_ci <- function(intervals, model, set, level = 0.95,
                              probability = NULL) {
  intervals <- intervals[!vapply(intervals, is.null, logical(1))]
  if (length(intervals) == 0L) {
    return(NULL)
  }
  metric_names <- unique(unlist(lapply(intervals, function(x) x$metric)))
  rows <- list()
  for (metric in metric_names) {
    pick <- function(field) {
      vapply(intervals, function(interval) {
        values <- interval[[field]][interval$metric == metric]
        if (length(values) == 0L) NA_real_ else as.numeric(values[1L])
      }, numeric(1))
    }
    estimate <- pick("mean")
    se <- pick("se")
    n_effective <- vapply(intervals, function(interval) {
      values <- interval$n_effective[interval$metric == metric]
      if (length(values) == 0L) NA_integer_ else as.integer(values[1L])
    }, integer(1))

    complete <- is.finite(estimate) & is.finite(se)
    pooled <- if (sum(complete) > 0L) {
      tryCatch(
        pool_mi(estimate[complete], se[complete]^2, level = level),
        error = function(e) NULL
      )
    } else {
      NULL
    }
    if (is.null(pooled)) {
      rows[[length(rows) + 1L]] <- data.frame(
        metric = metric,
        mean = NA_real_,
        se = NA_real_,
        lower = NA_real_,
        upper = NA_real_,
        df = NA_real_,
        n_effective = NA_integer_,
        formatted = "NA",
        stringsAsFactors = FALSE,
        row.names = NULL
      )
      next
    }
    available_n <- n_effective[!is.na(n_effective)]
    effective <- if (length(available_n) > 0L) min(available_n) else NA_integer_
    interval <- .clamp_metric_interval(metric, pooled$lower, pooled$upper)
    rows[[length(rows) + 1L]] <- data.frame(
      metric = metric,
      mean = pooled$mean,
      se = pooled$se,
      lower = interval[[1L]],
      upper = interval[[2L]],
      df = pooled$df,
      n_effective = as.integer(effective),
      formatted = sprintf("%.3f (%.3f-%.3f)", pooled$mean, interval[[1L]], interval[[2L]]),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }
  out <- do.call(rbind, rows)
  out$model <- model
  out$set <- set
  if (!is.null(probability)) {
    out$probability <- probability
  }
  out[, c(
    "model",
    "set",
    if (!is.null(probability)) "probability",
    setdiff(names(out), c("model", "set", "probability"))
  ), drop = FALSE]
}

.workflow_in_cv <- function(spec, step) {
  step %in% spec$in_cv
}

# glmnet requires at least two predictors. When feature selection leaves a
# single feature, switch glmnet learners to a logistic regression fit and record
# the downgrade instead of silently dropping the model.
.workflow_guard_single_feature <- function(learners, task, failures = NULL) {
  if (is.null(task) || is.null(task$n_features) || task$n_features >= 2L) {
    return(learners)
  }
  for (key in names(learners)) {
    learner <- learners[[key]]$learner
    if (is.null(learner) ||
        !learner$id %in% c("classif.cv_glmnet", "classif.glmnet")) {
      next
    }
    learners[[key]]$learner <- mlr3::lrn("classif.log_reg", predict_type = "prob")
    learners[[key]]$search_space <- NULL
    learners[[key]]$tune <- FALSE
    learners[[key]]$params <- list()
    learners[[key]]$fallback <- "classif.log_reg"
    .record_failure(
      failures,
      key,
      "fallback",
      "glmnet requires at least two features; classif.log_reg was used instead."
    )
  }
  learners
}

.workflow_preprocessing_in_cv <- function(spec) {
  any(vapply(
    c("imputation", "scaling", "encoding", "balance"),
    function(step) .workflow_in_cv(spec, step),
    logical(1)
  ))
}

.workflow_select_features <- function(data, spec) {
  tryCatch(select_features(data, spec), error = function(e) e)
}

.workflow_fold_preprocessor <- function(fold_train, spec, full_preprocessor) {
  current <- fold_train
  steps <- list()

  impute <- if (.workflow_in_cv(spec, "imputation")) {
    fit_imputer(
      current,
      method = full_preprocessor$steps$impute$method,
      outcome = spec$outcome,
      ordered_as = spec$ordered_as,
      m = spec$preprocess$m,
      seed = spec$seed,
      verbose = spec$verbose
    )
  } else {
    full_preprocessor$steps$impute
  }
  current <- apply_imputer(impute, current)
  steps$impute <- impute

  scale <- if (.workflow_in_cv(spec, "scaling")) {
    fit_scale(
      current,
      method = full_preprocessor$steps$scale$method,
      outcome = spec$outcome,
      ordered_as = spec$ordered_as,
      verbose = spec$verbose
    )
  } else {
    full_preprocessor$steps$scale
  }
  current <- apply_scale(scale, current)
  steps$scale <- scale

  encode <- if (.workflow_in_cv(spec, "encoding")) {
    fit_encode(
      current,
      method = full_preprocessor$steps$encode$method,
      outcome = spec$outcome,
      ordered_as = spec$ordered_as,
      verbose = spec$verbose
    )
  } else {
    full_preprocessor$steps$encode
  }
  current <- apply_encode(encode, current)
  steps$encode <- encode

  balance <- if (.workflow_in_cv(spec, "balance")) {
    params <- full_preprocessor$steps$balance$params
    fit_balance(
      current,
      method = full_preprocessor$steps$balance$method,
      outcome = spec$outcome,
      p = params$p,
      n_neighbors = params$n_neighbors,
      dup_size = params$dup_size,
      seed = spec$seed,
      verbose = spec$verbose
    )
  } else {
    full_preprocessor$steps$balance
  }
  if (.workflow_in_cv(spec, "balance")) {
    current <- balance$data
  }
  steps$balance <- balance

  preprocessor <- .new_preprocessor(steps)
  preprocessor$var_info <- if (is.null(full_preprocessor$var_info)) {
    detect_var_types(
      fold_train,
      outcome = spec$outcome,
      ordered_as = spec$ordered_as
    )
  } else {
    full_preprocessor$var_info
  }
  preprocessor$train <- current
  preprocessor
}

.workflow_prepare_fold <- function(fold_train, fold_test, spec,
                                   full_preprocessor, full_selection,
                                   preprocessed = FALSE) {
  if (isTRUE(preprocessed)) {
    # The data are already on one completed imputation (imputed, scaled, and
    # encoded outside the CV loop), so folds are split in that space.
    train <- fold_train
    test <- fold_test
    fold_preprocessor <- full_preprocessor
  } else {
    preprocessing_in_cv <- .workflow_preprocessing_in_cv(spec)
    fold_preprocessor <- if (preprocessing_in_cv) {
      .workflow_fold_preprocessor(fold_train, spec, full_preprocessor)
    } else {
      full_preprocessor
    }
    train <- if (preprocessing_in_cv) {
      fold_preprocessor$train
    } else {
      apply_preprocessor(fold_preprocessor, fold_train)
    }
    test <- apply_preprocessor(fold_preprocessor, fold_test)
  }

  if (.workflow_in_cv(spec, "selection")) {
    fold_selection <- .workflow_select_features(train, spec)
    if (inherits(fold_selection, "error")) {
      stop(fold_selection)
    }
  } else {
    fold_selection <- full_selection
  }
  features <- as.character(fold_selection$selected_features)
  features <- intersect(features, names(train))
  if (length(features) == 0L) {
    features <- intersect(as.character(full_selection$selected_features), names(train))
  }
  if (length(features) == 0L) {
    .abort("No selected features are available for the current fold.")
  }
  list(
    train = train[, c(features, spec$outcome), drop = FALSE],
    test = test[, c(features, spec$outcome), drop = FALSE],
    features = features,
    preprocessor = fold_preprocessor,
    selection = fold_selection
  )
}

.workflow_probabilities <- function(learner, task, positive) {
  prediction <- learner$predict(task)
  probabilities <- prediction$prob
  if (is.null(probabilities) || ncol(probabilities) == 0L) {
    .abort("The learner did not return class probabilities.")
  }
  column <- if (positive %in% colnames(probabilities)) {
    positive
  } else if (ncol(probabilities) == 1L) {
    colnames(probabilities)[1L]
  } else {
    .abort(sprintf(
      "The learner returned no probability column for the positive class '%s'.",
      as.character(positive)
    ))
  }
  list(prediction = prediction, prob = as.numeric(probabilities[, column]))
}

.workflow_metric_names <- function() {
  .ci_metric_names()
}

## Failure log ----------------------------------------------------------------

.new_failure_log <- function() {
  log <- new.env(parent = emptyenv())
  log$rows <- list()
  log
}

.record_failure <- function(log, model, stage, message) {
  if (is.null(log)) {
    return(invisible(FALSE))
  }
  log$rows[[length(log$rows) + 1L]] <- data.frame(
    model = as.character(model),
    stage = as.character(stage),
    message = as.character(message),
    stringsAsFactors = FALSE
  )
  invisible(TRUE)
}

.failure_table <- function(log) {
  if (is.null(log) || length(log$rows) == 0L) {
    return(data.frame(
      model = character(),
      stage = character(),
      message = character(),
      stringsAsFactors = FALSE
    ))
  }
  out <- do.call(rbind, log$rows)
  out <- unique(out)
  rownames(out) <- NULL
  out
}

## Resampling helpers ---------------------------------------------------------

.workflow_tuning_resampling <- function(spec) {
  folds <- max(2L, as.integer(spec$tuning$folds))
  repeats <- max(1L, as.integer(spec$tuning$repeats))
  if (repeats > 1L) {
    return(mlr3::rsmp("repeated_cv", folds = folds, repeats = repeats))
  }
  mlr3::rsmp("cv", folds = folds)
}

.workflow_tuning_terminator <- function(spec) {
  if (identical(spec$tuning$terminator, "seconds")) {
    return(mlr3tuning::trm(
      "run_time",
      secs = as.numeric(spec$tuning$seconds)
    ))
  }
  mlr3tuning::trm("evals", n_evals = as.integer(spec$tuning$evals))
}

# Out-of-fold probabilities for one fitted learner. The calibrator and the
# classification threshold are fitted on these values instead of the in-sample
# training predictions, which would overstate confidence.
.workflow_oof_probabilities <- function(state, spec, data, folds, seed, positive,
                                        failures = NULL) {
  fold_task <- mlr3::as_task_classif(
    data,
    target = spec$outcome,
    positive = as.character(spec$positive)
  )
  folds <- max(2L, min(as.integer(folds), fold_task$nrow))
  resampling <- mlr3::rsmp("cv", folds = folds)
  set.seed(as.integer(seed))
  resampling$instantiate(fold_task)
  prob <- rep(NA_real_, fold_task$nrow)
  for (i in seq_len(resampling$iters)) {
    train_index <- resampling$train_set(i)
    test_index <- resampling$test_set(i)
    if (length(test_index) == 0L) {
      next
    }
    fold_train <- data[train_index, , drop = FALSE]
    fold_test <- data[test_index, , drop = FALSE]
    prepared <- tryCatch(
      .workflow_prepare_fold(
        fold_train,
        fold_test,
        spec,
        full_preprocessor = state$preprocessor,
        full_selection = state$selection
      ),
      error = function(e) e
    )
    if (inherits(prepared, "error")) {
      .record_failure(failures, state$model_key, "oof_prepare",
                      conditionMessage(prepared))
      next
    }
    fold_learner <- tryCatch(
      train_model(
        make_task(prepared$train, spec),
        state$tuned$learner,
        params = state$tuned$best_params
      ),
      error = function(e) e
    )
    if (inherits(fold_learner, "error")) {
      .record_failure(failures, state$model_key, "oof_training",
                      conditionMessage(fold_learner))
      next
    }
    predicted <- tryCatch(
      .workflow_probabilities(
        fold_learner,
        make_task(prepared$test, spec),
        positive
      ),
      error = function(e) e
    )
    if (inherits(predicted, "error")) {
      .record_failure(failures, state$model_key, "oof_predict",
                      conditionMessage(predicted))
      next
    }
    prob[test_index] <- predicted$prob
  }
  prob
}

# Pool the out-of-fold probabilities across imputations. Returns the complete
# cases plus the matching 0/1 event vector.
.workflow_oof_event <- function(run_states, spec, data, failures = NULL) {
  folds <- 5L
  settings <- spec$evaluation$calibration_args
  if (!is.null(settings) && !is.null(settings$folds)) {
    folds <- as.integer(settings$folds)
  }
  values <- lapply(run_states, function(state) {
    .workflow_oof_probabilities(
      state,
      spec,
      data = data,
      folds = folds,
      seed = spec$seed,
      positive = spec$positive,
      failures = failures
    )
  })
  probabilities <- .workflow_mean_prob(values)
  truth <- as.character(data[[spec$outcome]])
  event <- as.integer(truth == as.character(spec$positive))
  keep <- !is.na(probabilities) & !is.na(event)
  list(
    all = probabilities,
    prob = probabilities[keep],
    event = event[keep],
    n = sum(keep)
  )
}

.workflow_metric_row <- function(probs, truth, threshold, spec, model, set,
                                 probability) {
  values <- .calc_binary_metrics(
    probs = probs,
    truth = truth,
    threshold = threshold,
    positive = spec$positive,
    metrics = .workflow_metric_names()
  )
  row <- data.frame(
    model = model,
    set = set,
    probability = probability,
    stringsAsFactors = FALSE
  )
  for (name in names(values)) {
    row[[name]] <- as.numeric(values[[name]])
  }
  row
}

.workflow_metrics <- function(probs, truth, threshold, spec, model, set,
                              probability = "raw") {
  .workflow_metric_row(
    probs,
    truth,
    threshold,
    spec,
    model,
    set,
    probability
  )
}

.workflow_threshold <- function(probs, truth, spec) {
  tryCatch(
    find_threshold(
      probs,
      truth,
      method = spec$threshold_method,
      cost = spec$cost,
      fixed_threshold = if (is.null(spec$fixed_threshold)) 0.5 else spec$fixed_threshold,
      positive = spec$positive
    ),
    error = function(e) NA_real_
  )
}

# Threshold from an already-cleaned 0/1 event vector (out-of-fold source).
.workflow_threshold_event <- function(probs, event, spec) {
  tryCatch(
    find_threshold(
      probs,
      event,
      method = spec$threshold_method,
      cost = spec$cost,
      fixed_threshold = if (is.null(spec$fixed_threshold)) 0.5 else spec$fixed_threshold,
      positive = 1
    ),
    error = function(e) NA_real_
  )
}

.workflow_calibrator <- function(probs, truth, spec) {
  method <- spec$evaluation$calibration
  if (is.null(method) || identical(method, "none")) {
    return(list(
      requested = FALSE,
      failed = FALSE,
      fit = NULL,
      message = NULL
    ))
  }
  fit <- tryCatch(
    .fit_calibrator(probs, truth, method),
    error = function(e) e
  )
  if (inherits(fit, "error")) {
    return(list(
      requested = TRUE,
      failed = TRUE,
      fit = NULL,
      message = conditionMessage(fit)
    ))
  }
  if (is.null(fit)) {
    return(list(
      requested = TRUE,
      failed = TRUE,
      fit = NULL,
      message = "The calibration model could not be fitted."
    ))
  }
  list(
    requested = TRUE,
    failed = FALSE,
    fit = fit,
    message = NULL
  )
}

.workflow_calibrated <- function(calibrator, probs) {
  if (is.null(calibrator)) {
    return(list(
      prob = rep(NA_real_, length(probs)),
      failed = TRUE,
      message = "No fitted calibrator is available."
    ))
  }
  out <- tryCatch(
    .predict_calibrator(calibrator, probs),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    return(list(
      prob = rep(NA_real_, length(probs)),
      failed = TRUE,
      message = conditionMessage(out)
    ))
  }
  out <- as.numeric(out)
  invalid <- length(out) != length(probs) ||
    any(!is.finite(out[!is.na(probs)])) ||
    any(is.na(out) & !is.na(probs)) ||
    !any(!is.na(out))
  if (invalid) {
    return(list(
      prob = rep(NA_real_, length(probs)),
      failed = TRUE,
      message = "Calibration returned invalid probabilities."
    ))
  }
  list(prob = out, failed = FALSE, message = NULL)
}

.workflow_entries_for_set <- function(prediction_entries, set) {
  keep <- vapply(
    prediction_entries,
    function(entry) identical(as.character(entry$set), as.character(set)),
    logical(1)
  )
  prediction_entries[keep]
}

.workflow_comparison_set <- function(prediction_entries, select_on = "validation") {
  sets <- unique(vapply(
    prediction_entries,
    function(entry) as.character(entry$set),
    character(1)
  ))
  if (identical(select_on, "auto")) {
    # `auto` keeps the internal validation set: the test and external sets are
    # reporting-only and never take part in model selection.
    select_on <- "validation"
  }
  if (identical(select_on, "external")) {
    .abort("`select_on` cannot be \"external\"; the external set is reporting-only.")
  }
  if (!select_on %in% sets) {
    .abort(sprintf(
      "The comparison set \"%s\" is not present in the workflow predictions.",
      select_on
    ))
  }
  if (identical(select_on, "test")) {
    warning(
      paste0(
        "The test set is used for model selection; its performance is ",
        "optimistic. Use `select_on = \"validation\"` to keep the test set ",
        "for a held-out report."
      ),
      call. = FALSE
    )
  }
  select_on
}

.workflow_calibration_table <- function(prediction_entries, spec, bins = 10L) {
  rows <- list()
  for (entry in prediction_entries) {
    before <- .binary_calibration_metrics(
      entry$prob,
      entry$truth,
      bins = bins,
      positive = spec$positive
    )
    rows[[length(rows) + 1L]] <- data.frame(
      model = entry$model,
      set = entry$set,
      stage = "before",
      before,
      stringsAsFactors = FALSE
    )
    if (isTRUE(entry$calibration_success)) {
      after <- .binary_calibration_metrics(
        entry$calibrated_prob,
        entry$truth,
        bins = bins,
        positive = spec$positive
      )
      rows[[length(rows) + 1L]] <- data.frame(
        model = entry$model,
        set = entry$set,
        stage = "after",
        after,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0L) {
    return(NULL)
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

.workflow_calibration_result <- function(prediction_entries, models, spec) {
  method <- spec$evaluation$calibration
  if (is.null(method) || identical(method, "none")) {
    return(NULL)
  }
  failures <- lapply(models, function(model) {
    list(
      requested = isTRUE(model$calibration_requested),
      failed = isTRUE(model$calibration_failed),
      message = model$calibration_message
    )
  })
  list(
    method = method,
    parameters = lapply(models, function(model) model$calibrator),
    metrics = .workflow_calibration_table(prediction_entries, spec),
    failures = failures
  )
}

.workflow_test_results <- function(prediction_entries, spec, comparison_set) {
  requested <- as.character(spec$evaluation$tests)
  if (length(requested) == 0L || is.null(comparison_set)) {
    return(NULL)
  }
  selected <- .workflow_entries_for_set(prediction_entries, comparison_set)
  if (length(selected) == 0L) {
    return(NULL)
  }
  model_names <- vapply(selected, `[[`, character(1), "model")
  probs <- lapply(selected, `[[`, "prob")
  calibrated_probs <- lapply(selected, `[[`, "calibrated_prob")
  names(probs) <- model_names
  names(calibrated_probs) <- model_names
  truth <- selected[[1L]]$truth
  tests <- list()
  if ("delong" %in% requested) {
    tests$delong <- tryCatch(
      delong_test(probs, truth, positive = spec$positive),
      error = function(e) NULL
    )
  }
  if ("hl" %in% requested) {
    tests$hl <- list(
      raw = lapply(probs, function(prob) {
        tryCatch(
          hl_test(prob, truth, positive = spec$positive),
          error = function(e) NULL
        )
      }),
      calibrated = lapply(calibrated_probs, function(prob) {
        if (length(prob) == 0L || all(is.na(prob))) {
          return(NULL)
        }
        tryCatch(
          hl_test(prob, truth, positive = spec$positive),
          error = function(e) NULL
        )
      })
    )
  }
  if (length(tests) == 0L) {
    return(NULL)
  }
  tests
}

## NRI / IDI ------------------------------------------------------------------

# Continuous NRI and IDI for one model against a reference, computed from the
# same evaluation set. Kept separate from `calc_nri_idi()` so the bootstrap can
# reuse the arithmetic without re-validating on every resample.
.nri_idi_core <- function(new_prob, ref_prob, event) {
  event_up <- mean(new_prob[event] > ref_prob[event])
  event_down <- mean(new_prob[event] < ref_prob[event])
  non_event_down <- mean(new_prob[!event] < ref_prob[!event])
  non_event_up <- mean(new_prob[!event] > ref_prob[!event])
  list(
    nri = (event_up - event_down) + (non_event_down - non_event_up),
    idi = (mean(new_prob[event]) - mean(new_prob[!event])) -
      (mean(ref_prob[event]) - mean(ref_prob[!event]))
  )
}

.workflow_bootstrap_nri_idi <- function(new_prob, ref_prob, event, R, level, seed) {
  R <- as.integer(R)
  n <- length(event)
  empty <- matrix(
    c(NA_real_, NA_real_, NA_real_, NA_real_),
    nrow = 2L,
    dimnames = list(c("nri", "idi"), c("lower", "upper"))
  )
  if (R < 1L || n < 2L) {
    return(empty)
  }
  set.seed(as.integer(seed))
  values <- matrix(NA_real_, nrow = R, ncol = 2L)
  for (r in seq_len(R)) {
    index <- sample.int(n, n, replace = TRUE)
    resampled_event <- event[index]
    if (length(unique(resampled_event)) < 2L) {
      next
    }
    estimate <- .nri_idi_core(
      new_prob[index],
      ref_prob[index],
      resampled_event
    )
    values[r, ] <- c(estimate$nri, estimate$idi)
  }
  alpha <- (1 - level) / 2
  out <- sapply(seq_len(2L), function(column) {
    column_values <- values[, column]
    if (all(is.na(column_values))) {
      return(c(lower = NA_real_, upper = NA_real_))
    }
    c(
      lower = stats::quantile(column_values, probs = alpha, na.rm = TRUE,
                              names = FALSE),
      upper = stats::quantile(column_values, probs = 1 - alpha, na.rm = TRUE,
                              names = FALSE)
    )
  })
  dimnames(out) <- list(c("lower", "upper"), c("nri", "idi"))
  t(out)
}

.workflow_model_threshold <- function(models, key) {
  value <- models[[key]]$thresholds$raw
  if (is.null(value) || length(value) != 1L || !is.finite(value)) {
    return(0.5)
  }
  as.numeric(value)
}

.workflow_reference_model <- function(metrics, models, spec) {
  keys <- names(models)
  requested <- spec$evaluation$reference_model
  if (is.null(requested)) {
    requested <- spec$reference_model
  }
  if (!is.null(requested)) {
    if (!requested %in% keys) {
      .abort(sprintf(
        "Reference model \"%s\" is not among the fitted models (%s).",
        requested,
        paste(keys, collapse = ", ")
      ))
    }
    return(as.character(requested))
  }
  if (is.data.frame(metrics) && nrow(metrics) > 0L &&
      all(c("set", "probability", "auc", "model") %in% names(metrics))) {
    train <- metrics[
      metrics$set == "train" & metrics$probability == "raw" &
        is.finite(metrics$auc),
      ,
      drop = FALSE
    ]
    if (nrow(train) > 0L) {
      return(as.character(train$model[which.max(train$auc)]))
    }
  }
  keys[1L]
}

.workflow_nri_idi <- function(prediction_entries, models, metrics, spec,
                              comparison_set, ci_R = 1000L) {
  if (is.null(comparison_set) || length(models) < 2L) {
    return(NULL)
  }
  selected <- .workflow_entries_for_set(prediction_entries, comparison_set)
  if (length(selected) < 2L) {
    return(NULL)
  }
  names(selected) <- vapply(selected, `[[`, character(1), "model")
  reference <- .workflow_reference_model(metrics, models, spec)
  reference_entry <- selected[[reference]]
  truth <- as.character(reference_entry$truth)
  event <- as.integer(truth == as.character(spec$positive))
  keep <- !is.na(event)
  event <- event[keep]
  if (length(event) < 10L || length(unique(event)) < 2L) {
    return(NULL)
  }
  R <- if (identical(spec$evaluation$ci, "bootstrap")) as.integer(ci_R) else 0L
  rows <- list()
  for (index in seq_along(selected)) {
    name <- names(selected)[[index]]
    entry <- selected[[index]]
    new_prob <- as.numeric(entry$prob)[keep]
    ref_prob <- as.numeric(reference_entry$prob)[keep]
    complete <- !is.na(new_prob) & !is.na(ref_prob)
    if (sum(complete) < 10L) {
      next
    }
    new_prob <- new_prob[complete]
    ref_prob <- ref_prob[complete]
    event_complete <- event[complete]
    point <- tryCatch(
      calc_nri_idi(
        new_prob,
        ref_prob,
        event_complete,
        method = "continuous",
        positive = 1
      ),
      error = function(e) NULL
    )
    if (is.null(point)) {
      next
    }
    categorical <- tryCatch(
      calc_nri_idi(
        new_prob,
        ref_prob,
        event_complete,
        method = "categorical",
        threshold_new = .workflow_model_threshold(models, name),
        threshold_reference = .workflow_model_threshold(models, reference),
        positive = 1
      ),
      error = function(e) NULL
    )
    interval <- .workflow_bootstrap_nri_idi(
      new_prob,
      ref_prob,
      event_complete,
      R = R,
      level = 0.95,
      seed = as.integer(spec$seed) + index - 1L
    )
    rows[[length(rows) + 1L]] <- data.frame(
      model = name,
      reference = reference,
      set = comparison_set,
      probability = "raw",
      nri = point$nri,
      nri_lower = interval["nri", "lower"],
      nri_upper = interval["nri", "upper"],
      nri_events = point$nri_events,
      nri_nonevents = point$nri_nonevents,
      nri_categorical = if (is.null(categorical)) NA_real_ else categorical$nri,
      idi = point$idi,
      idi_lower = interval["idi", "lower"],
      idi_upper = interval["idi", "upper"],
      new_slope = point$new_discrimination_slope,
      reference_slope = point$reference_discrimination_slope,
      n = point$n,
      n_events = point$n_events,
      n_nonevents = point$n_nonevents,
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0L) {
    return(NULL)
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

.workflow_dca_result <- function(prediction_entries, spec, comparison_set) {
  if (!isTRUE(spec$evaluation$dca) || is.null(comparison_set)) {
    return(NULL)
  }
  selected <- .workflow_entries_for_set(prediction_entries, comparison_set)
  if (length(selected) == 0L) {
    return(NULL)
  }
  rows <- list()
  for (entry in selected) {
    rows[[length(rows) + 1L]] <- dca(
      entry$prob,
      entry$truth,
      positive = spec$positive,
      model = entry$model,
      calibrated = FALSE
    )
    if (isTRUE(entry$calibration_success)) {
      rows[[length(rows) + 1L]] <- dca(
        entry$calibrated_prob,
        entry$truth,
        positive = spec$positive,
        model = entry$model,
        calibrated = TRUE
      )
    }
  }
  if (length(rows) == 0L) {
    return(NULL)
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  class(out) <- c("uniml_dca", "data.frame")
  out
}

.workflow_prediction_table <- function(entries) {
  frames <- list()
  for (name in names(entries)) {
    entry <- entries[[name]]
    frame <- data.frame(
      # Entry keys are "<model>:<set>"; the table keeps the model and the set in
      # their own columns so that the plotting layer can filter on both.
      model = if (is.null(entry$model)) name else as.character(entry$model),
      prob = as.numeric(entry$prob),
      calibrated_prob = as.numeric(entry$calibrated_prob),
      truth = as.character(entry$truth),
      positive = as.character(entry$positive),
      set = as.character(entry$set),
      stringsAsFactors = FALSE
    )
    frames[[name]] <- frame
  }
  if (length(frames) == 0L) {
    return(data.frame(
      model = character(), prob = numeric(), calibrated_prob = numeric(),
      truth = character(), positive = character(), set = character(),
      stringsAsFactors = FALSE
    ))
  }
  out <- do.call(rbind, frames)
  rownames(out) <- NULL
  out
}

.workflow_folds <- function(data, spec) {
  folds <- max(2L, as.integer(spec$tuning$folds))
  repeats <- max(1L, as.integer(spec$tuning$repeats))
  resampling <- if (repeats > 1L) {
    mlr3::rsmp("repeated_cv", folds = folds, repeats = repeats)
  } else {
    mlr3::rsmp("cv", folds = folds)
  }
  set.seed(as.integer(spec$seed))
  resampling$instantiate(mlr3::as_task_classif(
    data,
    target = spec$outcome,
    positive = spec$positive
  ))
  resampling
}

.workflow_evaluate_fold <- function(data, spec, split, positive, metric_names,
                                    tuned_models, failures = NULL) {
  train_index <- split$train
  test_index <- split$test
  fold_train <- data[train_index, , drop = FALSE]
  fold_test <- data[test_index, , drop = FALSE]
  if (length(unique(fold_train[[spec$outcome]])) < 2L) {
    return(NULL)
  }
  prepared <- .workflow_prepare_fold(
    fold_train,
    fold_test,
    spec,
    full_preprocessor = split$preprocessor,
    full_selection = split$selection,
    preprocessed = isTRUE(split$preprocessed)
  )
  task <- make_task(prepared$train, spec)
  learners <- resolve_learners(spec, task = task, n_train = task$nrow)
  learners <- .workflow_guard_single_feature(learners, task, failures)
  result <- list()
  for (key in names(learners)) {
    learner_spec <- learners[[key]]
    tuning_in_cv <- .workflow_in_cv(spec, "tuning")
    tuned <- if (tuning_in_cv) {
      tryCatch(
        tune_model(
          task,
          learner_spec,
          resampling = mlr3::rsmp("holdout", ratio = split$inner_ratio),
          tuner = spec$tuning$tuner,
          terminator = .workflow_tuning_terminator(spec),
          nested = spec$tuning$nested,
          seed = spec$seed
        ),
        error = function(e) e
      )
    } else {
      tuned_models[[key]]
    }
    if (is.null(tuned)) {
      .record_failure(failures, key, sprintf("cv_tuning_%s", split$role),
                      "No tuned model was available for this fold.")
      next
    }
    if (inherits(tuned, "error") || identical(tuned$status, "failed")) {
      .record_failure(
        failures,
        key,
        sprintf("cv_tuning_%s", split$role),
        if (inherits(tuned, "error")) conditionMessage(tuned) else "Tuning failed."
      )
      next
    }
    base_learner <- if (tuning_in_cv) tuned$learner else tuned$learner
    learner <- tryCatch(
      train_model(task, base_learner, params = tuned$best_params),
      error = function(e) e
    )
    if (inherits(learner, "error")) {
      .record_failure(failures, key, sprintf("cv_training_%s", split$role),
                      conditionMessage(learner))
      next
    }
    train_predicted <- tryCatch(
      .workflow_probabilities(learner, make_task(prepared$train, spec), positive),
      error = function(e) e
    )
    if (inherits(train_predicted, "error")) {
      .record_failure(failures, key, sprintf("cv_train_predict_%s", split$role),
                      conditionMessage(train_predicted))
      next
    }
    truth <- as.character(prepared$test[[spec$outcome]])
    threshold <- .workflow_threshold(
      train_predicted$prob,
      as.character(prepared$train[[spec$outcome]]),
      spec
    )
    predicted <- tryCatch(
      .workflow_probabilities(learner, make_task(prepared$test, spec), positive),
      error = function(e) e
    )
    if (inherits(predicted, "error")) {
      .record_failure(failures, key, sprintf("cv_test_predict_%s", split$role),
                      conditionMessage(predicted))
      next
    }
    values <- .calc_binary_metrics(
      probs = predicted$prob,
      truth = truth,
      threshold = threshold,
      positive = spec$positive,
      metrics = metric_names
    )
    row <- data.frame(
      model = key,
      repetition = as.integer(split$repetition),
      fold = as.integer(split$fold),
      stringsAsFactors = FALSE
    )
    for (name in names(values)) {
      row[[name]] <- as.numeric(values[[name]])
    }
    result[[key]] <- row
  }
  result
}

.workflow_cv <- function(data, spec, full_preprocessor, full_selection,
                         tuned_models, failures = NULL, preprocessed = FALSE) {
  resampling <- .workflow_folds(data, spec)
  metric_names <- .workflow_metric_names()
  rows <- list()
  inner_ratio <- max(0.5, min(0.8, 1 - 1 / max(2L, as.integer(spec$tuning$folds))))
  instance <- as.data.frame(resampling$instance)
  key_columns <- intersect(c("rep", "fold"), names(instance))
  keys <- unique(instance[, key_columns, drop = FALSE])
  n_iterations <- resampling$iters
  for (i in seq_len(n_iterations)) {
    repetition <- if ("rep" %in% names(keys)) keys$rep[[i]] else 1L
    fold <- if ("fold" %in% names(keys)) keys$fold[[i]] else i
    split <- list(
      train = resampling$train_set(i),
      test = resampling$test_set(i),
      role = sprintf("rep%d_fold%d", repetition, fold),
      repetition = repetition,
      fold = fold,
      inner_ratio = inner_ratio,
      preprocessor = full_preprocessor,
      selection = full_selection,
      preprocessed = preprocessed
    )
    evaluated <- tryCatch(
      .workflow_evaluate_fold(
        data,
        spec,
        split,
        spec$positive,
        metric_names,
        tuned_models,
        failures = failures
      ),
      error = function(e) e
    )
    if (inherits(evaluated, "error")) {
      .record_failure(
        failures,
        model = "*",
        stage = sprintf("cv_%s", split$role),
        message = conditionMessage(evaluated)
      )
      next
    }
    if (is.null(evaluated) || length(evaluated) == 0L) {
      .record_failure(
        failures,
        model = "*",
        stage = sprintf("cv_%s", split$role),
        message = "No model could be evaluated in this fold."
      )
      next
    }
    for (key in names(evaluated)) {
      rows[[length(rows) + 1L]] <- evaluated[[key]]
    }
  }
  fold_metrics <- if (length(rows) == 0L) {
    data.frame(
      model = character(), repetition = integer(), fold = integer(),
      stringsAsFactors = FALSE
    )
  } else {
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    out
  }
  summary <- if (nrow(fold_metrics) == 0L) {
    data.frame(
      model = character(), metric = character(), mean = numeric(),
      sd = numeric(), stringsAsFactors = FALSE
    )
  } else {
    .workflow_cv_summary(fold_metrics, metric_names)
  }
  list(
    resampling = resampling,
    metrics = fold_metrics,
    detailed = fold_metrics,
    summary = summary,
    leakage_policy = spec$leakage_policy,
    in_cv = spec$in_cv
  )
}

# Run the cross-validation machinery once per completed imputation and pool the
# per-imputation performance estimates with strict Rubin's rules.
.workflow_cv_multi <- function(data, spec, imputation_preprocessors, selections,
                               tuned_models, failures = NULL) {
  empty_detailed <- data.frame(
    model = character(), repetition = integer(), fold = integer(),
    imputation = integer(),
    stringsAsFactors = FALSE
  )
  # When imputation, scaling, and encoding sit outside the CV loop (the default
  # for multi-imputation) the folds are split on the completed, processed data,
  # so each imputation contributes genuinely different fold data.
  use_completed <- identical(spec$preprocess$balance, "none") &&
    !any(vapply(
      c("imputation", "scaling", "encoding"),
      function(step) .workflow_in_cv(spec, step),
      logical(1)
    ))
  frames <- vector("list", length(imputation_preprocessors))
  resampling <- NULL
  for (index in seq_along(imputation_preprocessors)) {
    preprocessor <- imputation_preprocessors[[index]]
    evaluated <- .workflow_cv(
      if (isTRUE(use_completed)) preprocessor$train else data,
      spec,
      preprocessor,
      selections[[index]],
      tuned_models,
      failures,
      preprocessed = isTRUE(use_completed)
    )
    if (is.null(resampling)) {
      resampling <- evaluated$resampling
    }
    detailed <- evaluated$detailed
    if (nrow(detailed) > 0L) {
      detailed$imputation <- as.integer(index)
      frames[[index]] <- detailed
    }
  }
  frames <- frames[!vapply(frames, is.null, logical(1))]
  detailed <- if (length(frames) == 0L) {
    empty_detailed
  } else {
    out <- do.call(rbind, frames)
    rownames(out) <- NULL
    out
  }
  summary <- if (nrow(detailed) == 0L) {
    data.frame(
      model = character(), metric = character(), mean = numeric(),
      se = numeric(), lower = numeric(), upper = numeric(), df = numeric(),
      sd = numeric(), median = numeric(), n = integer(),
      stringsAsFactors = FALSE
    )
  } else {
    .workflow_cv_summary(detailed, .workflow_metric_names())
  }
  list(
    resampling = resampling,
    metrics = detailed,
    detailed = detailed,
    summary = summary,
    leakage_policy = spec$leakage_policy,
    in_cv = spec$in_cv
  )
}

.workflow_cv_summary <- function(fold_metrics, metric_names) {
  rows <- list()
  for (model in unique(fold_metrics$model)) {
    frame <- fold_metrics[fold_metrics$model == model, , drop = FALSE]
    for (metric in metric_names) {
      if (!metric %in% names(frame)) {
        next
      }
      pooled <- .workflow_cv_pool_metric(frame, metric)
      all_values <- as.numeric(frame[[metric]])
      all_values <- all_values[is.finite(all_values)]
      interval <- .clamp_metric_interval(metric, pooled$lower, pooled$upper)
      rows[[length(rows) + 1L]] <- data.frame(
        model = model,
        metric = metric,
        mean = pooled$mean,
        se = pooled$se,
        lower = interval[[1L]],
        upper = interval[[2L]],
        df = pooled$df,
        sd = if (length(all_values) > 1L) stats::sd(all_values) else NA_real_,
        median = if (length(all_values) > 0L) stats::median(all_values) else NA_real_,
        n = pooled$n,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0L) {
    return(data.frame(
      model = character(), metric = character(), mean = numeric(),
      se = numeric(), lower = numeric(), upper = numeric(), df = numeric(),
      sd = numeric(), median = numeric(), n = integer(),
      stringsAsFactors = FALSE
    ))
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# Per-imputation (or per-repeat) point estimates for one metric: with repeated
# cross-validation the fold values are averaged within each repeat first.
.workflow_cv_group_estimates <- function(frame, metric) {
  values <- as.numeric(frame[[metric]])
  reps <- if ("repetition" %in% names(frame)) {
    frame$repetition
  } else {
    rep(1L, nrow(frame))
  }
  if (length(unique(reps)) > 1L) {
    estimates <- vapply(
      unique(reps),
      function(rep) mean(values[reps == rep], na.rm = TRUE),
      numeric(1)
    )
    estimates[is.finite(estimates)]
  } else {
    values[is.finite(values)]
  }
}

# Pool one model's CV estimates. With more than one completed imputation the
# per-imputation means and their fold-derived variances go through strict
# Rubin's rules; a single imputation uses a plain t interval on the estimates.
.workflow_cv_pool_metric <- function(frame, metric) {
  empty <- data.frame(
    mean = NA_real_, se = NA_real_, lower = NA_real_,
    upper = NA_real_, df = NA_real_, n = 0L
  )
  if (nrow(frame) == 0L || !metric %in% names(frame)) {
    return(empty)
  }
  imputation_ids <- if ("imputation" %in% names(frame)) {
    frame$imputation
  } else {
    rep(1L, nrow(frame))
  }
  groups <- split(seq_len(nrow(frame)), imputation_ids)
  group_estimates <- lapply(groups, function(rows) {
    .workflow_cv_group_estimates(frame[rows, , drop = FALSE], metric)
  })
  usable <- vapply(group_estimates, length, numeric(1)) > 0L
  if (!any(usable)) {
    return(empty)
  }
  if (sum(usable) == 1L) {
    estimates <- group_estimates[[which(usable)]]
    k <- length(estimates)
    if (k == 1L) {
      return(data.frame(
        mean = estimates[[1L]], se = NA_real_, lower = NA_real_,
        upper = NA_real_, df = NA_real_, n = 1L
      ))
    }
    se <- stats::sd(estimates) / sqrt(k)
    df <- k - 1
    critical <- stats::qt(0.975, df)
    return(data.frame(
      mean = mean(estimates),
      se = se,
      lower = mean(estimates) - critical * se,
      upper = mean(estimates) + critical * se,
      df = df,
      n = k
    ))
  }
  means <- vapply(group_estimates[usable], mean, numeric(1))
  variances <- vapply(group_estimates[usable], function(estimates) {
    if (length(estimates) > 1L) stats::var(estimates) / length(estimates) else NA_real_
  }, numeric(1))
  complete <- is.finite(means) & is.finite(variances)
  if (sum(complete) < 2L) {
    # Not enough imputations carried both a mean and a variance.
    keep <- which(usable)[complete]
    estimates <- unlist(group_estimates[keep])
    k <- length(estimates)
    if (k == 0L) {
      return(empty)
    }
    if (k == 1L) {
      return(data.frame(
        mean = estimates[[1L]], se = NA_real_, lower = NA_real_,
        upper = NA_real_, df = NA_real_, n = 1L
      ))
    }
    se <- stats::sd(estimates) / sqrt(k)
    df <- k - 1
    critical <- stats::qt(0.975, df)
    return(data.frame(
      mean = mean(estimates), se = se,
      lower = mean(estimates) - critical * se,
      upper = mean(estimates) + critical * se,
      df = df, n = k
    ))
  }
  pooled <- pool_mi(means[complete], variances[complete], level = 0.95)
  data.frame(
    mean = pooled$mean,
    se = pooled$se,
    lower = pooled$lower,
    upper = pooled$upper,
    df = pooled$df,
    n = sum(complete)
  )
}

#' Select the comparison model
#'
#' @param result A `uniml_result`.
#' @param model `"best"` or a model key.
#'
#' @return A single model key.
#' @keywords internal
.workflow_model_key <- function(result, model) {
  .assert_scalar_character(model, "model")
  keys <- names(result$models)
  if (length(keys) == 0L) {
    .abort("`result` does not contain any fitted models.")
  }
  if (!identical(model, "best")) {
    if (!model %in% keys) {
      .abort(sprintf("Unknown model \"%s\".", model))
    }
    return(model)
  }
  ranking <- result$ranking
  if (is.data.frame(ranking) && nrow(ranking) > 0L && "rank" %in% names(ranking)) {
    best <- ranking$model[which(ranking$rank == 1L)]
    if (length(best) > 0L && best[1L] %in% keys) {
      return(as.character(best[1L]))
    }
  }
  keys[1L]
}

# Learners selected for SHAP: "best", a specific key, or "all". Multiple
# imputations are represented by the first completed-data learner.
.workflow_shap_learners <- function(result, spec) {
  mode <- spec$explain$model
  keys <- names(result$models)
  if (length(keys) == 0L) {
    return(NULL)
  }
  selected <- if (identical(mode, "all")) {
    keys
  } else {
    .workflow_model_key(result, mode)
  }
  learners <- lapply(selected, function(key) result$models[[key]]$learner)
  keep <- !vapply(learners, is.null, logical(1))
  learners <- learners[keep]
  if (length(learners) == 0L) {
    return(NULL)
  }
  stats::setNames(learners, selected[keep])
}

.workflow_shap_data <- function(result) {
  train <- result$preprocess$train
  outcome <- result$meta$spec$outcome
  as.data.frame(
    train[, setdiff(names(train), outcome), drop = FALSE],
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

# SHAP is optional: `explain$model = "none"` disables it, and any failure is
# downgraded to a warning so the workflow still returns a result.
.workflow_shap <- function(result, spec) {
  mode <- spec$explain$model
  if (is.null(mode) || identical(mode, "none")) {
    return(NULL)
  }
  learners <- .workflow_shap_learners(result, spec)
  if (is.null(learners)) {
    return(NULL)
  }
  tryCatch(
    explain_shap(
      learners,
      data = .workflow_shap_data(result),
      seed = as.integer(spec$seed),
      positive = spec$positive
    ),
    error = function(e) {
      warning(
        sprintf("SHAP computation failed and was skipped: %s", conditionMessage(e)),
        call. = FALSE
      )
      NULL
    }
  )
}

.workflow_shap_thresholds <- function(result, spec, failures = NULL) {
  settings <- spec$explain$thresholds
  if (is.null(settings) || length(settings$targets) == 0L) {
    return(NULL)
  }
  shap <- result$shap
  if (is.null(shap)) {
    .record_failure(failures, "*", "thresholds", "SHAP values are unavailable.")
    return(NULL)
  }
  available <- colnames(shap$values_matrix_raw)
  if (is.null(available)) {
    available <- colnames(shap$feature_values_raw)
  }
  available <- setdiff(available, "model")
  zero <- list()
  paper <- list()
  errors <- list()
  for (target in settings$targets) {
    if (!is.null(available) && !target %in% available) {
      message <- sprintf(
        "`target` \"%s\" is not a model feature; available features: %s.",
        target,
        paste(available, collapse = ", ")
      )
      errors[[target]] <- message
      .record_failure(failures, "*", sprintf("thresholds_%s", target), message)
      next
    }
    extracted <- tryCatch(
      extract_thresholds(
        shap,
        target,
        interaction = settings$interaction,
        min_n = if (is.null(settings$min_n)) 30L else as.integer(settings$min_n),
        min_unique = if (is.null(settings$min_unique)) {
          10L
        } else {
          as.integer(settings$min_unique)
        },
        span = if (is.null(settings$span)) 0.75 else as.numeric(settings$span)
      ),
      error = function(e) e
    )
    if (inherits(extracted, "error")) {
      errors[[target]] <- conditionMessage(extracted)
      .record_failure(
        failures,
        "*",
        sprintf("thresholds_%s", target),
        conditionMessage(extracted)
      )
      next
    }
    zero[[target]] <- extracted$zero_crossings
    paper[[target]] <- extracted$paper_ready
  }
  if (length(zero) == 0L && length(paper) == 0L) {
    return(NULL)
  }
  list(
    zero_crossings = if (length(zero) > 0L) do.call(rbind, zero) else NULL,
    paper_ready = if (length(paper) > 0L) do.call(rbind, paper) else NULL,
    targets = as.character(settings$targets),
    interaction = settings$interaction,
    errors = errors
  )
}

.workflow_plots <- function(result, spec, failures = NULL) {
  requested <- as.character(spec$explain$plots)
  if (length(requested) == 0L || is.null(result$shap) || is.null(result$shap$objects)) {
    return(NULL)
  }
  graphs <- list()
  for (type in requested) {
    graph <- tryCatch(
      switch(
        type,
        importance = plot_shap_importance(result),
        beeswarm = plot_shap_beeswarm(result),
        dependence = plot_shap_dependence(result),
        waterfall = plot_shap_waterfall(result),
        force = plot_shap_force(result),
        NULL
      ),
      error = function(e) {
        .record_failure(
          failures,
          "*",
          sprintf("plot_%s", type),
          conditionMessage(e)
        )
        NULL
      }
    )
    if (!is.null(graph)) {
      graphs[[type]] <- graph
    }
  }
  if (length(graphs) == 0L) {
    return(NULL)
  }
  graphs
}

#' Run a complete uniml workflow
#'
#' Chains preprocessing, feature selection, tuning, cross validation,
#' calibration, evaluation, and ranking into a single `uniml_result`. The
#' validation and optional test sets never take part in fitting; they are only
#' transformed with parameters estimated on the training data.
#'
#' @param spec A `uniml_spec` object.
#' @param train Training data frame.
#' @param validation Validation data frame.
#' @param test Optional test data frame; reporting-only.
#' @param external Optional external validation data frame; reporting-only.
#' @param ci_R Number of bootstrap resamples for confidence intervals.
#'
#' @return A `uniml_result` with fitted models, predictions, metrics, and
#'   confidence intervals.
#' @references
#' Collins GS, Moons KGM, Dhiman P, et al. (2024). TRIPOD+AI statement: updated
#' guidance for reporting clinical prediction models that use regression or
#' machine learning methods. *BMJ*, 385, e078378. doi:10.1136/bmj-2023-078378
#'
#' Steyerberg EW, Harrell FE, Borsboom GJ, et al. (2001). Internal validation of
#' predictive models: efficiency of some procedures for logistic regression
#' analysis. *Journal of Clinical Epidemiology*, 54(8), 774-781.
#' doi:10.1016/S0895-4356(01)00341-9
#'
#' Steyerberg EW (2019). *Clinical Prediction Models: A Practical Approach to
#' Development, Validation, and Updating*, 2nd edition. Springer.
#' doi:10.1007/978-3-030-16399-0
#' @export
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   y = factor(rep(c("no", "yes"), 20)),
#'   x1 = stats::rnorm(40),
#'   x2 = stats::rnorm(40)
#' )
#' spec <- uniml_spec(task_type = "binary", outcome = "y", positive = "yes")
#' spec <- set_preprocess(spec, impute = "none", scale = "none", encode = "none")
#' spec <- set_feature_selection(spec, methods = "univariate")
#' spec <- set_learners(spec, "cart", tune = FALSE)
#' spec <- set_tuning(spec, folds = 2, evals = 1)
#' spec <- set_evaluation(spec, ci = "none", calibration = "none")
#' run_workflow(spec, train = data[1:30, ], validation = data[31:40, ])
#' }
run_workflow <- function(spec, train, validation, test = NULL,
                         external = NULL, ci_R = 1000) {
  validate_uniml_spec(spec)
  .assert_scalar_number(ci_R, "ci_R", integer = TRUE, min = 1)
  strategy <- spec$output$future_strategy
  if (identical(strategy, "sequential")) {
    future::plan(future::sequential)
  } else {
    future::plan(strategy, workers = as.integer(spec$output$n_workers))
  }
  on.exit(future::plan(future::sequential), add = TRUE)
  prepared_train <- .workflow_prepare_data(train, spec, "train")
  prepared_validation <- .workflow_prepare_data(
    validation,
    spec,
    "validation",
    levels = prepared_train$levels,
    require_two = FALSE
  )
  prepared_test <- if (is.null(test)) {
    NULL
  } else {
    .workflow_prepare_data(
      test,
      spec,
      "test",
      levels = prepared_train$levels,
      require_two = FALSE
    )
  }
  prepared_external <- if (is.null(external)) {
    NULL
  } else {
    .workflow_prepare_data(
      external,
      spec,
      "external",
      levels = prepared_train$levels,
      require_two = FALSE
    )
  }
  train_data <- prepared_train$data
  if (!spec$positive %in% prepared_train$levels) {
    .abort(sprintf("`positive` = \"%s\" is not present in the training outcome.", spec$positive))
  }
  if (length(unique(train_data[[spec$outcome]])) < 2L) {
    .abort("The training outcome must contain two classes.")
  }

  full_preprocessor <- .workflow_fit_preprocessor(train_data, spec)
  imputation_preprocessors <- .workflow_imputation_preprocessors(
    train_data,
    spec,
    full_preprocessor
  )
  multi_imputation <- length(imputation_preprocessors) > 1L

  processed_train <- imputation_preprocessors[[1L]]$train
  selections <- lapply(imputation_preprocessors, function(preprocessor) {
    found <- .workflow_select_features(preprocessor$train, spec)
    if (inherits(found, "error")) {
      stop(found)
    }
    found
  })
  selection <- if (multi_imputation) {
    .workflow_pool_selection(selections)
  } else {
    selections[[1L]]
  }

  selected_features <- intersect(
    as.character(selection$selected_features),
    names(processed_train)
  )
  if (length(selected_features) == 0L) {
    .abort("Feature selection returned no features available in the processed training data.")
  }
  task <- make_task(
    processed_train[, c(selected_features, spec$outcome), drop = FALSE],
    spec
  )
  failures <- .new_failure_log()
  learners <- resolve_learners(spec, task = task, n_train = task$nrow)
  learners <- .workflow_guard_single_feature(learners, task, failures)

  tuning <- list()
  models <- list()
  prediction_entries <- list()
  threshold_values <- list()
  metric_rows <- list()
  ci_rows <- list()
  for (key in names(learners)) {
    learner_spec <- learners[[key]]
    run_states <- list()
    for (imputation in seq_along(imputation_preprocessors)) {
      preprocessor <- imputation_preprocessors[[imputation]]
      imputation_selection <- selections[[imputation]]
      imputation_train <- preprocessor$train
      features <- intersect(
        as.character(imputation_selection$selected_features),
        names(imputation_train)
      )
      if (length(features) == 0L) {
        features <- intersect(selected_features, names(imputation_train))
      }
      if (length(features) == 0L) {
        .record_failure(failures, key, "features",
                        "No selected feature was available in the processed data.")
        next
      }
      imputation_task <- make_task(
        imputation_train[, c(features, spec$outcome), drop = FALSE],
        spec
      )
      tuned <- tryCatch(
        tune_model(
          imputation_task,
          learner_spec,
          resampling = .workflow_tuning_resampling(spec),
          tuner = spec$tuning$tuner,
          terminator = .workflow_tuning_terminator(spec),
          nested = spec$tuning$nested,
          seed = spec$seed
        ),
        error = function(e) e
      )
      if (inherits(tuned, "error") || identical(tuned$status, "failed")) {
        .record_failure(
          failures,
          key,
          "tuning",
          if (inherits(tuned, "error")) conditionMessage(tuned) else "Tuning failed."
        )
        next
      }
      tuned$metric <- spec$tuning$metric
      tuned$learner_key <- key
      learner <- tryCatch(
        train_model(imputation_task, tuned$learner, params = tuned$best_params),
        error = function(e) e
      )
      if (inherits(learner, "error")) {
        .record_failure(failures, key, "training", conditionMessage(learner))
        next
      }
      sets <- list(
        train = imputation_train,
        validation = prepared_validation$data
      )
      if (!is.null(prepared_test)) {
        sets$test <- prepared_test$data
      }
      if (!is.null(prepared_external)) {
        sets$external <- prepared_external$data
      }
      probs <- list()
      for (set in names(sets)) {
        transformed <- if (identical(set, "train")) {
          imputation_train
        } else {
          apply_preprocessor(preprocessor, sets[[set]])
        }
        set_features <- intersect(features, names(transformed))
        if (length(set_features) == 0L) {
          next
        }
        set_task <- make_task(
          transformed[, c(set_features, spec$outcome), drop = FALSE],
          spec
        )
        predicted <- tryCatch(
          .workflow_probabilities(learner, set_task, spec$positive),
          error = function(e) e
        )
        if (inherits(predicted, "error")) {
          next
        }
        probs[[set]] <- list(
          prob = predicted$prob,
          truth = as.character(sets[[set]][[spec$outcome]])
        )
      }
      if (!"train" %in% names(probs)) {
        next
      }
      run_states[[length(run_states) + 1L]] <- list(
        imputation = imputation,
        model_key = key,
        tuned = tuned,
        learner = learner,
        task = imputation_task,
        preprocessor = preprocessor,
        selection = imputation_selection,
        probs = probs
      )
    }
    if (length(run_states) == 0L) {
      next
    }
    tuning[[key]] <- run_states[[1L]]$tuned
    train_probabilities <- lapply(run_states, function(state) state$probs$train$prob)
    pooled_train_prob <- .workflow_mean_prob(train_probabilities)
    train_truth <- run_states[[1L]]$probs$train$truth
    train_event <- as.integer(
      as.character(train_truth) == as.character(spec$positive)
    )
    fit_source <- spec$evaluation$calibration_args$fit
    if (is.null(fit_source)) {
      fit_source <- "out_of_fold"
    }
    calibration_source <- if (identical(fit_source, "in_sample")) {
      list(prob = pooled_train_prob, event = train_event, all = pooled_train_prob)
    } else {
      .workflow_oof_event(
        run_states,
        spec,
        data = train_data,
        failures = failures
      )
    }
    if (length(calibration_source$event) < 10L ||
        length(unique(calibration_source$event)) < 2L) {
      .record_failure(
        failures,
        key,
        "calibration",
        "Not enough out-of-fold observations with both classes; in-sample predictions were used."
      )
      calibration_source <- list(
        prob = pooled_train_prob,
        event = train_event,
        all = pooled_train_prob
      )
    }
    calibration <- .workflow_calibrator(
      calibration_source$prob,
      calibration_source$event,
      spec
    )
    calibrated_train <- if (isTRUE(calibration$failed)) {
      list(
        prob = rep(NA_real_, length(pooled_train_prob)),
        failed = TRUE,
        message = calibration$message
      )
    } else if (isTRUE(calibration$requested)) {
      .workflow_calibrated(calibration$fit, pooled_train_prob)
    } else {
      list(
        prob = rep(NA_real_, length(pooled_train_prob)),
        failed = FALSE,
        message = NULL
      )
    }
    calibration_success <- isTRUE(calibration$requested) &&
      !isTRUE(calibration$failed) &&
      !isTRUE(calibrated_train$failed)
    calibration_message <- if (isTRUE(calibration$failed)) {
      calibration$message
    } else if (isTRUE(calibrated_train$failed)) {
      calibrated_train$message
    } else {
      NULL
    }
    if (isTRUE(calibration$requested) && !calibration_success) {
      if (is.null(calibration_message)) {
        calibration_message <- "Unknown calibration error."
      }
      .record_failure(failures, key, "calibration", calibration_message)
      warning(
        sprintf(
          "Probability calibration failed for model \"%s\"; calibrated probabilities are unavailable. %s",
          key,
          calibration_message
        ),
        call. = FALSE
      )
    }
    calibrated_source <- if (calibration_success) {
      .workflow_calibrated(calibration$fit, calibration_source$prob)$prob
    } else {
      NULL
    }
    model_thresholds <- list(
      raw = .workflow_threshold_event(
        calibration_source$prob,
        calibration_source$event,
        spec
      ),
      calibrated = if (calibration_success) {
        .workflow_threshold_event(
          calibrated_source,
          calibration_source$event,
          spec
        )
      } else {
        NA_real_
      }
    )
    deployment_threshold <- if (calibration_success &&
                                is.finite(model_thresholds$calibrated)) {
      model_thresholds$calibrated
    } else {
      model_thresholds$raw
    }
    threshold_values[[key]] <- model_thresholds
    calibrator <- if (calibration_success) calibration$fit else NULL
    models[[key]] <- list(
      learner = run_states[[1L]]$learner,
      calibrator = calibrator,
      threshold = deployment_threshold,
      thresholds = model_thresholds,
      calibration_requested = isTRUE(calibration$requested),
      calibration_failed = isTRUE(calibration$requested) && !calibration_success,
      calibration_message = calibration_message
    )
    if (multi_imputation) {
      models[[key]]$learners <- lapply(run_states, function(state) state$learner)
      models[[key]]$preprocessors <- lapply(run_states, function(state) state$preprocessor)
      models[[key]]$selections <- lapply(run_states, function(state) state$selection)
      models[[key]]$imputations <- vapply(
        run_states, function(state) as.integer(state$imputation), integer(1)
      )
      models[[key]]$best_params <- .workflow_pool_params(
        lapply(run_states, function(state) state$tuned$best_params)
      )
    }
    for (set in names(run_states[[1L]]$probs)) {
      set_states <- run_states[vapply(
        run_states,
        function(state) set %in% names(state$probs),
        logical(1)
      )]
      set_probabilities <- lapply(set_states, function(state) state$probs[[set]]$prob)
      pooled_prob <- .workflow_mean_prob(set_probabilities)
      truth <- set_states[[1L]]$probs[[set]]$truth
      calibrated_prob <- rep(NA_real_, length(pooled_prob))
      calibrated_states <- vector("list", length(set_states))
      set_calibration_success <- calibration_success
      if (set_calibration_success) {
        calibrated_set <- .workflow_calibrated(calibrator, pooled_prob)
        if (isTRUE(calibrated_set$failed)) {
          set_calibration_success <- FALSE
          warning(
            sprintf(
              "Calibrated probabilities could not be produced for model \"%s\" on the %s set. %s",
              key,
              set,
              calibrated_set$message
            ),
            call. = FALSE
          )
        } else {
          calibrated_prob <- calibrated_set$prob
          for (state_index in seq_along(set_states)) {
            calibrated_states[[state_index]] <- .workflow_calibrated(
              calibrator,
              set_states[[state_index]]$probs[[set]]$prob
            )
          }
          state_failures <- vapply(
            calibrated_states,
            function(item) isTRUE(item$failed),
            logical(1)
          )
          if (any(state_failures)) {
            set_calibration_success <- FALSE
            calibrated_prob <- rep(NA_real_, length(pooled_prob))
            warning(
              sprintf(
                "Calibration failed for at least one imputation for model \"%s\" on the %s set.",
                key,
                set
              ),
              call. = FALSE
            )
          }
        }
      }
      prediction_entries[[paste0(key, ":", set)]] <- list(
        model = key,
        prob = pooled_prob,
        calibrated_prob = calibrated_prob,
        calibration_success = set_calibration_success,
        truth = truth,
        positive = spec$positive,
        set = set
      )
      probability_paths <- list(
        raw = list(
          pooled = pooled_prob,
          threshold = model_thresholds$raw,
          states = lapply(set_states, function(state) state$probs[[set]]$prob)
        )
      )
      if (set_calibration_success) {
        probability_paths$calibrated <- list(
          pooled = calibrated_prob,
          threshold = model_thresholds$calibrated,
          states = lapply(calibrated_states, `[[`, "prob")
        )
      }
      for (probability in names(probability_paths)) {
        path <- probability_paths[[probability]]
        metric_rows[[length(metric_rows) + 1L]] <- if (length(set_states) == 1L) {
          .workflow_metrics(
            path$pooled,
            truth,
            path$threshold,
            spec,
            key,
            set,
            probability
          )
        } else {
          .workflow_pool_metric_rows(lapply(seq_along(set_states), function(index) {
            .workflow_metrics(
              path$states[[index]],
              set_states[[index]]$probs[[set]]$truth,
              path$threshold,
              spec,
              key,
              set,
              probability
            )
          }))
        }
        if (identical(spec$evaluation$ci, "bootstrap")) {
          intervals <- lapply(seq_along(set_states), function(index) {
            tryCatch(
              calc_ci(
                path$states[[index]],
                set_states[[index]]$probs[[set]]$truth,
                method = "bootstrap",
                R = as.integer(ci_R),
                seed = spec$seed + index - 1L,
                threshold = path$threshold,
                positive = spec$positive,
                metrics = .workflow_metric_names()
              ),
              error = function(e) NULL
            )
          })
          interval <- if (length(set_states) == 1L) {
            single <- intervals[[1L]]
            if (!is.null(single)) {
              single$model <- key
              single$set <- set
              single$probability <- probability
            }
            single
          } else {
            .workflow_pool_ci(
              intervals,
              key,
              set,
              probability = probability
            )
          }
          if (!is.null(interval)) {
            ci_rows[[length(ci_rows) + 1L]] <- interval
          }
        }
      }
    }
  }

  if (length(models) == 0L) {
    .abort("No model could be fitted for the workflow.")
  }

  predictions <- .workflow_prediction_table(prediction_entries)
  # rebuild with the required column order
  predictions <- data.frame(
    model = predictions$model,
    prob = predictions$prob,
    calibrated_prob = predictions$calibrated_prob,
    truth = predictions$truth,
    positive = predictions$positive,
    set = predictions$set,
    stringsAsFactors = FALSE
  )

  metrics <- if (length(metric_rows) == 0L) {
    data.frame(
      model = character(),
      set = character(),
      probability = character(),
      stringsAsFactors = FALSE
    )
  } else {
    out <- do.call(rbind, metric_rows)
    out <- out[, c(
      "model",
      "set",
      "probability",
      setdiff(names(out), c("model", "set", "probability"))
    ), drop = FALSE]
    rownames(out) <- NULL
    out
  }
  ci <- if (length(ci_rows) == 0L) {
    NULL
  } else {
    out <- do.call(rbind, ci_rows)
    out <- out[, c(
      "model",
      "set",
      "probability",
      setdiff(names(out), c("model", "set", "probability"))
    ), drop = FALSE]
    rownames(out) <- NULL
    out
  }

  cv <- .workflow_cv_multi(
    train_data,
    spec,
    imputation_preprocessors = imputation_preprocessors,
    selections = selections,
    tuned_models = tuning,
    failures = failures
  )
  comparison_set <- .workflow_comparison_set(prediction_entries, spec$select_on)
  calibration <- .workflow_calibration_result(
    prediction_entries,
    models,
    spec
  )
  tests <- .workflow_test_results(
    prediction_entries,
    spec,
    comparison_set
  )
  requested_reference <- spec$evaluation$reference_model
  if (is.null(requested_reference)) {
    requested_reference <- spec$reference_model
  }
  if (!is.null(requested_reference) &&
      !requested_reference %in% names(models)) {
    .abort(sprintf(
      "Reference model \"%s\" is not among the fitted models (%s).",
      requested_reference,
      paste(names(models), collapse = ", ")
    ))
  }
  nri_idi <- tryCatch(
    .workflow_nri_idi(
      prediction_entries,
      models,
      metrics,
      spec,
      comparison_set,
      ci_R = ci_R
    ),
    error = function(e) {
      .record_failure(failures, "*", "nri_idi", conditionMessage(e))
      NULL
    }
  )
  if (!is.null(nri_idi)) {
    if (is.null(tests)) {
      tests <- list()
    }
    tests$nri_idi <- nri_idi
  }
  dca <- .workflow_dca_result(
    prediction_entries,
    spec,
    comparison_set
  )

  result <- assemble_result(
    data_info = list(
      task_type = spec$task_type,
      outcome = spec$outcome,
      positive = spec$positive,
      n_train = nrow(train_data),
      n_validation = nrow(prepared_validation$data),
      n_test = if (is.null(prepared_test)) 0L else nrow(prepared_test$data),
      n_external = if (is.null(prepared_external)) {
        0L
      } else {
        nrow(prepared_external$data)
      }
    ),
    preprocess = full_preprocessor,
    selection = selection,
    tuning = tuning,
    cv = cv,
    metrics = metrics,
    ci = ci,
    calibration = calibration,
    tests = tests,
    dca = dca,
    models = models,
    meta = list(
      spec = spec,
      predictions = predictions,
      thresholds = threshold_values,
      leakage_policy = spec$leakage_policy,
      in_cv = spec$in_cv,
      select_on = comparison_set,
      seed = as.integer(spec$seed),
      ci_R = as.integer(ci_R),
      calibration_fit = spec$evaluation$calibration_args$fit,
      resampling = cv$resampling
    ),
    select_on = comparison_set,
    best_model_metric = spec$best_model_metric
  )
  result["shap"] <- list(.workflow_shap(result, spec))
  result["thresholds"] <- list(.workflow_shap_thresholds(result, spec, failures))
  result["plots"] <- list(.workflow_plots(result, spec, failures))
  result$meta$failures <- .failure_table(failures)
  failure_rows <- result$meta$failures
  if (nrow(failure_rows) > 0L) {
    warning(
      sprintf(
        "%d workflow step(s) failed or were downgraded; see `result$meta$failures`.",
        nrow(failure_rows)
      ),
      call. = FALSE
    )
  }
  result
}

#' Predict with a fitted uniml workflow
#'
#' Reuses the stored preprocessing, feature selection, calibration, and final
#' model. The new data are never used to refit any component.
#'
#' @param object A `uniml_result` returned by `run_workflow()`.
#' @param newdata New data frame with the same feature columns as the training
#'   data.
#' @param model `"best"` or one of `names(object$models)`.
#' @param ... Reserved for future use.
#'
#' @return A data frame with the model key, raw and calibrated probabilities,
#'   the classification from the stored threshold, the stored threshold itself,
#'   and the truth labels when an outcome column is available.
#' @export
#' @examples
#' \dontrun{
#' res <- run_workflow(spec, train = train, validation = validation)
#' predict_uniml(res, train)
#' }
predict_uniml <- function(object, newdata, model = "best", ...) {
  validate_uniml_result(object)
  if (!inherits(object, "uniml_result")) {
    .abort("`object` must be a `uniml_result`.")
  }
  if (is.null(object$meta$spec) || is.null(object$models) || is.null(object$preprocess)) {
    .abort("`object` was not created by `run_workflow()`; predictions are unavailable.")
  }
  if (!is.data.frame(newdata)) {
    .abort("`newdata` must be a data frame.")
  }
  key <- .workflow_model_key(object, model)
  entry <- object$models[[key]]
  if (!is.list(entry)) {
    .abort(sprintf("Model \"%s\" does not contain a fitted learner.", key))
  }
  learners <- if (!is.null(entry$learners) && length(entry$learners) > 0L) {
    as.list(entry$learners)
  } else {
    list(entry$learner)
  }
  if (!all(vapply(learners, inherits, logical(1), "Learner"))) {
    .abort(sprintf("Model \"%s\" does not contain fitted learners.", key))
  }
  preprocessors <- if (!is.null(entry$preprocessors) &&
                       length(entry$preprocessors) == length(learners)) {
    entry$preprocessors
  } else {
    rep(list(object$preprocess), length(learners))
  }
  selections <- if (!is.null(entry$selections) &&
                    length(entry$selections) == length(learners)) {
    entry$selections
  } else {
    rep(list(object$selection), length(learners))
  }
  spec <- object$meta$spec
  training_outcome <- object$preprocess$train[[spec$outcome]]
  outcome_levels <- if (is.factor(training_outcome)) {
    levels(training_outcome)
  } else {
    sort(unique(as.character(training_outcome)))
  }
  if (!spec$positive %in% outcome_levels) {
    .abort("The stored positive class is not present in the fitted outcome levels.")
  }
  has_outcome <- spec$outcome %in% names(newdata)
  truth_raw <- if (has_outcome) as.character(newdata[[spec$outcome]]) else NULL
  data <- .workflow_prepare_features(newdata, spec, "newdata")
  if (!has_outcome) {
    data[[spec$outcome]] <- factor(
      rep(outcome_levels[1L], nrow(data)),
      levels = outcome_levels
    )
  }
  if (has_outcome) {
    invalid <- !is.na(truth_raw) & !truth_raw %in% outcome_levels
    if (any(invalid)) {
      .abort("`newdata` contains outcome levels that were not present during training.")
    }
  }
  probability_list <- list()
  for (index in seq_along(learners)) {
    transformed <- apply_preprocessor(preprocessors[[index]], data)
    features <- intersect(
      as.character(selections[[index]]$selected_features),
      names(transformed)
    )
    if (length(features) == 0L) {
      next
    }
    transformed[[spec$outcome]] <- if (has_outcome) {
      factor(truth_raw, levels = outcome_levels)
    } else {
      factor(
        rep(outcome_levels[1L], nrow(transformed)),
        levels = outcome_levels
      )
    }
    task <- make_task(
      transformed[, c(features, spec$outcome), drop = FALSE],
      spec
    )
    predicted <- tryCatch(
      .workflow_probabilities(learners[[index]], task, spec$positive)$prob,
      error = function(e) e
    )
    if (inherits(predicted, "error")) {
      next
    }
    probability_list[[length(probability_list) + 1L]] <- predicted
  }
  if (length(probability_list) == 0L) {
    .abort("No selected features are available for prediction.")
  }
  prob <- .workflow_mean_prob(probability_list)
  calibrated_prob <- rep(NA_real_, length(prob))
  calibrated_available <- FALSE
  if (!is.null(entry$calibrator)) {
    calibrated <- .workflow_calibrated(entry$calibrator, prob)
    if (isTRUE(calibrated$failed)) {
      warning(
        sprintf(
          "Calibration failed during prediction for model \"%s\"; raw probabilities and the raw threshold were used. %s",
          key,
          calibrated$message
        ),
        call. = FALSE
      )
    } else {
      calibrated_prob <- calibrated$prob
      calibrated_available <- TRUE
    }
  }
  thresholds <- entry$thresholds
  raw_threshold <- if (is.list(thresholds) && !is.null(thresholds$raw)) {
    thresholds$raw
  } else if (!is.null(entry$threshold)) {
    entry$threshold
  } else {
    NA_real_
  }
  calibrated_threshold <- if (is.list(thresholds) &&
                              !is.null(thresholds$calibrated)) {
    thresholds$calibrated
  } else if (!is.null(entry$calibrator)) {
    entry$threshold
  } else {
    NA_real_
  }
  use_calibrated <- calibrated_available && is.finite(calibrated_threshold)
  probability <- if (use_calibrated) "calibrated" else "raw"
  selected_prob <- if (use_calibrated) calibrated_prob else prob
  threshold <- if (use_calibrated) calibrated_threshold else raw_threshold
  truth <- if (is.null(truth_raw)) rep(NA_character_, length(prob)) else truth_raw
  predicted <- if (is.na(threshold)) {
    rep(NA_integer_, length(prob))
  } else {
    as.integer(selected_prob >= threshold)
  }
  out <- data.frame(
    model = key,
    prob = prob,
    calibrated_prob = calibrated_prob,
    probability = probability,
    predicted = predicted,
    threshold = threshold,
    truth = truth,
    stringsAsFactors = FALSE
  )
  if (any(!is.na(out$truth))) {
    metrics <- tryCatch(
      .workflow_metrics(
        selected_prob,
        out$truth,
        threshold,
        spec,
        key,
        "newdata",
        probability
      ),
      error = function(e) NULL
    )
    attr(out, "metrics") <- metrics
  }
  out
}

#' @rdname predict_uniml
#' @export
predict.uniml_result <- function(object, newdata, model = "best", ...) {
  predict_uniml(object, newdata, model = model, ...)
}
