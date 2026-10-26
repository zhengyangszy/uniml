# L9b workflow engine ---------------------------------------------------------
#
# The engine owns everything that does not depend on the outcome shape:
# preparing the data, fitting the preprocessor and the feature selection, tuning,
# training one model per learner and imputation, and predicting every evaluation
# set. It never scores anything.
#
# Scoring, thresholds, calibration, and the rest of the report are the job of the
# caller: `run_workflow()` in R/workflow.R for binary outcomes, and the
# `unimlsurv` report for survival ones. The task-type hooks used here are
# described in `register_workflow_adapter()`.

#' Fit a task type's models and predict the evaluation sets
#'
#' The shared half of a workflow. Given a specification and the data sets, it
#' prepares the data, fits imputation, preprocessing, and feature selection, tunes
#' and trains one model per learner, and predicts the training, validation, test,
#' and external sets.
#'
#' It stops there. Turning predictions into numbers is the caller's job, because
#' that is where task types differ; see [register_workflow_adapter()] and
#' [run_workflow()], which is the binary report on top of this engine.
#'
#' @param spec A `uniml_spec` object.
#' @param train Training data frame.
#' @param validation Validation data frame.
#' @param test Optional test data frame; reporting-only.
#' @param external Optional external data frame; reporting-only.
#' @param adapter Workflow adapter; defaults to the one registered for
#'   `spec$task_type`.
#' @param learners Optional resolved learner list, used when the task type's
#'   learners are not resolvable by [resolve_learners()].
#'
#' @return A list with the prepared data (`prepared`, `train_data`), the fitted
#'   `preprocessor`, the `selection`, the mlr3 `task`, the `learners`, one fitted
#'   state per learner in `states`, the `tuning` results, and the `failures` log.
#'   Each state holds `tuned`, `learner`, `task`, `preprocessor`, `selection`,
#'   `imputation`, and `values`, where `values[[set]]` is
#'   `list(value, truth, prediction)`.
#' @export
#' @examples
#' \dontrun{
#' spec <- uniml_spec(task_type = "binary", outcome = "y", positive = "yes")
#' engine <- workflow_engine(spec, train, validation)
#' names(engine$states)
#' }
workflow_engine <- function(spec, train, validation, test = NULL, external = NULL,
                            adapter = workflow_adapter(spec$task_type),
                            learners = NULL) {
  validate_uniml_spec(spec)
  if (!is.list(adapter) || !all(.workflow_adapter_hooks() %in% names(adapter))) {
    .abort("`adapter` must be a workflow adapter; see `register_workflow_adapter()`.")
  }
  strategy <- spec$output$future_strategy
  if (identical(strategy, "sequential")) {
    future::plan(future::sequential)
  } else {
    future::plan(strategy, workers = as.integer(spec$output$n_workers))
  }
  on.exit(future::plan(future::sequential), add = TRUE)

  prepared_train <- adapter$prepare(train, spec, "train")
  prepared_validation <- adapter$prepare(
    validation,
    spec,
    "validation",
    levels = prepared_train$levels,
    require_two = FALSE
  )
  prepared_test <- if (is.null(test)) {
    NULL
  } else {
    adapter$prepare(test, spec, "test", levels = prepared_train$levels,
                    require_two = FALSE)
  }
  prepared_external <- if (is.null(external)) {
    NULL
  } else {
    adapter$prepare(external, spec, "external", levels = prepared_train$levels,
                    require_two = FALSE)
  }
  train_data <- prepared_train$data

  full_preprocessor <- .workflow_fit_preprocessor(train_data, spec)
  imputation_preprocessors <- .workflow_imputation_preprocessors(
    train_data,
    spec,
    full_preprocessor
  )
  multi_imputation <- length(imputation_preprocessors) > 1L

  processed_train <- imputation_preprocessors[[1L]]$train
  selections <- lapply(imputation_preprocessors, function(preprocessor) {
    found <- .workflow_select_features(preprocessor$train, spec, adapter = adapter)
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
  task <- adapter$make_task(processed_train, spec, selected_features)
  failures <- .new_failure_log()
  if (is.null(learners)) {
    learners <- resolve_learners(spec, task = task, n_train = task$nrow)
  }
  learners <- .workflow_guard_single_feature(learners, task, failures)

  tuning <- list()
  states <- list()
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
      imputation_task <- adapter$make_task(imputation_train, spec, features)
      tuned <- tryCatch(
        if (is.null(adapter$tune)) {
          tune_model(
            imputation_task,
            learner_spec,
            resampling = .workflow_tuning_resampling(spec),
            tuner = spec$tuning$tuner,
            terminator = .workflow_tuning_terminator(spec),
            nested = spec$tuning$nested,
            seed = spec$seed
          )
        } else {
          adapter$tune(imputation_task, learner_spec, spec)
        },
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
        if (is.null(adapter$train)) {
          train_model(imputation_task, tuned$learner, params = tuned$best_params)
        } else {
          adapter$train(imputation_task, tuned$learner, tuned$best_params, spec)
        },
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
      values <- list()
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
        set_task <- adapter$make_task(transformed, spec, set_features)
        predicted <- tryCatch(
          adapter$predict(learner, set_task, spec),
          error = function(e) e
        )
        if (inherits(predicted, "error")) {
          next
        }
        values[[set]] <- list(
          value = predicted$value,
          prediction = predicted$prediction,
          truth = as.character(sets[[set]][[spec$outcome]])
        )
      }
      if (!"train" %in% names(values)) {
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
        values = values
      )
    }
    if (length(run_states) == 0L) {
      next
    }
    tuning[[key]] <- run_states[[1L]]$tuned
    states[[key]] <- run_states
  }

  list(
    adapter = adapter,
    prepared = list(
      train = prepared_train,
      validation = prepared_validation,
      test = prepared_test,
      external = prepared_external
    ),
    train_data = train_data,
    preprocessor = full_preprocessor,
    imputation_preprocessors = imputation_preprocessors,
    selections = selections,
    selection = selection,
    selected_features = selected_features,
    task = task,
    learners = learners,
    tuning = tuning,
    states = states,
    multi_imputation = multi_imputation,
    failures = failures
  )
}
