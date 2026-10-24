# L4 model tuning -----------------------------------------------------------

.tuning_measure_for <- function(metric) {
  if (inherits(metric, "Measure")) {
    if (!identical(metric$task_type, "classif")) {
      .abort("`metric` must be a classification measure.")
    }
    return(metric$clone())
  }

  .assert_scalar_character(metric, "metric")
  switch(
    metric,
    auc = mlr3::msr("classif.auc"),
    pr_auc = mlr3::msr("classif.prauc"),
    brier = mlr3::msr("classif.bbrier"),
    accuracy = mlr3::msr("classif.acc"),
    sensitivity = mlr3::msr("classif.sensitivity"),
    specificity = mlr3::msr("classif.specificity"),
    ppv = mlr3::msr("classif.ppv"),
    npv = mlr3::msr("classif.npv"),
    f1 = mlr3::msr("classif.fbeta", beta = 1),
    f2 = mlr3::msr("classif.fbeta", beta = 2),
    balanced_accuracy = mlr3::msr("classif.bacc"),
    dor = mlr3::msr("classif.dor"),
    kappa = .abort(
      "Tuning metric \"kappa\" is not supported by the installed mlr3 measures."
    ),
    .abort(sprintf("Unsupported tuning metric \"%s\".", metric))
  )
}

.learner_tuning_metric <- function(learner_spec) {
  metric <- learner_spec$metric
  if (is.null(metric)) {
    metric <- "auc"
  }
  metric
}

.tuning_tuner_for <- function(tuner) {
  if (inherits(tuner, "Tuner")) {
    return(tuner$clone(deep = TRUE))
  }
  .assert_scalar_character(tuner, "tuner")
  switch(
    tuner,
    random_search = mlr3tuning::tnr("random_search"),
    grid_search = mlr3tuning::tnr("grid_search"),
    bayesian = {
      if (!requireNamespace("mlr3mbo", quietly = TRUE)) {
        .abort("The `mlr3mbo` package is required for Bayesian tuning.")
      }
      if (!requireNamespace("DiceKriging", quietly = TRUE)) {
        .abort("The `DiceKriging` package is required for Bayesian tuning.")
      }
      mlr3tuning::tnr("mbo")
    },
    .abort(sprintf("Unsupported tuning strategy \"%s\".", tuner))
  )
}

.tuning_terminator_for <- function(terminator) {
  if (is.null(terminator)) {
    return(mlr3tuning::trm("evals", n_evals = 20L))
  }
  if (!inherits(terminator, "Terminator")) {
    .abort("`terminator` must be NULL or an mlr3tuning terminator.")
  }
  terminator$clone(deep = TRUE)
}

.tuning_search_space_for <- function(learner_spec, task) {
  search_space <- learner_spec$search_space
  if (is.function(search_space)) {
    search_space <- search_space(task, params = learner_spec$params)
  }
  if (!is.null(search_space) && !.is_param_set(search_space)) {
    .abort("The learner search space must be a `paradox::ParamSet`.")
  }
  search_space
}

.first_numeric_value <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_real_)
  }
  as.numeric(x[[1L]])
}

.new_tuning_result <- function(status, learner, learner_id, best_params,
                               best_metric, measure, tuner, terminator,
                               resampling, nested, seed, tuning_instance,
                               archive, cv_result, cv_scores, error = NULL) {
  structure(
    list(
      status = status,
      learner = learner,
      learner_id = learner_id,
      best_params = best_params,
      best_metric = best_metric,
      measure = measure,
      tuner = tuner,
      terminator = terminator,
      resampling = resampling,
      nested = nested,
      seed = as.integer(seed),
      tuning_instance = tuning_instance,
      archive = archive,
      cv_result = cv_result,
      cv_scores = cv_scores,
      error = error
    ),
    class = c("uniml_tuning_result", "list")
  )
}

.cv_scores_for <- function(cv_result, measure) {
  if (is.null(cv_result)) {
    return(NULL)
  }
  cv_result$score(measure)
}

.validate_tuning_inputs <- function(task, learner_spec, resampling, nested,
                                    seed) {
  if (!inherits(task, "TaskClassif")) {
    .abort("`task` must be an mlr3 classification task.")
  }
  if (!inherits(learner_spec, "uniml_learner_spec")) {
    .abort("`learner_spec` must be an `uniml_learner_spec` object.")
  }
  if (!inherits(learner_spec$learner, "Learner")) {
    .abort("`learner_spec` must contain an mlr3 learner.")
  }
  if (is.null(resampling)) {
    resampling <- mlr3::rsmp("cv", folds = 5L)
  }
  if (!inherits(resampling, "Resampling")) {
    .abort("`resampling` must be NULL or an mlr3 resampling object.")
  }
  .assert_scalar_logical(nested, "nested")
  .assert_scalar_number(seed, "seed", integer = TRUE)
  list(resampling = resampling)
}

#' Tune an mlr3 learner
#'
#' Tunes one resolved learner specification on a training task. Random search,
#' grid search, and Bayesian optimization are supported. When `nested = FALSE`,
#' the tuning instance is optimized on the supplied resampling and the selected
#' parameters are evaluated again with the same resampling. When `nested =
#' TRUE`, an [mlr3tuning::auto_tuner()] is evaluated on an outer resampling
#' before the best configuration is extracted on the full task. The tuning
#' metric is taken from `learner_spec$metric`, which is normally populated from
#' `set_tuning()`.
#'
#' @param task An mlr3 classification task.
#' @param learner_spec A `uniml_learner_spec` object from
#'   [resolve_learners()].
#' @param resampling An mlr3 resampling object used for tuning and evaluation.
#' @param tuner Tuning strategy: `"random_search"`, `"grid_search"`,
#'   `"bayesian"`, or an mlr3 tuner object.
#' @param terminator An mlr3tuning terminator. The default uses 20 evals.
#' @param nested Whether to use nested tuning.
#' @param seed Random seed.
#'
#' @return A structured `uniml_tuning_result` with the configured learner,
#'   best parameters, best tuning value, reproducibility metadata, and the
#'   resulting cross-validation result.
#' @export
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   y = factor(rep(c("no", "yes"), 20)),
#'   x1 = stats::rnorm(40),
#'   x2 = stats::rnorm(40)
#' )
#' spec <- uniml_spec(task_type = "binary", outcome = "y", positive = "yes")
#' spec <- set_learners(spec, "cart", tune = TRUE)
#' task <- make_task(data, spec)
#' learners <- resolve_learners(spec, task = task)
#' tune_model(
#'   task, learners[[1L]],
#'   resampling = mlr3::rsmp("cv", folds = 3),
#'   tuner = "random_search",
#'   terminator = mlr3tuning::trm("evals", n_evals = 2)
#' )
#' }
tune_model <- function(task, learner_spec, resampling = NULL,
                       tuner = "random_search", terminator = NULL,
                       nested = FALSE, seed = 123) {
  inputs <- .validate_tuning_inputs(
    task = task,
    learner_spec = learner_spec,
    resampling = resampling,
    nested = nested,
    seed = seed
  )
  resampling <- inputs$resampling
  measure <- .tuning_measure_for(.learner_tuning_metric(learner_spec))
  search_space <- .tuning_search_space_for(learner_spec, task)
  tuner_obj <- .tuning_tuner_for(tuner)
  terminator_obj <- .tuning_terminator_for(terminator)
  base_learner <- learner_spec$learner$clone(deep = TRUE)

  if (!isTRUE(learner_spec$tune) || is.null(search_space)) {
    set.seed(as.integer(seed))
    cv_result <- mlr3::resample(
      task,
      base_learner$clone(deep = TRUE),
      resampling,
      store_models = FALSE
    )
    return(.new_tuning_result(
      status = "untuned",
      learner = base_learner,
      learner_id = learner_spec$key,
      best_params = if (is.null(learner_spec$params)) list() else learner_spec$params,
      best_metric = NA_real_,
      measure = measure,
      tuner = tuner_obj,
      terminator = terminator_obj,
      resampling = resampling,
      nested = nested,
      seed = seed,
      tuning_instance = NULL,
      archive = NULL,
      cv_result = cv_result,
      cv_scores = .cv_scores_for(cv_result, measure)
    ))
  }

  if (isTRUE(nested)) {
    nested_fit <- tryCatch(
      {
        auto_tuner <- mlr3tuning::auto_tuner(
          tuner = tuner_obj,
          learner = base_learner$clone(deep = TRUE),
          resampling = resampling,
          measure = measure,
          terminator = terminator_obj,
          search_space = search_space,
          store_benchmark_result = TRUE,
          store_models = FALSE
        )
        set.seed(as.integer(seed))
        outer_result <- mlr3::resample(
          task,
          auto_tuner,
          resampling,
          store_models = FALSE
        )
        set.seed(as.integer(seed))
        auto_tuner$train(task)
        list(auto_tuner = auto_tuner, outer_result = outer_result)
      },
      error = function(e) e
    )

    if (inherits(nested_fit, "error")) {
      return(.new_tuning_result(
        status = "failed",
        learner = base_learner,
        learner_id = learner_spec$key,
        best_params = list(),
        best_metric = NA_real_,
        measure = measure,
        tuner = tuner_obj,
        terminator = terminator_obj,
        resampling = resampling,
        nested = TRUE,
        seed = seed,
        tuning_instance = NULL,
        archive = NULL,
        cv_result = NULL,
        cv_scores = NULL,
        error = conditionMessage(nested_fit)
      ))
    }

    instance <- nested_fit$auto_tuner$tuning_instance
    best_params <- if (!is.null(instance)) {
      instance$result_learner_param_vals
    } else {
      NULL
    }
    if (is.null(best_params)) {
      best_params <- list()
    }
    tuned_learner <- base_learner$clone(deep = TRUE)
    if (length(best_params) > 0L) {
      tuned_learner$param_set$set_values(.values = best_params)
    }
    outer_result <- nested_fit$outer_result
    return(.new_tuning_result(
      status = "nested_tuned",
      learner = tuned_learner,
      learner_id = learner_spec$key,
      best_params = best_params,
      best_metric = if (!is.null(instance)) {
        .first_numeric_value(instance$result_y)
      } else {
        NA_real_
      },
      measure = measure,
      tuner = tuner_obj,
      terminator = terminator_obj,
      resampling = resampling,
      nested = TRUE,
      seed = seed,
      tuning_instance = instance,
      archive = if (!is.null(instance)) instance$archive else NULL,
      cv_result = outer_result,
      cv_scores = .cv_scores_for(outer_result, measure)
    ))
  }

  instance <- tryCatch(
    {
      set.seed(as.integer(seed))
      mlr3tuning::ti(
        task = task,
        learner = base_learner$clone(deep = TRUE),
        resampling = resampling,
        measures = measure,
        terminator = terminator_obj,
        search_space = search_space,
        store_benchmark_result = TRUE,
        store_models = FALSE
      )
    },
    error = function(e) e
  )
  if (inherits(instance, "error")) {
    return(.new_tuning_result(
      status = "failed",
      learner = base_learner,
      learner_id = learner_spec$key,
      best_params = list(),
      best_metric = NA_real_,
      measure = measure,
      tuner = tuner_obj,
      terminator = terminator_obj,
      resampling = resampling,
      nested = FALSE,
      seed = seed,
      tuning_instance = NULL,
      archive = NULL,
      cv_result = NULL,
      cv_scores = NULL,
      error = conditionMessage(instance)
    ))
  }

  optimized <- tryCatch(
    {
      set.seed(as.integer(seed))
      tuner_obj$optimize(instance)
      TRUE
    },
    error = function(e) e
  )
  if (inherits(optimized, "error")) {
    return(.new_tuning_result(
      status = "failed",
      learner = base_learner,
      learner_id = learner_spec$key,
      best_params = list(),
      best_metric = NA_real_,
      measure = measure,
      tuner = tuner_obj,
      terminator = terminator_obj,
      resampling = resampling,
      nested = FALSE,
      seed = seed,
      tuning_instance = instance,
      archive = instance$archive,
      cv_result = NULL,
      cv_scores = NULL,
      error = conditionMessage(optimized)
    ))
  }

  best_params <- instance$result_learner_param_vals
  if (is.null(best_params)) {
    best_params <- list()
  }
  tuned_learner <- base_learner$clone(deep = TRUE)
  if (length(best_params) > 0L) {
    tuned_learner$param_set$set_values(.values = best_params)
  }
  set.seed(as.integer(seed))
  cv_result <- mlr3::resample(
    task,
    tuned_learner,
    resampling,
    store_models = FALSE
  )
  .new_tuning_result(
    status = "tuned",
    learner = tuned_learner,
    learner_id = learner_spec$key,
    best_params = best_params,
    best_metric = .first_numeric_value(instance$result_y),
    measure = measure,
    tuner = tuner_obj,
    terminator = terminator_obj,
    resampling = resampling,
    nested = FALSE,
    seed = seed,
    tuning_instance = instance,
    archive = instance$archive,
    cv_result = cv_result,
    cv_scores = .cv_scores_for(cv_result, measure)
  )
}
