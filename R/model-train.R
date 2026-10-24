# L4 final model training ---------------------------------------------------

.training_learner <- function(learner, params = NULL) {
  if (inherits(learner, "uniml_learner_spec")) {
    learner_obj <- learner$learner$clone(deep = TRUE)
    inherited_params <- learner$params
  } else if (inherits(learner, "Learner")) {
    learner_obj <- learner$clone(deep = TRUE)
    inherited_params <- NULL
  } else {
    .abort("`learner` must be an mlr3 learner or a `uniml_learner_spec` object.")
  }

  .set_probability_predict_type(learner_obj)
  if (!is.null(inherited_params) && length(inherited_params) > 0L) {
    .apply_learner_params(learner_obj, inherited_params)
  }
  if (!is.null(params)) {
    .apply_learner_params(learner_obj, params)
  }
  learner_obj
}

#' Train a final classification model
#'
#' Clones the supplied learner, applies optional named parameters, and trains
#' it on the provided training task. The task is not modified.
#'
#' @param task An mlr3 classification task.
#' @param learner An mlr3 learner or a `uniml_learner_spec` object.
#' @param params Optional named list of learner parameters. Values supplied
#'   here override parameters stored in `learner`.
#'
#' @return A trained mlr3 `Learner`.
#' @export
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   y = factor(rep(c("no", "yes"), 20)),
#'   x1 = stats::rnorm(40),
#'   x2 = stats::rnorm(40)
#' )
#' spec <- uniml_spec(task_type = "binary", outcome = "y", positive = "yes")
#' task <- make_task(data, spec)
#' train_model(task, mlr3::lrn("classif.rpart", predict_type = "prob"))
#' }
train_model <- function(task, learner, params = NULL) {
  if (!inherits(task, "TaskClassif")) {
    .abort("`task` must be an mlr3 classification task.")
  }
  learner_obj <- .training_learner(learner, params = params)
  learner_obj$train(task)
  learner_obj
}
