# mlr3 prediction bridge ------------------------------------------------------

#' Convert an mlr3 prediction into a `uniml_binary` object
#'
#' The plotting and evaluation helpers accept a `uniml_binary` object. This
#' converter wraps an [mlr3::PredictionClassif] so that a prediction produced by
#' a plain mlr3 learner can be plugged into the same functions as a workflow
#' result.
#'
#' @param x An [mlr3::PredictionClassif] with `predict_type = "prob"`.
#' @param positive Positive class label. Defaults to the last class level.
#'
#' @return A `uniml_binary` object accepted by the `uniml` plotting functions.
#' @export
#' @examples
#' data <- data.frame(
#'   y = factor(rep(c("no", "yes"), 20), levels = c("no", "yes")),
#'   x1 = stats::rnorm(40),
#'   x2 = stats::rnorm(40)
#' )
#' spec <- uniml_spec(task_type = "binary", outcome = "y", positive = "yes")
#' task <- make_task(data, spec)
#' learner <- mlr3::lrn("classif.rpart", predict_type = "prob")
#' learner$train(task)
#' binary <- as_uniml_binary(learner$predict(task))
#' names(binary)
as_uniml_binary <- function(x, positive = NULL) {
  if (!inherits(x, "PredictionClassif")) {
    .abort("`x` must be an mlr3 `PredictionClassif` object.")
  }
  prob <- x$prob
  if (is.null(prob) || ncol(prob) < 2L) {
    .abort("`x` must contain probabilities; set `predict_type = \"prob\"`.")
  }
  classes <- colnames(prob)
  label <- if (is.null(positive)) classes[[length(classes)]] else as.character(positive)
  if (!label %in% classes) {
    .abort(sprintf(
      "`positive` must be one of: %s.",
      paste(classes, collapse = ", ")
    ))
  }
  structure(
    list(
      prob = as.numeric(prob[, label]),
      truth = as.character(x$truth),
      positive = label
    ),
    class = c("uniml_binary", "uniml_prediction")
  )
}
