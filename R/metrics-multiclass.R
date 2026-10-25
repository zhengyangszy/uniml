# L4 multiclass metrics -------------------------------------------------------
#
# Multiclass metrics are reported as one number per model where that is
# meaningful (accuracy, multi-class Brier, log loss, macro one-vs-rest AUC) plus
# a per-class table. The conventions are fixed here once so that a figure, a
# table, and a paper cannot disagree:
#
# * `auc` is the macro one-vs-rest AUC averaged over classes with equal weight
#   (`mlr3measures::mauc_au1u`), not a weighted or micro average.
# * `brier` is the multi-class Brier score, the mean over rows of the sum of
#   squared class-wise errors.
# * `sensitivity`, `specificity`, `ppv`, `npv`, and `f1` are macro averages of
#   the one-vs-rest values, so every class contributes equally regardless of how
#   rare it is.

.multiclass_metric_names <- function() {
  c(
    "auc", "accuracy", "balanced_accuracy", "kappa", "logloss", "brier",
    "sensitivity", "specificity", "ppv", "npv", "f1"
  )
}

.validate_probability_matrix <- function(probs, name = "probs") {
  if (!is.matrix(probs) || !is.numeric(probs)) {
    .abort(sprintf("`%s` must be a numeric matrix with one column per class.", name))
  }
  if (ncol(probs) < 2L) {
    .abort(sprintf("`%s` must have at least two class columns.", name))
  }
  if (is.null(colnames(probs)) || any(!nzchar(colnames(probs)))) {
    .abort(sprintf("`%s` must have non-empty column names for its classes.", name))
  }
  finite <- probs[!is.na(probs)]
  if (any(!is.finite(finite) | finite < 0 | finite > 1)) {
    .abort(sprintf("`%s` must contain probabilities in [0, 1].", name))
  }
  invisible(TRUE)
}

.clean_multiclass_inputs <- function(probs, truth, name = "probs") {
  .validate_probability_matrix(probs, name = name)
  if (length(truth) != nrow(probs)) {
    .abort(sprintf("`%s` and `truth` must have the same number of rows.", name))
  }
  classes <- colnames(probs)
  observed <- as.character(truth)
  unknown <- setdiff(unique(observed[!is.na(observed)]), classes)
  if (length(unknown) > 0L) {
    .abort(sprintf(
      "`truth` contains class(es) missing from `%s`: %s.",
      name,
      paste(unknown, collapse = ", ")
    ))
  }
  keep <- !is.na(truth) & stats::complete.cases(probs)
  prob <- probs[keep, , drop = FALSE]
  observed <- observed[keep]
  if (nrow(prob) > 0L && any(observed == "")) {
    .abort("`truth` must not contain empty class labels.")
  }
  totals <- rowSums(prob)
  if (any(abs(totals - 1) > 1e-6)) {
    .abort(sprintf(
      "The rows of `%s` must sum to 1; check that every class column is present.",
      name
    ))
  }
  list(prob = prob, truth = observed, classes = classes, n = nrow(prob))
}

.multiclass_confusion <- function(predicted, observed, classes) {
  counts <- table(
    factor(observed, levels = classes),
    factor(predicted, levels = classes)
  )
  out <- matrix(
    as.numeric(counts),
    nrow = length(classes),
    ncol = length(classes),
    dimnames = list(observed = classes, predicted = classes)
  )
  out
}

.multiclass_per_class <- function(confusion) {
  n <- sum(confusion)
  support <- rowSums(confusion)
  predicted <- colSums(confusion)
  rows <- lapply(seq_len(nrow(confusion)), function(i) {
    tp <- confusion[i, i]
    fn <- support[[i]] - tp
    fp <- predicted[[i]] - tp
    tn <- n - tp - fn - fp
    data.frame(
      class = rownames(confusion)[[i]],
      n = as.integer(support[[i]]),
      tp = as.integer(tp),
      fp = as.integer(fp),
      fn = as.integer(fn),
      tn = as.integer(tn),
      sensitivity = .ratio_or_na(tp, tp + fn),
      specificity = .ratio_or_na(tn, tn + fp),
      ppv = .ratio_or_na(tp, tp + fp),
      npv = .ratio_or_na(tn, tn + fn),
      f1 = if (is.na(tp / pmax(tp + fn, 1)) || is.na(tp / pmax(tp + fp, 1))) {
        NA_real_
      } else {
        .ratio_or_na(2 * tp, 2 * tp + fp + fn)
      },
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

.macro_mean <- function(values) {
  values <- as.numeric(values)
  values <- values[is.finite(values)]
  if (length(values) == 0L) {
    return(NA_real_)
  }
  mean(values)
}

.calc_multiclass_metrics <- function(probs, truth, metrics = "all") {
  cleaned <- .clean_multiclass_inputs(probs, truth)
  classes <- cleaned$classes
  prob <- cleaned$prob
  observed <- cleaned$truth
  n <- cleaned$n
  has_two <- length(unique(observed)) >= 2L

  if (n == 0L) {
    values <- list()
    for (name in .multiclass_metric_names()) {
      values[[name]] <- NA_real_
    }
    values$confusion <- .multiclass_confusion(character(), character(), classes)
    values$per_class <- NULL
    values$n_samples <- 0L
    values$n_classes <- as.integer(length(classes))
    return(.metric_list(values, metrics))
  }

  predicted <- classes[max.col(prob, ties.method = "first")]
  confusion <- .multiclass_confusion(predicted, observed, classes)
  per_class <- .multiclass_per_class(confusion)

  accuracy <- if (n == 0L) NA_real_ else sum(diag(confusion)) / n
  support <- rowSums(confusion)
  recall <- ifelse(support > 0L, diag(confusion) / support, NA_real_)
  balanced_accuracy <- .macro_mean(recall)
  chance <- sum(support * colSums(confusion)) / n^2
  kappa <- if (is.na(accuracy) || is.na(chance) || chance >= 1) {
    NA_real_
  } else {
    (accuracy - chance) / (1 - chance)
  }

  factor_truth <- factor(observed, levels = classes)
  values <- list(
    auc = .safe_metric(
      mlr3measures::mauc_au1u,
      factor_truth,
      prob,
      positive = NULL
    ),
    accuracy = accuracy,
    balanced_accuracy = balanced_accuracy,
    kappa = kappa,
    logloss = .safe_metric(
      mlr3measures::logloss,
      factor_truth,
      prob,
      positive = NULL
    ),
    brier = .safe_metric(
      mlr3measures::mbrier,
      factor_truth,
      prob,
      positive = NULL
    ),
    sensitivity = .macro_mean(per_class$sensitivity),
    specificity = .macro_mean(per_class$specificity),
    ppv = .macro_mean(per_class$ppv),
    npv = .macro_mean(per_class$npv),
    f1 = .macro_mean(per_class$f1),
    confusion = confusion,
    per_class = per_class,
    n_samples = as.integer(n),
    n_classes = as.integer(length(classes))
  )
  if (!has_two) {
    values$auc <- NA_real_
  }
  .metric_list(values, metrics)
}

.calc_metrics_uniml_multiclass <- function(x, truth = NULL, metrics = "all", ...) {
  if (!is.list(x) || is.null(x$prob)) {
    .abort("A `uniml_multiclass` prediction must contain a `prob` matrix.")
  }
  if (is.null(truth)) {
    truth <- x$truth
  }
  if (is.null(truth)) {
    .abort("`truth` is required for multiclass metric calculation.")
  }
  .calc_multiclass_metrics(x$prob, truth, metrics = metrics)
}

#' @rdname calc_metrics
#' @export
calc_metrics.uniml_multiclass <- function(x, truth = NULL, metrics = "all", ...) {
  .calc_metrics_uniml_multiclass(x, truth = truth, metrics = metrics, ...)
}

#' Convert an mlr3 prediction into a `uniml_multiclass` object
#'
#' The multiclass metric and plotting helpers accept a `uniml_multiclass`
#' object: a probability matrix with one column per class plus the observed
#' labels.
#'
#' @param x An [mlr3::PredictionClassif] with `predict_type = "prob"`, or a
#'   numeric probability matrix.
#' @param truth Observed class labels. Required when `x` is a matrix.
#'
#' @return A `uniml_multiclass` object with `prob`, `truth`, and `classes`.
#' @references
#' Hand DJ, Till RJ (2001). A simple generalisation of the area under the ROC
#' curve for multiple class classification problems. *Machine Learning*, 45,
#' 171-186. doi:10.1023/A:1010920819831
#' @export
#' @examples
#' prob <- rbind(
#'   c(a = 0.7, b = 0.2, c = 0.1),
#'   c(a = 0.2, b = 0.6, c = 0.2),
#'   c(a = 0.1, b = 0.3, c = 0.6)
#' )
#' multiclass <- as_uniml_multiclass(prob, truth = c("a", "b", "c"))
#' calc_metrics(multiclass)$accuracy
as_uniml_multiclass <- function(x, truth = NULL) {
  if (inherits(x, "PredictionClassif")) {
    prob <- x$prob
    if (is.null(prob) || ncol(prob) < 2L) {
      .abort("`x` must contain probabilities; set `predict_type = \"prob\"`.")
    }
    if (is.null(truth)) {
      truth <- as.character(x$truth)
    }
  } else if (is.matrix(x)) {
    prob <- x
    if (is.null(truth)) {
      .abort("`truth` is required when `x` is a probability matrix.")
    }
  } else {
    .abort("`x` must be an mlr3 `PredictionClassif` or a probability matrix.")
  }
  .validate_probability_matrix(prob, name = "prob")
  if (ncol(prob) < 3L) {
    .abort(
      paste(
        "`as_uniml_multiclass()` is for tasks with three or more classes;",
        "use `as_uniml_binary()` for a two-class outcome."
      )
    )
  }
  truth <- as.character(truth)
  structure(
    list(
      # The column names carry the class labels and must survive the coercion.
      prob = prob,
      truth = truth,
      classes = colnames(prob)
    ),
    class = c("uniml_multiclass", "uniml_prediction")
  )
}
