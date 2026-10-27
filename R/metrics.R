# L4 binary metrics ----------------------------------------------------------
#
# The metric layer is deliberately independent of mlr3 task objects. A
# probability vector plus a truth vector is enough to calculate a complete
# binary-classification summary, which also makes the functions convenient to
# use in CV and post-processing code.

.binary_metric_names <- function() {
  c(
    "auc", "pr_auc", "brier", "accuracy", "sensitivity", "specificity",
    "ppv", "npv", "f1", "f2", "youden", "plr", "nlr", "dor",
    "balanced_accuracy", "kappa", "calib_slope", "calib_intercept",
    "calib_large",
    # Binned calibration, reported so that the binary and multiclass paths expose
    # the same numbers. The slope and intercept above are the better diagnostic;
    # ECE and MCE are the bin-dependent summaries that multiclass calibration has
    # to fall back on, and they are defined by the same `.binned_calibration()`.
    "ece", "mce"
  )
}

.positive_label <- function(truth, positive = NULL) {
  if (is.factor(truth)) {
    levels <- levels(truth)
    if (is.null(positive)) {
      positive <- levels[length(levels)]
    }
    positive <- as.character(positive)
    if (!positive %in% levels) {
      .abort("`positive` must match one of the levels in `truth`.")
    }
  } else if (is.logical(truth)) {
    if (is.null(positive)) {
      positive <- TRUE
    }
    if (!is.logical(positive) || length(positive) != 1L || is.na(positive)) {
      .abort("`positive` must be TRUE or FALSE for logical truth values.")
    }
  } else if (is.numeric(truth)) {
    values <- unique(truth[!is.na(truth)])
    if (length(values) == 0L) {
      .abort("`truth` must contain at least one non-missing value.")
    }
    if (is.null(positive)) {
      positive <- max(values)
    }
    if (!is.numeric(positive) || length(positive) != 1L || is.na(positive)) {
      .abort("`positive` must be a single non-missing numeric value.")
    }
  } else if (is.character(truth)) {
    values <- unique(truth[!is.na(truth)])
    if (length(values) == 0L) {
      .abort("`truth` must contain at least one non-missing value.")
    }
    if (is.null(positive)) {
      positive <- values[length(values)]
    }
    if (!is.character(positive) || length(positive) != 1L || is.na(positive)) {
      .abort("`positive` must be a single non-missing character value.")
    }
  } else {
    .abort("`truth` must be numeric, logical, character, or factor.")
  }
  positive
}

.binary_event_vector <- function(truth, positive = NULL) {
  positive <- .positive_label(truth, positive = positive)
  values <- rep(NA, length(truth))
  observed <- !is.na(truth)
  values[observed] <- as.character(truth[observed]) == as.character(positive)
  list(event = values, positive = positive)
}

.validate_probability_vector <- function(probs, name = "probs") {
  if (!is.numeric(probs) || !is.null(dim(probs))) {
    .abort(sprintf("`%s` must be a numeric vector.", name))
  }
  if (any(!is.na(probs) & (!is.finite(probs) | probs < 0 | probs > 1))) {
    .abort(sprintf("`%s` must contain probabilities in [0, 1].", name))
  }
  invisible(TRUE)
}

.clean_binary_inputs <- function(probs, truth, positive = NULL, name = "probs") {
  .validate_probability_vector(probs, name = name)
  if (length(probs) != length(truth)) {
    .abort(sprintf("`%s` and `truth` must have the same length.", name))
  }
  labels <- .binary_event_vector(truth, positive = positive)
  keep <- !is.na(probs) & !is.na(truth)
  list(
    prob = as.numeric(probs[keep]),
    event = as.numeric(labels$event[keep]),
    positive = labels$positive,
    n = sum(keep)
  )
}

.safe_metric <- function(fun, truth, prob, positive, ...) {
  if (length(unique(truth[!is.na(truth)])) < 2L) {
    return(NA_real_)
  }
  tryCatch(
    as.numeric(fun(
      truth = truth,
      prob = prob,
      positive = positive,
      ...
    )),
    error = function(e) NA_real_
  )
}

.metric_list <- function(values, metrics) {
  if (identical(metrics, "all")) {
    return(values)
  }
  .assert_character_vector(metrics, "metrics")
  unknown <- setdiff(metrics, names(values))
  if (length(unknown) > 0L) {
    .abort(sprintf(
      "`metrics` contains unsupported values: %s.",
      paste(unknown, collapse = ", ")
    ))
  }
  values[metrics]
}

# Recycle a scalar safely when a metric has an undefined denominator.
.ratio_or_na <- function(numerator, denominator) {
  if (length(denominator) != 1L || is.na(denominator) || denominator == 0 ||
      !is.finite(denominator)) {
    return(NA_real_)
  }
  as.numeric(numerator / denominator)
}

.calc_binary_metrics <- function(probs, truth, threshold = NULL,
                                 positive = NULL, metrics = "all",
                                 threshold_method = "fixed",
                                 cost = c(fp = 1, fn = 1),
                                 fixed_threshold = 0.5) {
  if (!is.null(threshold)) {
    .assert_scalar_number(threshold, "threshold", min = 0, max = 1)
  }
  .assert_choice(
    threshold_method,
    "threshold_method",
    c("youden", "cost", "fixed")
  )
  .assert_cost(cost)
  .assert_scalar_number(fixed_threshold, "fixed_threshold", min = 0, max = 1)

  cleaned <- .clean_binary_inputs(probs, truth, positive = positive)
  event <- cleaned$event
  prob <- cleaned$prob
  n <- cleaned$n
  has_both_classes <- length(unique(event)) == 2L

  if (is.null(threshold)) {
    threshold <- find_threshold(
      prob,
      event,
      method = threshold_method,
      cost = cost,
      fixed_threshold = fixed_threshold,
      positive = 1
    )
  }
  if (!is.finite(threshold)) {
    threshold <- NA_real_
  }

  if (is.na(threshold)) {
    predicted <- rep(NA_real_, length(prob))
  } else {
    predicted <- as.numeric(prob >= threshold)
  }

  tp <- if (any(!is.na(predicted))) {
    sum(predicted == 1 & event == 1, na.rm = TRUE)
  } else {
    0
  }
  tn <- if (any(!is.na(predicted))) {
    sum(predicted == 0 & event == 0, na.rm = TRUE)
  } else {
    0
  }
  fp <- if (any(!is.na(predicted))) {
    sum(predicted == 1 & event == 0, na.rm = TRUE)
  } else {
    0
  }
  fn <- if (any(!is.na(predicted))) {
    sum(predicted == 0 & event == 1, na.rm = TRUE)
  } else {
    0
  }

  accuracy <- .ratio_or_na(tp + tn, tp + tn + fp + fn)
  # The recalibration is two `glm()` fits; skip them unless they were asked for,
  # so a bootstrap that only wants AUC does not pay for them.
  wants_calibration <- identical(metrics, "all") ||
    any(c("calib_slope", "calib_intercept", "calib_large") %in% metrics)
  calibration <- if (wants_calibration) {
    .calibration_fit(prob, event)
  } else {
    list(intercept = NA_real_, slope = NA_real_, large = NA_real_)
  }
  # The same binning rule as the multiclass ECE. The two numbers are not the
  # same quantity, though: here the probability of the event is binned, because a
  # binary outcome has one, while a multiclass model only has a confidence in the
  # class it predicted. `ece_classwise` is what makes the two comparable — for
  # the positive column it is this very number.
  wants_binned <- identical(metrics, "all") || any(c("ece", "mce") %in% metrics)
  binned <- if (wants_binned && n > 0L) {
    .binned_calibration(prob, event, bins = 10L)
  } else {
    list(ece = NA_real_, mce = NA_real_)
  }
  sensitivity <- .ratio_or_na(tp, tp + fn)
  specificity <- .ratio_or_na(tn, tn + fp)
  ppv <- .ratio_or_na(tp, tp + fp)
  npv <- .ratio_or_na(tn, tn + fn)
  f1 <- if (is.na(sensitivity) || is.na(ppv) ||
            (sensitivity + ppv) == 0) {
    NA_real_
  } else {
    2 * sensitivity * ppv / (sensitivity + ppv)
  }
  f2 <- if (is.na(sensitivity) || is.na(ppv) ||
            (4 * ppv + sensitivity) == 0) {
    NA_real_
  } else {
    5 * ppv * sensitivity / (4 * ppv + sensitivity)
  }
  youden <- if (is.na(sensitivity) || is.na(specificity)) {
    NA_real_
  } else {
    sensitivity + specificity - 1
  }
  plr <- if (is.na(sensitivity) || is.na(specificity) || specificity >= 1) {
    NA_real_
  } else {
    sensitivity / (1 - specificity)
  }
  nlr <- if (is.na(sensitivity) || is.na(specificity) || specificity == 0) {
    NA_real_
  } else {
    (1 - sensitivity) / specificity
  }
  dor <- if (is.na(plr) || is.na(nlr) || nlr == 0) {
    NA_real_
  } else {
    plr / nlr
  }
  balanced_accuracy <- if (is.na(sensitivity) || is.na(specificity)) {
    NA_real_
  } else {
    (sensitivity + specificity) / 2
  }

  p0 <- accuracy
  total <- tp + tn + fp + fn
  pe <- .ratio_or_na(
    (tp + fp) * (tp + fn) + (tn + fp) * (tn + fn),
    total^2
  )
  kappa <- if (is.na(p0) || is.na(pe) || pe >= 1) {
    NA_real_
  } else {
    (p0 - pe) / (1 - pe)
  }

  values <- list(
    auc = .safe_metric(      mlr3measures::auc,
      factor(event, levels = c(0, 1)),
      prob,
      positive = "1"
    ),
    pr_auc = .safe_metric(
      mlr3measures::prauc,
      factor(event, levels = c(0, 1)),
      prob,
      positive = "1"
    ),
    brier = if (n == 0L) NA_real_ else mean((event - prob)^2),
    accuracy = accuracy,
    sensitivity = sensitivity,
    specificity = specificity,
    ppv = ppv,
    npv = npv,
    f1 = f1,
    f2 = f2,
    youden = youden,
    plr = plr,
    nlr = nlr,
    dor = dor,
    balanced_accuracy = balanced_accuracy,
    kappa = kappa,
    calib_slope = calibration$slope,
    calib_intercept = calibration$intercept,
    calib_large = calibration$large,
    ece = binned$ece,
    mce = binned$mce,
    tp = as.integer(tp),
    tn = as.integer(tn),
    fp = as.integer(fp),
    fn = as.integer(fn),
    threshold = threshold,
    n_samples = as.integer(n)
  )
  .metric_list(values, metrics)
}

.calc_metrics_uniml_binary <- function(x, truth = NULL, threshold = NULL,
                                       positive = NULL, metrics = "all",
                                       threshold_method = "fixed",
                                       cost = c(fp = 1, fn = 1),
                                       fixed_threshold = 0.5, ...) {
  if (!is.list(x) || is.null(x$prob)) {
    .abort("A `uniml_binary` prediction must contain a `prob` component.")
  }
  if (is.null(truth)) {
    truth <- x$truth
  }
  if (is.null(truth)) {
    .abort("`truth` is required for binary metric calculation.")
  }
  if (is.null(positive)) {
    positive <- x$positive
  }
  .calc_binary_metrics(
    probs = x$prob,
    truth = truth,
    threshold = threshold,
    positive = positive,
    metrics = metrics,
    threshold_method = threshold_method,
    cost = cost,
    fixed_threshold = fixed_threshold
  )
}

#' Calibration intercept and slope
#'
#' The two numbers a clinical reader looks for after the calibration plot. Both
#' come from one logistic recalibration of the predictions,
#' `logit(observed) = intercept + slope * logit(predicted)`, which is the
#' convention of Steyerberg and of van Calster's calibration guidance: an ideal
#' model has slope 1, and the intercept is 0. Calibration-in-the-large is reported
#' separately, as the intercept of the same model with the slope fixed at 1, so a
#' systematic over- or under-prediction is not hidden inside a slope away from 1.
#'
#' @param probs Predicted probabilities of the positive class.
#' @param labels Observed outcomes.
#' @param positive Value of `labels` that means "event".
#' @param level Confidence level for the interval, or `NULL` for no interval.
#' @param R Bootstrap resamples used for the interval.
#' @param seed Random seed for the bootstrap.
#'
#' @return A one-row data frame with `calib_intercept`, `calib_slope`,
#'   `calib_large`, their `lower`/`upper` bounds when `level` is not `NULL`, and
#'   `n`.
#' @references
#' Steyerberg EW (2019). *Clinical Prediction Models: A Practical Approach to
#' Development, Validation, and Updating*, 2nd edition. Springer.
#' doi:10.1007/978-3-030-16399-0
#'
#' Van Calster B, McLernon DJ, van Smeden M, Wynants L, Steyerberg EW (2019).
#' Calibration: the Achilles heel of predictive analytics. *BMC Medicine*, 17,
#' 230. doi:10.1186/s12916-019-1466-7
#'
#' Cox DR (1958). Two further applications of a model for binary regression.
#' *Biometrika*, 45(3-4), 562-565.
#' @export
#' @examples
#' set.seed(1)
#' probs <- stats::runif(200)
#' labels <- stats::rbinom(200, 1, probs)
#' calibration_metrics(probs, labels, level = NULL)
calibration_metrics <- function(probs, labels, positive = NULL, level = 0.95,
                                R = 500L, seed = 2026L) {
  if (is.matrix(probs)) {
    # A multinomial model has no single slope or intercept, so this function does
    # not pretend to give one. The multiclass numbers live with the other
    # multiclass metrics, where they also get bootstrap intervals.
    .abort(paste(
      "`calibration_metrics()` reports a binary recalibration slope and",
      "intercept, which a multiclass probability matrix does not have. Use",
      "`calc_metrics()` for the top-label ECE/MCE and the class-wise ECE, or",
      "`multiclass_calibration()` for the per-class numbers and the reliability",
      "tables; `calc_ci()` gives the intervals."
    ))
  }
  cleaned <- .clean_binary_inputs(probs, labels, positive = positive)
  event <- cleaned$event
  prob <- cleaned$prob
  n <- cleaned$n
  point <- .calibration_fit(prob, event)
  out <- data.frame(
    calib_intercept = point$intercept,
    calib_slope = point$slope,
    calib_large = point$large,
    n = as.integer(n),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  if (is.null(level)) {
    return(out)
  }
  .assert_scalar_number(level, "level", min = 0.5, max = 0.999)
  .assert_scalar_number(R, "R", integer = TRUE, min = 10)
  .assert_scalar_number(seed, "seed", integer = TRUE, min = 1)
  set.seed(as.integer(seed))
  values <- matrix(NA_real_, nrow = as.integer(R), ncol = 3L)
  for (index in seq_len(as.integer(R))) {
    sample_index <- sample.int(n, n, replace = TRUE)
    fit <- .calibration_fit(prob[sample_index], event[sample_index])
    values[index, ] <- c(fit$intercept, fit$slope, fit$large)
  }
  alpha <- (1 - level) / 2
  bounds <- apply(values, 2L, function(column) {
    if (all(is.na(column))) {
      return(c(NA_real_, NA_real_))
    }
    stats::quantile(column, probs = c(alpha, 1 - alpha), na.rm = TRUE,
                    names = FALSE)
  })
  out$intercept_lower <- bounds[1L, 1L]
  out$intercept_upper <- bounds[2L, 1L]
  out$slope_lower <- bounds[1L, 2L]
  out$slope_upper <- bounds[2L, 2L]
  out$large_lower <- bounds[1L, 3L]
  out$large_upper <- bounds[2L, 3L]
  out
}

# One logistic recalibration, plus the slope-fixed intercept. A sample that is too
# small, has too few events, or separates the outcome returns NA rather than a
# number nobody should trust: with separation the slope runs off to infinity and
# the interval would be meaningless.
.calibration_fit <- function(prob, event) {
  empty <- list(intercept = NA_real_, slope = NA_real_, large = NA_real_)
  if (length(prob) < 20L || sum(event == 1L) < 5L || sum(event == 0L) < 5L) {
    return(empty)
  }
  logit <- .safe_logit(prob)
  fit <- tryCatch(
    suppressWarnings(stats::glm(event ~ logit, family = stats::binomial())),
    error = function(e) NULL
  )
  if (is.null(fit) || !fit$converged) {
    return(empty)
  }
  coefficients <- stats::coef(fit)
  if (length(coefficients) < 2L || anyNA(coefficients)) {
    return(empty)
  }
  fitted <- stats::fitted(fit)
  if (any(fitted <= 1e-08) || any(fitted >= 1 - 1e-08)) {
    return(empty)
  }
  large <- tryCatch(
    {
      offset_fit <- suppressWarnings(stats::glm(
        event ~ 1,
        offset = logit,
        family = stats::binomial()
      ))
      if (offset_fit$converged) unname(stats::coef(offset_fit)[[1L]]) else NA_real_
    },
    error = function(e) NA_real_
  )
  list(
    intercept = unname(coefficients[[1L]]),
    slope = unname(coefficients[[2L]]),
    large = large
  )
}

#' Calculate binary classification metrics
#'
#' Calculates the complete binary metric set used by `uniml`. `x` can be a
#' numeric probability vector, an mlr3 `PredictionClassif`, or a
#' `uniml_binary` prediction object with `prob`, `truth`, and optional
#' `positive` components.
#'
#' @param x A probability vector, mlr3 prediction, or `uniml_binary` object.
#' @param truth Optional binary truth vector.
#' @param threshold Optional fixed classification threshold.
#' @param positive Positive class label. For an mlr3 `PredictionClassif` the
#'   default is the positive class of the task the prediction was made for,
#'   falling back to the first probability column when the prediction does not
#'   carry its task.
#' @param metrics `"all"` or a character vector of metric names.
#' @param probability Which probability column of a prediction table to score:
#'   `"raw"` or `"calibrated"`.
#' @param threshold_method Threshold method used when `threshold` is `NULL`.
#' @param cost Named false-positive and false-negative cost vector.
#' @param fixed_threshold Threshold used by the `"fixed"` method.
#' @param ... Reserved for method-specific arguments.
#'
#' @return A named list of metrics and confusion-matrix counts.
#' @export
#' @rdname calc_metrics
#' @examples
#' calc_metrics(
#'   c(0.2, 0.8, 0.6, 0.3),
#'   c(0, 1, 1, 0),
#'   threshold = 0.5
#' )
calc_metrics.uniml_binary <- function(x, truth = NULL, threshold = NULL,
                                      positive = NULL, metrics = "all",
                                      threshold_method = "fixed",
                                      cost = c(fp = 1, fn = 1),
                                      fixed_threshold = 0.5, ...) {
  .calc_metrics_uniml_binary(
    x, truth = truth, threshold = threshold, positive = positive,
    metrics = metrics, threshold_method = threshold_method, cost = cost,
    fixed_threshold = fixed_threshold, ...
  )
}

#' @rdname calc_metrics
#' @export
calc_metrics.data.frame <- function(x, truth = NULL, threshold = NULL,
                                    positive = NULL, metrics = "all",
                                    probability = c("raw", "calibrated"),
                                    threshold_method = "fixed",
                                    cost = c(fp = 1, fn = 1),
                                    fixed_threshold = 0.5, ...) {
  probability <- match.arg(probability)
  if (!"prob" %in% names(x)) {
    .abort("`x` must contain a `prob` column.")
  }
  prob <- if (identical(probability, "calibrated")) {
    if (!"calibrated_prob" %in% names(x)) {
      .abort("`x` has no `calibrated_prob` column.")
    }
    x$calibrated_prob
  } else {
    x$prob
  }
  if (is.null(truth)) {
    if (!"truth" %in% names(x)) {
      .abort("`truth` is required when `x` has no `truth` column.")
    }
    truth <- x$truth
  }
  if (is.null(positive) && "positive" %in% names(x)) {
    positive <- x$positive[!is.na(x$positive)][[1L]]
  }
  .calc_binary_metrics(
    probs = prob,
    truth = truth,
    threshold = threshold,
    positive = positive,
    metrics = metrics,
    threshold_method = threshold_method,
    cost = cost,
    fixed_threshold = fixed_threshold
  )
}

#' @rdname calc_metrics
#' @export
calc_metrics.numeric <- function(x, truth = NULL, threshold = NULL,                                 positive = NULL, metrics = "all",
                                 threshold_method = "fixed",
                                 cost = c(fp = 1, fn = 1),
                                 fixed_threshold = 0.5, ...) {
  if (is.null(truth)) {
    .abort("`truth` is required for numeric probability input.")
  }
  .calc_binary_metrics(
    probs = x,
    truth = truth,
    threshold = threshold,
    positive = positive,
    metrics = metrics,
    threshold_method = threshold_method,
    cost = cost,
    fixed_threshold = fixed_threshold
  )
}

#' @rdname calc_metrics
#' @export
calc_metrics.PredictionClassif <- function(x, threshold = NULL,
                                           positive = NULL,
                                           metrics = "all",
                                           threshold_method = "fixed",
                                           cost = c(fp = 1, fn = 1),
                                           fixed_threshold = 0.5, ...) {
  if (is.null(x$prob) || ncol(x$prob) == 0L) {
    .abort("The prediction object does not contain probabilities.")
  }
  if (ncol(x$prob) > 2L) {
    # Three or more classes cannot be scored as one positive class against the
    # rest without choosing a convention, so they get their own metric set.
    return(.calc_multiclass_metrics(
      x$prob,
      as.character(x$truth),
      metrics = metrics
    ))
  }
  positive <- .prediction_positive_class(x, positive = positive)
  prob <- x$prob[, positive]
  .calc_binary_metrics(
    probs = prob,
    truth = x$truth,
    threshold = threshold,
    positive = positive,
    metrics = metrics,
    threshold_method = threshold_method,
    cost = cost,
    fixed_threshold = fixed_threshold
  )
}

#' Find a binary classification threshold
#'
#' @param probs Numeric predicted probabilities.
#' @param labels Binary truth labels.
#' @param method One of `"youden"`, `"cost"`, or `"fixed"`.
#' @param cost Named false-positive and false-negative costs.
#' @param fixed_threshold Fixed threshold used by the `"fixed"` method.
#' @param positive Positive class label.
#'
#' @return A single numeric threshold, or `NA_real_` when it cannot be
#'   estimated from the supplied classes.
#' @references
#' Youden WJ (1950). Index for rating diagnostic tests. *Cancer*, 3(1), 32-35.
#' @export
#' @examples
#' find_threshold(c(0.1, 0.4, 0.6, 0.9), c(0, 0, 1, 1), method = "youden")
find_threshold <- function(probs, labels, method = c("youden", "cost", "fixed"),
                           cost = c(fp = 1, fn = 1), fixed_threshold = 0.5,
                           positive = NULL) {
  method <- match.arg(method)
  .assert_cost(cost)
  .assert_scalar_number(fixed_threshold, "fixed_threshold", min = 0, max = 1)
  if (identical(method, "fixed")) {
    return(as.numeric(fixed_threshold))
  }

  cleaned <- .clean_binary_inputs(probs, labels, positive = positive)
  if (cleaned$n == 0L || length(unique(cleaned$event)) < 2L) {
    return(NA_real_)
  }
  prob <- cleaned$prob
  event <- cleaned$event
  thresholds <- sort(unique(c(0, prob, 1)))
  predicted <- outer(prob, thresholds, FUN = ">=")
  tp <- colSums(predicted & event == 1)
  tn <- colSums(!predicted & event == 0)
  fp <- colSums(predicted & event == 0)
  fn <- colSums(!predicted & event == 1)

  if (identical(method, "youden")) {
    sens <- tp / pmax(tp + fn, 1)
    spec <- tn / pmax(tn + fp, 1)
    score <- sens + spec - 1
    best <- which.max(score)
  } else {
    score <- fp * cost[["fp"]] + fn * cost[["fn"]]
    best <- which.min(score)
  }
  as.numeric(thresholds[best])
}

#' Calculate continuous or categorical NRI and IDI
#'
#' @param pred_new New-model predicted probabilities.
#' @param pred_reference Reference-model predicted probabilities.
#' @param truth Binary truth labels.
#' @param method `"continuous"` or `"categorical"` NRI.
#' @param threshold_new,threshold_reference Thresholds used for categorical NRI.
#' @param positive Positive class label.
#'
#' @return A list with NRI, IDI, group-specific NRI components, and sample
#'   counts. Metrics are `NA` when the required event or non-event group is
#'   absent.
#' @references
#' Pencina MJ, D'Agostino RB Sr, D'Agostino RB Jr, Vasan RS (2008). Evaluating
#' the added predictive ability of a new marker: from area under the ROC curve
#' to reclassification and beyond. *Statistics in Medicine*, 27(2), 157-172.
#' doi:10.1002/sim.2929
#'
#' Pencina MJ, D'Agostino RB Sr, Steyerberg EW (2011). Extensions of net
#' reclassification improvement calculations to measure usefulness of new
#' biomarkers. *Statistics in Medicine*, 30(1), 11-21. doi:10.1002/sim.4067
#' @export
#' @examples
#' calc_nri_idi(
#'   pred_new = c(0.2, 0.7, 0.8, 0.1),
#'   pred_reference = c(0.3, 0.6, 0.7, 0.2),
#'   truth = c(0, 1, 1, 0)
#' )
calc_nri_idi <- function(pred_new, pred_reference, truth,
                         method = c("continuous", "categorical"),
                         threshold_new = 0.5,
                         threshold_reference = 0.5,
                         positive = NULL) {
  method <- match.arg(method)
  .validate_probability_vector(pred_new, "pred_new")
  .validate_probability_vector(pred_reference, "pred_reference")
  if (length(pred_new) != length(pred_reference) ||
      length(pred_new) != length(truth)) {
    .abort("`pred_new`, `pred_reference`, and `truth` must have equal lengths.")
  }
  .assert_scalar_number(threshold_new, "threshold_new", min = 0, max = 1)
  .assert_scalar_number(
    threshold_reference,
    "threshold_reference",
    min = 0,
    max = 1
  )

  labels <- .binary_event_vector(truth, positive = positive)
  keep <- !is.na(pred_new) & !is.na(pred_reference) & !is.na(truth)
  new_prob <- as.numeric(pred_new[keep])
  ref_prob <- as.numeric(pred_reference[keep])
  event <- labels$event[keep]
  n <- length(event)
  n_events <- sum(event)
  n_nonevents <- sum(!event)

  if (n == 0L || n_events == 0L || n_nonevents == 0L) {
    return(list(
      nri = NA_real_,
      nri_events = NA_real_,
      nri_nonevents = NA_real_,
      idi = NA_real_,
      new_discrimination_slope = NA_real_,
      reference_discrimination_slope = NA_real_,
      method = method,
      n = as.integer(n),
      n_events = as.integer(n_events),
      n_nonevents = as.integer(n_nonevents)
    ))
  }

  if (identical(method, "continuous")) {
    event_up <- mean(new_prob[event] > ref_prob[event])
    event_down <- mean(new_prob[event] < ref_prob[event])
    non_event_down <- mean(new_prob[!event] < ref_prob[!event])
    non_event_up <- mean(new_prob[!event] > ref_prob[!event])
    nri_events <- event_up - event_down
    nri_nonevents <- non_event_down - non_event_up
  } else {
    new_class <- as.numeric(new_prob >= threshold_new)
    ref_class <- as.numeric(ref_prob >= threshold_reference)
    event_up <- mean(new_class[event] == 1 & ref_class[event] == 0)
    event_down <- mean(new_class[event] == 0 & ref_class[event] == 1)
    non_event_up <- mean(new_class[!event] == 1 & ref_class[!event] == 0)
    non_event_down <- mean(new_class[!event] == 0 & ref_class[!event] == 1)
    nri_events <- event_up - event_down
    nri_nonevents <- non_event_down - non_event_up
  }

  new_slope <- mean(new_prob[event]) - mean(new_prob[!event])
  ref_slope <- mean(ref_prob[event]) - mean(ref_prob[!event])
  idi <- new_slope - ref_slope

  list(
    nri = nri_events + nri_nonevents,
    nri_events = nri_events,
    nri_nonevents = nri_nonevents,
    idi = idi,
    new_discrimination_slope = new_slope,
    reference_discrimination_slope = ref_slope,
    method = method,
    n = as.integer(n),
    n_events = as.integer(n_events),
    n_nonevents = as.integer(n_nonevents)
  )
}
