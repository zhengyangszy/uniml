# L5 multiclass calibration ---------------------------------------------------
#
# A multiclass model outputs a whole vector of probabilities per patient, so
# "calibration" has to be defined before it can be measured. Two conventions are
# reported here and both are needed; they answer different questions and a model
# can pass one and fail the other.
#
# * **Top-label** calibration (Guo et al. 2017): take the class with the largest
#   probability, call that probability the confidence, and ask whether a patient
#   whose confidence is 0.8 really belongs to the predicted class 80% of the
#   time. This is what a clinician does with a report that says "class X, 82%".
#   Its summary numbers are the expected calibration error (ECE) and the maximum
#   calibration error (MCE) over confidence bins.
# * **Class-wise** calibration (Kull et al. 2019): for every class separately,
#   treat "is this patient class k" as a binary outcome and compare the predicted
#   probability with the observed frequency. Its summary number is the class-wise
#   ECE, the unweighted mean of the per-class ECEs, so a rare class that is badly
#   calibrated cannot be hidden by three common classes that are fine.
#
# Top-label ECE ignores every non-maximal probability, which is why it is
# reported together with the class-wise ECE rather than instead of it. Neither is
# a proper scoring rule: the Brier score and the log loss in `calc_metrics()` are
# the proper scores, and they are what a model comparison should be ranked on.
#
# There is no single slope or intercept for a multinomial model: the univariate
# logistic recalibration of the binary case has no unique multivariate
# counterpart, and the multinomial analogue (temperature scaling, Guo et al.
# 2017) is a recalibration method rather than a diagnostic. What is reported
# instead is the level bias, `mean confidence - accuracy`, which is zero when the
# model is calibrated on average.

.multiclass_bins <- function(probability, bins) {
  pmin(pmax(floor(probability * bins) + 1L, 1L), bins)
}

# ECE and MCE for one probability column against one binary outcome. The bin
# definition matches `.binary_calibration_metrics()` exactly, so a binary outcome
# scored through the multiclass path cannot report a different ECE than the same
# outcome scored as binary.
.binned_calibration <- function(probability, event, bins = 10L) {
  bin <- .multiclass_bins(probability, bins)
  group_n <- tabulate(bin, nbins = bins)
  keep <- group_n > 0L
  if (!any(keep)) {
    return(list(
      ece = NA_real_, mce = NA_real_,
      table = data.frame(
        bin = integer(), n = integer(), predicted = numeric(),
        observed = numeric(), difference = numeric(), stringsAsFactors = FALSE
      )
    ))
  }
  predicted <- vapply(
    which(keep), function(i) mean(probability[bin == i]), numeric(1)
  )
  observed <- vapply(which(keep), function(i) mean(event[bin == i]), numeric(1))
  counts <- group_n[keep]
  difference <- predicted - observed
  list(
    ece = sum(counts * abs(difference)) / sum(counts),
    mce = max(abs(difference)),
    table = data.frame(
      bin = as.integer(which(keep)),
      n = as.integer(counts),
      predicted = predicted,
      observed = observed,
      difference = difference,
      stringsAsFactors = FALSE
    )
  )
}

#' Calibration of a multiclass model
#'
#' Measures how well a multiclass probability matrix is calibrated, under two
#' stated conventions, and returns the reliability tables that the plots use.
#'
#' **Top-label** calibration keeps the largest probability of each row as the
#' confidence and asks whether patients with that confidence really belong to the
#' predicted class. **Class-wise** calibration repeats a one-vs-rest comparison
#' for every class, so the classes are assessed separately instead of only where
#' the model is most confident. The class-wise ECE is the unweighted mean of the
#' per-class ECEs; the top-label ECE weights by how many patients fall in each
#' confidence bin.
#'
#' ECE and MCE are not proper scoring rules and must not be used to choose between
#' models. Use the Brier score or the log loss from `calc_metrics()` for that, and
#' read ECE/MCE as "how wrong is the stated probability when the model is
#' wrong".
#'
#' Note that a two-class matrix is *not* the same problem as a binary outcome with
#' `calc_metrics()`: there the binning is on the probability of the event, here it
#' is on the confidence in the class the model picked. The two agree only through
#' `per_class$ece` for the positive class, which is exactly the binary number.
#'
#' @param probs A numeric probability matrix with one column per class and one
#'   row per patient. The columns must be named and the rows must sum to 1.
#' @param labels Observed class labels, one per row of `probs`.
#' @param bins Number of equal-width probability bins.
#'
#' @return A list with
#' * `summary`: one row with `n`, `n_classes`, `accuracy`, `mean_confidence`,
#'   `level_bias`, `ece`, `mce`, `ece_classwise`, `bins`;
#' * `per_class`: one row per class with `n`, `prevalence`, `mean_predicted`,
#'   `ece`, and `mce`;
#' * `reliability`: a long table with `scope` (`"top_label"` or `"class"`),
#'   `class`, `bin`, `n`, `predicted`, `observed`, and `difference`, which is what
#'   `plot_calibration()` draws.
#' @references
#' Guo C, Pleiss G, Sun Y, Weinberger KQ (2017). On calibration of modern neural
#' networks. *Proceedings of the 34th International Conference on Machine
#' Learning*, 70, 1321-1330.
#'
#' Kull M, Perello-Nieto M, Kängsepp M, Silva Filho T, Song H, Flach P (2019).
#' Beyond temperature scaling: obtaining well-calibrated multi-class
#' probabilities with Dirichlet calibration. *Advances in Neural Information
#' Processing Systems*, 32.
#'
#' Van Calster B, McLernon DJ, van Smeden M, Wynants L, Steyerberg EW (2019).
#' Calibration: the Achilles heel of predictive analytics. *BMC Medicine*, 17,
#' 230. doi:10.1186/s12916-019-1466-7
#' @export
#' @examples
#' prob <- rbind(
#'   c(a = 0.7, b = 0.2, c = 0.1),
#'   c(a = 0.2, b = 0.6, c = 0.2),
#'   c(a = 0.1, b = 0.3, c = 0.6),
#'   c(a = 0.5, b = 0.3, c = 0.2)
#' )
#' calibration <- multiclass_calibration(prob, labels = c("a", "b", "c", "a"),
#'                                       bins = 2L)
#' calibration$summary
#' calibration$per_class
multiclass_calibration <- function(probs, labels, bins = 10L) {
  .assert_scalar_number(bins, "bins", integer = TRUE, min = 2)
  cleaned <- .clean_multiclass_inputs(probs, labels)
  classes <- cleaned$classes
  prob <- cleaned$prob
  observed <- cleaned$truth
  n <- cleaned$n
  empty <- list(
    summary = data.frame(
      n = as.integer(n),
      n_classes = as.integer(length(classes)),
      accuracy = NA_real_,
      mean_confidence = NA_real_,
      level_bias = NA_real_,
      ece = NA_real_,
      mce = NA_real_,
      ece_classwise = NA_real_,
      bins = as.integer(bins),
      stringsAsFactors = FALSE
    ),
    per_class = data.frame(
      class = classes, n = NA_integer_, prevalence = NA_real_,
      mean_predicted = NA_real_, ece = NA_real_, mce = NA_real_,
      stringsAsFactors = FALSE
    ),
    reliability = data.frame(
      scope = character(), class = character(), bin = integer(),
      n = integer(), predicted = numeric(), observed = numeric(),
      difference = numeric(), stringsAsFactors = FALSE
    )
  )
  if (n == 0L) {
    return(empty)
  }

  top_label <- classes[max.col(prob, ties.method = "first")]
  confidence <- apply(prob, 1L, max)
  correct <- as.integer(top_label == observed)
  top <- .binned_calibration(confidence, correct, bins = bins)
  top$table$scope <- "top_label"
  top$table$class <- NA_character_

  rows <- list()
  per_class <- lapply(seq_along(classes), function(index) {
    event <- as.integer(observed == classes[[index]])
    column <- prob[, index]
    fit <- .binned_calibration(column, event, bins = bins)
    if (nrow(fit$table) > 0L) {
      fit$table$scope <- "class"
      fit$table$class <- classes[[index]]
      rows[[length(rows) + 1L]] <<- fit$table
    }
    data.frame(
      class = classes[[index]],
      n = as.integer(sum(event)),
      prevalence = sum(event) / n,
      mean_predicted = mean(column),
      ece = fit$ece,
      mce = fit$mce,
      stringsAsFactors = FALSE
    )
  })
  per_class <- do.call(rbind, per_class)
  rownames(per_class) <- NULL

  reliability <- do.call(
    rbind,
    c(list(top$table), rows)
  )
  rownames(reliability) <- NULL
  reliability <- reliability[, c(
    "scope", "class", "bin", "n", "predicted", "observed", "difference"
  )]

  accuracy <- mean(correct)
  mean_confidence <- mean(confidence)
  summary <- data.frame(
    n = as.integer(n),
    n_classes = as.integer(length(classes)),
    accuracy = accuracy,
    mean_confidence = mean_confidence,
    level_bias = mean_confidence - accuracy,
    ece = top$ece,
    mce = top$mce,
    ece_classwise = .macro_mean(per_class$ece),
    bins = as.integer(bins),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  list(summary = summary, per_class = per_class, reliability = reliability)
}

# L5 multiclass recalibration -------------------------------------------------
#
# Temperature scaling (Guo et al. 2017) divides the logits by a single scalar
# fitted on held-out predictions, so the recalibrated probabilities are
# `softmax(log(p) / T)`. Because the division is monotone and applied to every
# class, the class the model ranks first never changes: the confusion matrix, the
# accuracy, and every threshold-based metric are untouched, and only the numbers
# attached to them move. That is the practical reason a one-parameter method is
# worth having - it cannot make the model worse at deciding and it fixes the
# probability it reports.
#
# Why not Dirichlet calibration (Kull et al. 2019), which fits `softmax(W log p +
# b)`? It is the stronger method when there is enough data, and it is the natural
# next step, but it estimates K^2 + K parameters, it can change the predicted
# class, and on the few hundred validation patients this package is aimed at
# those parameters are not identifiable. A single scalar is a decision this
# package can defend on that sample size; a K x K matrix is not.

# Log-probabilities with a floor, so a model that emits a hard 0 or 1 still has a
# finite likelihood and the scalar can be estimated. The floor is the same one
# `uniml:::.safe_logit()` uses for the binary calibrators.
.multiclass_log_probabilities <- function(prob) {
  log(pmin(pmax(prob, 1e-12), 1))
}

.softmax_rows <- function(log_values) {
  shifted <- log_values - apply(log_values, 1L, max)
  weights <- exp(shifted)
  weights / rowSums(weights)
}

# Negative log-likelihood of the observed labels under `softmax(log(p) / T)`.
.temperature_nll <- function(log_prob, index, temperature) {
  scaled <- .softmax_rows(log_prob / temperature)
  picked <- scaled[cbind(seq_along(index), index)]
  -mean(log(pmin(pmax(picked, 1e-12), 1)))
}

# A one-parameter fit: minimise the log loss over log T on a bounded interval, then
# keep the better of the optimum and T = 1. Temperature scaling must not be able
# to make the model worse than leaving it alone, and near T = 1 the two are
# indistinguishable, which is exactly what "already calibrated" should look like.
.fit_temperature <- function(log_prob, index) {
  before <- .temperature_nll(log_prob, index, 1)
  objective <- function(log_temperature) {
    .temperature_nll(log_prob, index, exp(log_temperature))
  }
  fitted <- tryCatch(
    stats::optimize(objective, interval = c(-2.5, 2.5), tol = 1e-6),
    error = function(e) NULL
  )
  if (is.null(fitted) || !is.finite(fitted$objective)) {
    return(list(
      temperature = 1,
      nll_before = before,
      nll_after = before,
      converged = FALSE
    ))
  }
  temperature <- exp(fitted$minimum)
  after <- fitted$objective
  if (after >= before) {
    temperature <- 1
    after <- before
  }
  list(
    temperature = temperature,
    nll_before = before,
    nll_after = after,
    converged = TRUE
  )
}

#' @rdname calibrate_probabilities
#' @export
calibrate_probabilities.matrix <- function(x, labels = NULL, ...) {
  if (ncol(x) < 3L) {
    .abort(
      paste(
        "A matrix with fewer than three class columns is a binary problem;",
        "pass the positive-class probability vector, or an `uniml_binary`",
        "object, so that `method = \"platt\"` or `\"isotonic\"` applies."
      )
    )
  }
  if (is.null(labels)) {
    .abort("`labels` is required for a probability matrix.")
  }
  calibrate_probabilities(
    as_uniml_multiclass(x, truth = labels),
    labels = labels,
    ...
  )
}

.apply_temperature <- function(prob, temperature) {
  if (isTRUE(all.equal(temperature, 1))) {
    return(prob)
  }
  scaled <- .softmax_rows(.multiclass_log_probabilities(prob) / temperature)
  dimnames(scaled) <- dimnames(prob)
  scaled
}

# The before/after table a recalibration has to be judged on: the proper scores
# it is fitted against, and the calibration summaries it is supposed to improve.
.multiclass_calibration_table <- function(prob, observed, bins = 10L) {
  classes <- colnames(prob)
  calibration <- multiclass_calibration(prob, observed, bins = bins)$summary
  index <- match(observed, classes)
  onehot <- matrix(
    0,
    nrow = nrow(prob),
    ncol = length(classes),
    dimnames = dimnames(prob)
  )
  onehot[cbind(seq_len(nrow(prob)), index)] <- 1
  clamped <- pmin(pmax(prob, 1e-12), 1)
  data.frame(
    n = calibration$n,
    n_classes = calibration$n_classes,
    accuracy = calibration$accuracy,
    level_bias = calibration$level_bias,
    ece = calibration$ece,
    mce = calibration$mce,
    ece_classwise = calibration$ece_classwise,
    brier = mean(rowSums((onehot - prob)^2)),
    logloss = -mean(log(clamped[cbind(seq_len(nrow(prob)), index)])),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

# Cross-fitting for the multiclass case: each fold is calibrated by a temperature
# estimated on the other folds, so the returned probabilities are honest even on
# the data the model was trained on.
.crossfit_temperature <- function(prob, observed, folds, seed) {
  fold_id <- .calibration_fold_ids(observed, folds, seed)
  calibrated <- prob
  fold_temperatures <- numeric(max(fold_id))
  failed <- FALSE
  log_prob <- .multiclass_log_probabilities(prob)
  classes <- colnames(prob)
  index <- match(observed, classes)
  for (fold in seq_along(fold_temperatures)) {
    train <- fold_id != fold
    if (sum(train) < 2L || length(unique(observed[train])) < 2L) {
      failed <- TRUE
      fold_temperatures[[fold]] <- NA_real_
      next
    }
    fit <- .fit_temperature(log_prob[train, , drop = FALSE], index[train])
    fold_temperatures[[fold]] <- fit$temperature
    calibrated[!train, ] <- .apply_temperature(
      prob[!train, , drop = FALSE],
      fit$temperature
    )
  }
  list(
    calibrated = calibrated,
    fold_temperatures = fold_temperatures,
    failed = failed
  )
}

#' Calibrate multiclass probabilities by temperature scaling
#'
#' Fits the single scalar `T` that makes a multiclass model's reported
#' probabilities match how often it is right. The recalibrated probabilities are
#' `softmax(log(p) / T)`: `T > 1` softens an over-confident model, `T < 1`
#' sharpens an under-confident one, and `T = 1` leaves the predictions alone.
#'
#' Because the division is monotone and is applied to every class in the same way,
#' temperature scaling **cannot change which class the model ranks first**. The
#' confusion matrix, the accuracy, and every threshold-based metric come out
#' identical; only the probabilities attached to them move. This is the reason a
#' one-parameter method is worth having: it cannot make the model worse at
#' deciding, only better at reporting.
#'
#' `T` is fitted by minimising the log loss on the observed labels, and `T = 1` is
#' kept whenever it is at least as good, so the fitted scalar never degrades the
#' probabilities it was fitted on. The returned `calibrated` matrix comes from
#' cross-fitting (each fold's temperature is estimated on the other folds), so the
#' before/after comparison is not measured on the data the scalar was fitted on.
#' That honesty has a cost: on a model that was already calibrated, the
#' cross-fitted log loss can come out a hair worse than the raw probabilities,
#' because each fold's scalar is a noisy estimate of 1. The guarantee is about the
#' scalar, not about the fold-to-fold noise.
#'
#' Dirichlet calibration (Kull et al. 2019) is the stronger method when there are
#' enough patients, but it estimates `K^2 + K` parameters and can change the
#' predicted class; on the few hundred validation patients this package targets
#' those parameters are not identifiable. Temperature scaling is the part of the
#' idea that a sample of that size can support.
#'
#' @param x A `uniml_multiclass` prediction, or a probability matrix with
#'   `labels` supplied.
#' @param labels Observed class labels. Required when `x` is a matrix.
#' @param new_probs Optional probability matrix to recalibrate with the scalar
#'   fitted on `x` (the validation-to-test handover). Its columns must be the same
#'   classes, in the same order.
#' @param new_labels Optional labels for `new_probs`, used to report the
#'   before/after table on that set.
#' @param method `"temperature"` or `"none"`.
#' @param folds Number of cross-fitting folds.
#' @param seed Random seed for the cross-fitting.
#' @param bins Number of reliability bins used in the before/after table.
#' @param ... Reserved for method-specific arguments.
#'
#' @return A `uniml_calibration` object with `method`, `calibrated` (a matrix),
#'   `raw`, `parameters` (including `fitted$temperature`), `metrics_before`,
#'   `metrics_after`, `failed`, `message`, and `n`.
#' @references
#' Guo C, Pleiss G, Sun Y, Weinberger KQ (2017). On calibration of modern neural
#' networks. *Proceedings of the 34th International Conference on Machine
#' Learning*, 70, 1321-1330.
#'
#' Kull M, Perello-Nieto M, Kängsepp M, Silva Filho T, Song H, Flach P (2019).
#' Beyond temperature scaling: obtaining well-calibrated multi-class
#' probabilities with Dirichlet calibration. *Advances in Neural Information
#' Processing Systems*, 32.
#'
#' Van Calster B, McLernon DJ, van Smeden M, Wynants L, Steyerberg EW (2019).
#' Calibration: the Achilles heel of predictive analytics. *BMC Medicine*, 17,
#' 230. doi:10.1186/s12916-019-1466-7
#' @export
#' @examples
#' # A multinomial model that is deliberately too confident: the reported
#' # probabilities are the truth raised to the power 3, so the scalar that undoes
#' # the damage is about 3.
#' set.seed(1)
#' truth <- matrix(stats::runif(180), nrow = 60)
#' truth <- truth / rowSums(truth)
#' colnames(truth) <- c("a", "b", "c")
#' labels <- apply(truth, 1L, function(p) sample(colnames(truth), 1L, prob = p))
#' reported <- truth^3
#' reported <- reported / rowSums(reported)
#' calibrated <- calibrate_probabilities(reported, labels = labels, folds = 2L)
#' round(calibrated$parameters$fitted$temperature, 2)
#' calibrated$metrics_before[, c("accuracy", "ece", "logloss")]
#' calibrated$metrics_after[, c("accuracy", "ece", "logloss")]
calibrate_probabilities.uniml_multiclass <- function(
    x,
    labels = NULL,
    new_probs = NULL,
    new_labels = NULL,
    method = c("temperature", "none"),
    folds = 5L,
    seed = 123L,
    bins = 10L,
    ...) {
  method <- match.arg(method)
  .assert_scalar_number(folds, "folds", integer = TRUE, min = 2)
  .assert_scalar_number(seed, "seed", integer = TRUE)
  .assert_scalar_number(bins, "bins", integer = TRUE, min = 2)
  if (is.matrix(x)) {
    if (is.null(labels)) {
      .abort("`labels` is required when `x` is a probability matrix.")
    }
    x <- as_uniml_multiclass(x, truth = labels)
  }
  if (!is.list(x) || is.null(x$prob)) {
    .abort("A `uniml_multiclass` prediction must contain a `prob` matrix.")
  }
  if (is.null(x$truth)) {
    .abort("`x$truth` is required for probability calibration.")
  }
  training <- .clean_multiclass_inputs(x$prob, x$truth)
  if (training$n < 10L) {
    .abort("Calibration requires at least 10 complete observations.")
  }
  if (length(unique(training$truth)) < 2L) {
    .abort("Calibration requires at least two observed classes.")
  }
  classes <- training$classes
  raw <- training$prob
  observed <- training$truth
  if (!is.null(new_labels) && is.null(new_probs)) {
    .abort("`new_probs` is required when `new_labels` is supplied.")
  }
  if (!is.null(new_probs)) {
    new_probs <- as.matrix(new_probs)
    if (!identical(colnames(new_probs), classes)) {
      .abort(paste(
        "`new_probs` must have the same class columns, in the same order, as the",
        "probabilities the calibrator was fitted on."
      ))
    }
    .validate_probability_matrix(new_probs, name = "new_probs")
  }

  if (identical(method, "none")) {
    crossfit <- list(
      calibrated = raw,
      fold_temperatures = numeric(),
      failed = FALSE
    )
    final <- list(
      temperature = 1,
      nll_before = .temperature_nll(
        .multiclass_log_probabilities(raw),
        match(observed, classes),
        1
      ),
      nll_after = NA_real_,
      converged = TRUE
    )
    message <- "calibration skipped"
  } else {
    crossfit <- .crossfit_temperature(raw, observed, folds, seed)
    log_prob <- .multiclass_log_probabilities(raw)
    index <- match(observed, classes)
    final <- .fit_temperature(log_prob, index)
    message <- if (crossfit$failed) {
      paste(
        "At least one calibration fold failed; its raw probabilities were kept",
        "and the final scalar was fitted on every row."
      )
    } else {
      NULL
    }
  }

  if (is.null(new_probs)) {
    calibrated <- crossfit$calibrated
    n <- training$n
    metrics_before <- .multiclass_calibration_table(raw, observed, bins = bins)
    metrics_after <- .multiclass_calibration_table(
      calibrated,
      observed,
      bins = bins
    )
  } else {
    calibrated <- .apply_temperature(new_probs, final$temperature)
    calibrated[is.na(new_probs)] <- NA_real_
    n <- sum(stats::complete.cases(new_probs))
    if (is.null(new_labels)) {
      metrics_before <- NULL
      metrics_after <- NULL
    } else {
      checked <- .clean_multiclass_inputs(new_probs, new_labels)
      calibrated_checked <- .apply_temperature(
        checked$prob,
        final$temperature
      )
      metrics_before <- .multiclass_calibration_table(
        checked$prob,
        checked$truth,
        bins = bins
      )
      metrics_after <- .multiclass_calibration_table(
        calibrated_checked,
        checked$truth,
        bins = bins
      )
    }
  }

  structure(
    list(
      method = method,
      calibrated = calibrated,
      raw = raw,
      parameters = list(
        fitted = list(
          type = "temperature",
          temperature = final$temperature,
          nll_before = final$nll_before,
          nll_after = final$nll_after,
          converged = final$converged
        ),
        folds = crossfit$fold_temperatures,
        training_n = training$n
      ),
      metrics_before = metrics_before,
      metrics_after = metrics_after,
      failed = isTRUE(crossfit$failed),
      message = message,
      n = as.integer(n)
    ),
    class = c("uniml_calibration", "list")
  )
}

