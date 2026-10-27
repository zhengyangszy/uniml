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
