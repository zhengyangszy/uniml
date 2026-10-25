# L5 bootstrap confidence intervals -----------------------------------------

#' Pool estimates across multiple imputations (strict Rubin's rules)
#'
#' Combines `m` per-imputation point estimates and their variance estimates
#' into a single pooled estimate. The total variance follows Rubin (1987),
#' `T = U + (1 + 1/m) B`, and the confidence interval uses the
#' Barnard-Rubin (1999) degrees of freedom with a t quantile, so small `m`
#' does not produce overconfident intervals.
#'
#' @param estimates Numeric vector with one estimate per imputation.
#' @param variances Numeric vector with one variance estimate (standard error
#'   squared) per imputation, aligned with `estimates`.
#' @param level Confidence level.
#'
#' @return A list with `mean` (pooled estimate), `se` (square root of the
#'   total variance), `lower`, `upper`, `df` (Barnard-Rubin degrees of freedom;
#'   `Inf` when no between-imputation variance is present), `m`,
#'   `within` (`U`), `between` (`B`), and `total` (`T`).
#' @references
#' Rubin DB (1987). *Multiple Imputation for Nonresponse in Surveys*. Wiley.
#' doi:10.1002/9780470316696
#'
#' Barnard J, Rubin DB (1999). Small-sample degrees of freedom with multiple
#' imputation. *Biometrika*, 86(4), 948-955. doi:10.1093/biomet/86.4.948
#' @export
#' @examples
#' pool_mi(c(0.80, 0.82, 0.79), c(0.0004, 0.0004, 0.0005))
pool_mi <- function(estimates, variances, level = 0.95) {
  if (!is.numeric(estimates) || !is.numeric(variances) ||
      length(estimates) != length(variances) || length(estimates) == 0L) {
    .abort("`estimates` and `variances` must be numeric vectors of equal length.")
  }
  if (length(estimates) < 1L || anyNA(estimates) || anyNA(variances) ||
      any(variances < 0) || any(!is.finite(variances)) ||
      any(!is.finite(estimates))) {
    .abort("`estimates` and `variances` must be finite and free of missing values.")
  }
  .assert_scalar_number(level, "level", min = 0, max = 1)
  if (level <= 0 || level >= 1) {
    .abort("`level` must be strictly between 0 and 1.")
  }
  m <- length(estimates)
  mean_pooled <- mean(estimates)
  within <- mean(variances)
  between <- if (m > 1L) stats::var(estimates) else 0
  total <- within + (1 + 1 / m) * between
  se <- sqrt(total)
  alpha <- (1 - level) / 2

  if (!is.finite(total) || total <= 0) {
    return(list(
      mean = mean_pooled,
      se = if (is.finite(se)) se else NA_real_,
      lower = mean_pooled,
      upper = mean_pooled,
      df = NA_real_,
      m = as.integer(m),
      within = within,
      between = between,
      total = total
    ))
  }

  fraction_between <- (1 + 1 / m) * between / total
  if (!is.finite(fraction_between) || fraction_between <= 0) {
    df <- Inf
  } else {
    nu_old <- (m - 1) / fraction_between^2
    nu_obs <- ((nu_old + 1) / (nu_old + 3)) * nu_old * (1 - fraction_between)
    df <- 1 / (1 / nu_old + 1 / nu_obs)
    if (!is.finite(df) || df <= 0) {
      df <- Inf
    } else if (df < 1) {
      # When the between-imputation variance dominates and m is small the
      # Barnard-Rubin degrees of freedom can collapse towards zero, which makes
      # the t quantile explode. A floor of one degree of freedom keeps the
      # interval finite and still very conservative.
      df <- 1
    }
  }
  critical <- if (is.finite(df)) {
    stats::qt(1 - alpha, df)
  } else {
    stats::qnorm(1 - alpha)
  }
  if (!is.finite(critical)) {
    critical <- stats::qnorm(1 - alpha)
    df <- Inf
  }
  list(
    mean = mean_pooled,
    se = se,
    lower = mean_pooled - critical * se,
    upper = mean_pooled + critical * se,
    df = df,
    m = as.integer(m),
    within = within,
    between = between,
    total = total
  )
}

.ci_metric_names <- function() {
  c(
    "auc", "pr_auc", "brier", "accuracy", "sensitivity", "specificity",
    "ppv", "npv", "f1", "f2", "youden", "plr", "nlr", "dor",
    "balanced_accuracy", "kappa"
  )
}

# Theoretical range of each metric. Rubin intervals can leave the parameter
# space when the between-imputation variance dominates; they are clamped here.
.metric_bounds <- function(metric) {
  switch(
    metric,
    auc = c(0, 1),
    pr_auc = c(0, 1),
    brier = c(0, 1),
    accuracy = c(0, 1),
    sensitivity = c(0, 1),
    specificity = c(0, 1),
    ppv = c(0, 1),
    npv = c(0, 1),
    f1 = c(0, 1),
    f2 = c(0, 1),
    balanced_accuracy = c(0, 1),
    youden = c(-1, 1),
    kappa = c(-1, 1),
    plr = c(0, Inf),
    nlr = c(0, Inf),
    dor = c(0, Inf),
    NULL
  )
}

.clamp_metric_interval <- function(metric, lower, upper) {
  bounds <- .metric_bounds(metric)
  if (is.null(bounds) || !is.finite(lower) || !is.finite(upper)) {
    return(c(lower, upper))
  }
  c(max(lower, bounds[[1L]]), min(upper, bounds[[2L]]))
}

.bootstrap_metrics_once <- function(index, probs, event, threshold, metrics) {
  boot_event <- event[index]
  if (length(unique(boot_event)) < 2L) {
    return(stats::setNames(rep(NA_real_, length(metrics)), metrics))
  }

  values <- .calc_binary_metrics(
    probs = probs[index],
    truth = boot_event,
    threshold = threshold,
    positive = 1,
    metrics = metrics
  )
  vapply(values, as.numeric, numeric(1))
}

#' Bootstrap confidence intervals for binary metrics
#'
#' Resamples paired probabilities and labels with replacement, recalculates
#' the requested binary metrics, and returns percentile confidence intervals.
#'
#' @param probs Numeric predicted probabilities.
#' @param labels Binary truth labels.
#' @param method Confidence-interval method. Currently `"bootstrap"`.
#' @param R Number of bootstrap resamples.
#' @param level Confidence level.
#' @param seed Random seed.
#' @param threshold Classification threshold.
#' @param threshold_method Threshold method used when `threshold = NULL`.
#' @param cost Named false-positive and false-negative cost vector.
#' @param fixed_threshold Fixed threshold used by the `"fixed"` method.
#' @param positive Positive class label.
#' @param metrics `"all"` or a character vector of metric names.
#' @param ... Reserved for method-specific arguments.
#'
#' @return A data frame with columns `metric`, `mean`, `se` (bootstrap standard
#'   error), `lower`, `upper`, `n_effective`, and `formatted`.
#' @references
#' Efron B, Tibshirani RJ (1993). *An Introduction to the Bootstrap*. Chapman &
#' Hall. doi:10.1201/9780429246593
#' @export
#' @examples
#' calc_ci(
#'   c(0.1, 0.4, 0.6, 0.9),
#'   c(0, 0, 1, 1),
#'   R = 20,
#'   threshold = 0.5,
#'   seed = 1
#' )
calc_ci <- function(probs, labels, method = "bootstrap", R = 1000L,
                    level = 0.95, seed = 123L, threshold = 0.5,
                    threshold_method = "fixed", cost = c(fp = 1, fn = 1),
                    fixed_threshold = 0.5, positive = NULL, metrics = "all",
                    ...) {
  .assert_choice(method, "method", "bootstrap")
  .assert_scalar_number(R, "R", integer = TRUE, min = 1)
  .assert_scalar_number(level, "level", min = 0, max = 1)
  if (level <= 0 || level >= 1) {
    .abort("`level` must be strictly between 0 and 1.")
  }
  .assert_scalar_number(seed, "seed", integer = TRUE)
  .assert_choice(
    threshold_method,
    "threshold_method",
    c("youden", "cost", "fixed")
  )
  .assert_cost(cost)
  .assert_scalar_number(
    fixed_threshold,
    "fixed_threshold",
    min = 0,
    max = 1
  )
  if (!is.null(threshold)) {
    .assert_scalar_number(threshold, "threshold", min = 0, max = 1)
  }

  cleaned <- .clean_binary_inputs(probs, labels, positive = positive)
  metric_names <- if (identical(metrics, "all")) {
    .ci_metric_names()
  } else {
    .assert_character_vector(metrics, "metrics")
    unknown <- setdiff(metrics, .ci_metric_names())
    if (length(unknown) > 0L) {
      .abort(sprintf(
        "`metrics` contains unsupported values: %s.",
        paste(unknown, collapse = ", ")
      ))
    }
    metrics
  }

  if (is.null(threshold)) {
    threshold <- find_threshold(
      cleaned$prob,
      cleaned$event,
      method = threshold_method,
      cost = cost,
      fixed_threshold = fixed_threshold,
      positive = 1
    )
  }
  if (!is.finite(threshold)) {
    threshold <- NA_real_
  }

  if (cleaned$n == 0L || is.na(threshold)) {
    values <- lapply(metric_names, function(metric) rep(NA_real_, R))
  } else {
    set.seed(seed)
    values <- future.apply::future_lapply(
      seq_len(R),
      function(i) {
        index <- sample.int(cleaned$n, cleaned$n, replace = TRUE)
        .bootstrap_metrics_once(
          index,
          probs = cleaned$prob,
          event = cleaned$event,
          threshold = threshold,
          metrics = metric_names
        )
      },
      future.seed = TRUE
    )
    values <- lapply(
      metric_names,
      function(metric) {
        vapply(values, function(x) unname(x[[metric]]), numeric(1))
      }
    )
  }

  result <- lapply(seq_along(metric_names), function(i) {
    x <- values[[i]]
    effective <- sum(!is.na(x))
    if (effective == 0L) {
      stats::setNames(
        c(NA_real_, NA_real_, NA_real_, NA_real_, effective),
        c("mean", "se", "lower", "upper", "n_effective")
      )
    } else {
      bounds <- stats::quantile(
        x,
        probs = c((1 - level) / 2, 1 - (1 - level) / 2),
        na.rm = TRUE,
        names = FALSE,
        type = 7
      )
      c(
        mean = mean(x, na.rm = TRUE),
        se = stats::sd(x, na.rm = TRUE),
        lower = bounds[1L],
        upper = bounds[2L],
        n_effective = effective
      )
    }
  })

  means <- vapply(result, function(x) x[["mean"]], numeric(1))
  se <- vapply(result, function(x) x[["se"]], numeric(1))
  lower <- vapply(result, function(x) x[["lower"]], numeric(1))
  upper <- vapply(result, function(x) x[["upper"]], numeric(1))
  n_effective <- vapply(
    result,
    function(x) as.integer(x[["n_effective"]]),
    integer(1)
  )
  formatted <- ifelse(
    is.na(means),
    "NA",
    sprintf("%.3f (%.3f-%.3f)", means, lower, upper)
  )

  out <- data.frame(
    metric = metric_names,
    mean = means,
    se = se,
    lower = lower,
    upper = upper,
    n_effective = n_effective,
    formatted = formatted,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  too_few <- n_effective < ceiling(0.8 * R)
  if (any(too_few)) {
    warning(
      sprintf(
        "Fewer than %.0f%% of bootstrap resamples were effective for: %s.",
        100 * 0.8,
        paste(out$metric[too_few], collapse = ", ")
      ),
      call. = FALSE
    )
  }
  out
}
