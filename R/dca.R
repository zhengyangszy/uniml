# L5 decision-curve analysis -------------------------------------------------

.dca_binary_probabilities <- function(probs, labels, thresholds = NULL,
                                      positive = NULL, model = "model",
                                      calibrated = FALSE) {
  .assert_scalar_character(model, "model")
  .assert_scalar_logical(calibrated, "calibrated")
  cleaned <- .clean_binary_inputs(probs, labels, positive = positive)
  if (cleaned$n == 0L) {
    .abort("DCA requires at least one complete observation.")
  }
  if (is.null(thresholds)) {
    thresholds <- seq(0.01, 0.99, by = 0.01)
  }
  if (!is.numeric(thresholds) || length(thresholds) == 0L ||
      anyNA(thresholds) || any(!is.finite(thresholds)) ||
      any(thresholds < 0 | thresholds >= 1)) {
    .abort("`thresholds` must contain finite values in [0, 1).")
  }
  thresholds <- sort(unique(as.numeric(thresholds)))
  event <- cleaned$event
  prob <- cleaned$prob
  n <- length(event)

  model_net_benefit <- vapply(thresholds, function(pt) {
    treated <- prob >= pt
    tp <- sum(treated & event == 1)
    fp <- sum(treated & event == 0)
    tp / n - fp / n * (pt / (1 - pt))
  }, numeric(1))

  event_rate <- mean(event)
  treat_all <- vapply(thresholds, function(pt) {
    event_rate - (1 - event_rate) * (pt / (1 - pt))
  }, numeric(1))

  out <- rbind(
    data.frame(
      model = model,
      strategy = "model",
      threshold = thresholds,
      net_benefit = model_net_benefit,
      n_samples = as.integer(n),
      calibrated = calibrated,
      stringsAsFactors = FALSE
    ),
    data.frame(
      model = "Treat All",
      strategy = "treat_all",
      threshold = thresholds,
      net_benefit = treat_all,
      n_samples = as.integer(n),
      calibrated = calibrated,
      stringsAsFactors = FALSE
    ),
    data.frame(
      model = "Treat None",
      strategy = "treat_none",
      threshold = thresholds,
      net_benefit = rep(0, length(thresholds)),
      n_samples = as.integer(n),
      calibrated = calibrated,
      stringsAsFactors = FALSE
    )
  )
  rownames(out) <- NULL
  class(out) <- c("uniml_dca", "data.frame")
  out
}

.dca_uniml_binary <- function(x, thresholds = NULL, positive = NULL,
                              model = "model", calibrated = FALSE, ...) {
  if (!is.list(x) || is.null(x$prob)) {
    .abort("A `uniml_binary` prediction must contain a `prob` component.")
  }
  if (is.null(x$truth)) {
    .abort("`x$truth` is required for decision-curve analysis.")
  }
  .dca_binary_probabilities(
    x$prob,
    x$truth,
    thresholds = thresholds,
    positive = if (is.null(positive)) x$positive else positive,
    model = model,
    calibrated = calibrated
  )
}

#' Decision-curve analysis
#'
#' @param x Predicted probabilities.
#' @param labels Binary truth labels.
#' @param thresholds Decision thresholds in `[0, 1)`.
#' @param positive Positive class label.
#' @param model Model name used in the returned data frame.
#' @param calibrated Whether the probabilities are calibrated.
#' @param ... Reserved for method-specific arguments.
#'
#' @return A data frame of class `uniml_dca` containing model and baseline
#'   net benefits.
#' @export
#' @examples
#' dca(
#'   c(0.1, 0.4, 0.6, 0.9),
#'   c(0, 0, 1, 1),
#'   thresholds = c(0.2, 0.5, 0.8)
#' )
dca.numeric <- function(x, labels = NULL, thresholds = NULL, positive = NULL,
                        model = "model", calibrated = FALSE, ...) {
  .validate_probability_vector(x, "x")
  if (is.null(labels)) {
    .abort("`labels` is required for numeric probability input.")
  }
  if (length(x) != length(labels)) {
    .abort("`x` and `labels` must have the same length.")
  }
  .dca_binary_probabilities(
    x,
    labels,
    thresholds = thresholds,
    positive = positive,
    model = model,
    calibrated = calibrated
  )
}

#' @export
print.uniml_dca <- function(x, ...) {
  cat("<uniml_dca>\n")
  cat(sprintf(
    "strategies: %s; thresholds: %d\n",
    paste(unique(x$model), collapse = ", "),
    length(unique(x$threshold))
  ))
  invisible(x)
}
