# L5 probability calibration -------------------------------------------------

.safe_logit <- function(probs) {
  eps <- 1e-12
  safe <- pmin(pmax(probs, eps), 1 - eps)
  pmin(pmax(log(safe / (1 - safe)), -50), 50)
}

.binary_calibration_metrics <- function(probs, labels, bins = 10L,
                                        positive = NULL) {
  .assert_scalar_number(bins, "bins", integer = TRUE, min = 2)
  cleaned <- .clean_binary_inputs(probs, labels, positive = positive)
  if (cleaned$n == 0L) {
    return(data.frame(
      brier = NA_real_,
      ece = NA_real_,
      mce = NA_real_,
      calib_slope = NA_real_,
      calib_intercept = NA_real_,
      calib_r2 = NA_real_,
      n = 0L
    ))
  }

  event <- cleaned$event
  prob <- cleaned$prob
  brier <- mean((event - prob)^2)
  binned <- .binned_calibration(prob, event, bins = bins)
  ece <- binned$ece
  mce <- binned$mce

  calib_slope <- NA_real_
  calib_intercept <- NA_real_
  calib_r2 <- NA_real_
  groups <- binned$table
  if (nrow(groups) >= 2L) {
    fit <- tryCatch(
      stats::lm(observed ~ predicted, data = groups, weights = groups$n),
      error = function(e) NULL
    )
    if (!is.null(fit)) {
      coefs <- stats::coef(fit)
      calib_intercept <- unname(coefs[1L])
      calib_slope <- unname(coefs[2L])
      calib_r2 <- unname(summary(fit)$r.squared)
    }
  }

  data.frame(
    brier = brier,
    ece = ece,
    mce = mce,
    calib_slope = calib_slope,
    calib_intercept = calib_intercept,
    calib_r2 = calib_r2,
    n = as.integer(cleaned$n),
    row.names = NULL
  )
}

.fit_platt <- function(probs, labels) {
  if (length(probs) < 2L || length(unique(labels)) < 2L) {
    return(NULL)
  }
  logit <- .safe_logit(probs)
  fit <- tryCatch(
    stats::glm(labels ~ logit, family = stats::binomial()),
    error = function(e) NULL
  )
  if (is.null(fit)) {
    return(NULL)
  }
  coefficients <- stats::coef(fit)
  if (length(coefficients) != 2L || anyNA(coefficients)) {
    return(NULL)
  }
  list(
    type = "platt",
    intercept = unname(coefficients[1L]),
    slope = unname(coefficients[2L])
  )
}

.fit_platt_segment <- function(probs, labels) {
  overall <- .fit_platt(probs, labels)
  segment <- ifelse(probs < 0.3, 1L, ifelse(probs < 0.7, 2L, 3L))
  segments <- lapply(seq_len(3L), function(i) {
    index <- segment == i
    if (sum(index) < 5L) {
      return(NULL)
    }
    .fit_platt(probs[index], labels[index])
  })
  if (is.null(overall) && all(vapply(segments, is.null, logical(1)))) {
    return(NULL)
  }
  list(
    type = "platt_segment",
    overall = overall,
    segments = segments,
    breaks = c(0, 0.3, 0.7, 1)
  )
}

.fit_isotonic <- function(probs, labels) {
  if (length(probs) < 2L || length(unique(labels)) < 2L) {
    return(NULL)
  }
  order_index <- order(probs)
  fit <- tryCatch(
    stats::isoreg(probs[order_index], labels[order_index]),
    error = function(e) NULL
  )
  if (is.null(fit) || any(!is.finite(fit$f)) || length(fit$x) == 0L) {
    return(NULL)
  }
  list(type = "isotonic", x = fit$x, y = fit$yf)
}

.calibration_event <- function(labels) {
  if (is.null(labels)) {
    return(NULL)
  }
  if (is.logical(labels)) {
    return(as.integer(labels))
  }
  if (is.numeric(labels)) {
    values <- unique(labels[!is.na(labels)])
    if (length(values) > 2L) {
      .abort("Calibration requires a binary outcome.")
    }
    if (length(values) == 2L && !all(values %in% c(0, 1))) {
      return(as.integer(labels == max(values)))
    }
    return(as.integer(labels))
  }
  if (is.factor(labels) || is.character(labels)) {
    factored <- factor(labels)
    if (nlevels(factored) > 2L) {
      .abort("Calibration requires a binary outcome.")
    }
    return(as.integer(factored == levels(factored)[nlevels(factored)]))
  }
  .abort("Unsupported label type for calibration.")
}

.fit_calibrator <- function(probs, labels, method) {
  # `glm(family = binomial())` treats a character response as a proportion, so
  # the labels are always normalised to 0/1 before fitting.
  event <- .calibration_event(labels)
  switch(
    method,
    platt = .fit_platt(probs, event),
    platt_segment = .fit_platt_segment(probs, event),
    isotonic = .fit_isotonic(probs, event),
    .abort("Unsupported calibration method.")
  )
}

.predict_calibrator <- function(fit, probs) {
  if (is.null(fit)) {
    return(as.numeric(probs))
  }
  if (identical(fit$type, "platt")) {
    return(stats::plogis(fit$intercept + fit$slope * .safe_logit(probs)))
  }
  if (identical(fit$type, "isotonic")) {
    predicted <- stats::approx(
      fit$x,
      fit$y,
      xout = probs,
      rule = 2,
      ties = "ordered"
    )$y
    return(pmin(pmax(predicted, 0), 1))
  }
  if (identical(fit$type, "platt_segment")) {
    segment <- ifelse(probs < 0.3, 1L, ifelse(probs < 0.7, 2L, 3L))
    out <- rep(NA_real_, length(probs))
    for (i in seq_len(3L)) {
      index <- segment == i
      if (!any(index)) {
        next
      }
      local_fit <- fit$segments[[i]]
      if (is.null(local_fit)) {
        local_fit <- fit$overall
      }
      out[index] <- .predict_calibrator(local_fit, probs[index])
    }
    missing <- is.na(out)
    if (any(missing) && !is.null(fit$overall)) {
      out[missing] <- .predict_calibrator(fit$overall, probs[missing])
    }
    return(out)
  }
  as.numeric(probs)
}

.calibration_fold_ids <- function(event, folds, seed) {
  folds <- min(as.integer(folds), length(event))
  set.seed(seed)
  fold_id <- integer(length(event))
  for (class_value in unique(event)) {
    index <- which(event == class_value)
    index <- sample(index, length(index))
    fold_id[index] <- rep(seq_len(folds), length.out = length(index))
  }
  fold_id
}

.crossfit_calibration <- function(probs, event, method, folds, seed) {
  n <- length(event)
  fold_id <- .calibration_fold_ids(event, folds, seed)
  calibrated <- as.numeric(probs)
  fold_fits <- vector("list", max(fold_id))
  failed <- FALSE
  for (fold in seq_along(fold_fits)) {
    train <- fold_id != fold
    test <- !train
    fit <- .fit_calibrator(probs[train], event[train], method)
    fold_fits[[fold]] <- fit
    if (is.null(fit)) {
      failed <- TRUE
      next
    }
    calibrated[test] <- .predict_calibrator(fit, probs[test])
  }
  list(
    calibrated = pmin(pmax(calibrated, 0), 1),
    fold_fits = fold_fits,
    failed = failed
  )
}

.calibrate_probabilities_uniml_binary <- function(
    x,
    new_probs = NULL,
    new_labels = NULL,
    method = c("platt", "platt_segment", "isotonic", "none"),
    folds = 5L,
    seed = 123L,
    positive = NULL,
    bins = 10L,
    ...) {
  method <- match.arg(method)
  .assert_scalar_number(folds, "folds", integer = TRUE, min = 2)
  .assert_scalar_number(seed, "seed", integer = TRUE)
  .assert_scalar_number(bins, "bins", integer = TRUE, min = 2)

  if (!is.list(x) || is.null(x$prob)) {
    .abort("A `uniml_binary` prediction must contain a `prob` component.")
  }
  if (is.null(x$truth)) {
    .abort("`x$truth` is required for probability calibration.")
  }
  training <- .clean_binary_inputs(
    x$prob,
    x$truth,
    positive = if (is.null(positive)) x$positive else positive
  )
  if (training$n == 0L) {
    .abort("Calibration requires at least one complete training observation.")
  }
  if (length(unique(training$event)) < 2L) {
    .abort("Calibration requires both outcome classes in the training data.")
  }

  if (is.null(new_probs)) {
    if (!is.null(new_labels)) {
      .abort("`new_probs` is required when `new_labels` is supplied.")
    }
    raw <- training$prob
    event <- training$event
    if (identical(method, "none")) {
      calibrated <- raw
      fold_fits <- list()
      failed <- FALSE
      message <- "calibration skipped"
    } else {
      crossfit <- .crossfit_calibration(
        raw,
        event,
        method = method,
        folds = folds,
        seed = seed
      )
      calibrated <- crossfit$calibrated
      fold_fits <- crossfit$fold_fits
      failed <- crossfit$failed
      message <- if (failed) {
        "At least one calibration fold failed; failed fold predictions use raw probabilities."
      } else {
        NULL
      }
    }
    final_fit <- if (identical(method, "none")) {
      NULL
    } else {
      .fit_calibrator(raw, event, method)
    }
    parameters <- list(
      fitted = final_fit,
      folds = fold_fits,
      training_n = training$n
    )
    metrics_before <- .binary_calibration_metrics(raw, event, bins = bins)
    metrics_after <- .binary_calibration_metrics(
      calibrated,
      event,
      bins = bins
    )
    n <- training$n
  } else {
    .validate_probability_vector(new_probs, "new_probs")
    raw <- as.numeric(new_probs)
    if (!is.null(new_labels)) {
      .clean_binary_inputs(
        raw,
        new_labels,
        positive = training$positive
      )
      validation_event <- as.numeric(.binary_event_vector(
        new_labels,
        positive = training$positive
      )$event)
      validation_n <- sum(!is.na(raw) & !is.na(new_labels))
    } else {
      validation_event <- rep(NA_real_, length(raw))
      validation_n <- sum(!is.na(raw))
    }
    if (identical(method, "none")) {
      calibrated <- raw
      final_fit <- NULL
      failed <- FALSE
      message <- "calibration skipped"
    } else {
      final_fit <- .fit_calibrator(training$prob, training$event, method)
      if (is.null(final_fit)) {
        calibrated <- raw
        failed <- TRUE
        message <- "Calibration fitting failed; raw probabilities were returned."
      } else {
        calibrated <- .predict_calibrator(final_fit, raw)
        calibrated[is.na(raw)] <- NA_real_
        failed <- FALSE
        message <- NULL
      }
    }
    parameters <- list(
      fitted = final_fit,
      training_n = training$n
    )
    if (!is.null(new_labels)) {
      keep <- !is.na(validation_event) & !is.na(raw)
      metrics_before <- .binary_calibration_metrics(
        raw[keep],
        validation_event[keep],
        bins = bins,
        positive = 1
      )
      metrics_after <- .binary_calibration_metrics(
        calibrated[keep],
        validation_event[keep],
        bins = bins,
        positive = 1
      )
    } else {
      metrics_before <- NULL
      metrics_after <- NULL
    }
    n <- validation_n
  }

  structure(
    list(
      method = method,
      calibrated = as.numeric(calibrated),
      raw = as.numeric(raw),
      parameters = parameters,
      metrics_before = metrics_before,
      metrics_after = metrics_after,
      failed = failed,
      message = message,
      n = as.integer(n)
    ),
    class = c("uniml_calibration", "list")
  )
}

#' @rdname calibrate_probabilities
#' @param labels Binary truth labels. Required by the numeric method.
#' @param method Calibration method: `"platt"`, `"platt_segment"`,
#'   `"isotonic"`, or `"none"`.
#' @param folds Number of cross-fitting folds.
#' @param seed Random seed used for cross-fitting.
#' @export
calibrate_probabilities.numeric <- function(
    x,
    labels = NULL,
    method = c("platt", "platt_segment", "isotonic", "none"),
    folds = 5L,
    seed = 123L,
    ...) {
  .validate_probability_vector(x, "x")
  if (is.null(labels)) {
    .abort("`labels` is required for numeric probability input.")
  }
  if (length(x) != length(labels)) {
    .abort("`x` and `labels` must have the same length.")
  }
  binary <- structure(
    list(prob = x, truth = labels),
    class = c("uniml_binary", "uniml_prediction")
  )
  calibrate_probabilities(
    binary,
    method = method,
    folds = folds,
    seed = seed,
    ...
  )
}

#' Print a fitted calibration
#'
#' @param x A `uniml_calibration` object.
#' @param ... Additional arguments passed to methods.
#'
#' @return Invisibly returns `x`.
#' @export
print.uniml_calibration <- function(x, ...) {
  cat("<uniml_calibration>\n")
  cat(sprintf("method: %s\n", x$method))
  cat(sprintf("n: %d, failed: %s\n", x$n, x$failed))
  if (!is.null(x$metrics_after)) {
    cat(sprintf(
      "Brier after: %.4f, ECE after: %.4f\n",
      x$metrics_after$brier,
      x$metrics_after$ece
    ))
  }
  invisible(x)
}
