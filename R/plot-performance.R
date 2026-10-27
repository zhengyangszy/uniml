# L7 performance plots -------------------------------------------------------
#
# ROC / calibration / DCA / confusion-matrix figures. Every public plotting
# function accepts either a `uniml_result` or a prediction collection so the
# atomic layer and the workflow layer share one implementation.

.plot_binary_metric_columns <- c("auc", "brier", "ece", "mce")

.prediction_like <- function(x) {
  is.list(x) &&
    !is.data.frame(x) &&
    !is.null(x$truth) &&
    (!is.null(x$prob) || !is.null(x$calibrated_prob))
}

.prediction_frame <- function(x) {
  if (!is.data.frame(x)) {
    return(NULL)
  }
  if (!"truth" %in% names(x) ||
      !any(c("prob", "calibrated_prob") %in% names(x))) {
    return(NULL)
  }
  x
}

# Normalise every accepted prediction source into a data frame with the
# canonical columns `model`, `prob`, `truth`, `calibrated_prob`, `positive`,
# and `set`. Old long tables are converted by matching raw/calibrated rows
# within each model.
.normalise_predictions <- function(x, model = NULL, calibrated = FALSE) {
  .assert_scalar_logical(calibrated, "calibrated")
  if (is.null(x)) {
    .abort("Prediction data are required for this plot.")
  }
  if (inherits(x, "uniml_result")) {
    x <- .result_predictions(x)
  }
  if (is.null(x)) {
    .abort("Prediction data are required for this plot.")
  }

  if (.prediction_like(x)) {
    entries <- list(x)
    names(entries) <- if (is.null(model) || !nzchar(model)) "model" else model
  } else if (is.data.frame(x)) {
    frame <- .prediction_frame(x)
    if (is.null(frame)) {
      .abort(
        "Prediction data frames need `truth` and either `prob` or `calibrated_prob`."
      )
    }
    if (!"model" %in% names(frame)) {
      frame$model <- if (is.null(model) || !nzchar(model)) "model" else model
    }
    frame$model <- as.character(frame$model)
    if (anyNA(frame$model) || any(!nzchar(frame$model))) {
      .abort("`model` values in prediction data must not be missing or empty.")
    }
    model_names <- unique(frame$model)
    rows <- lapply(model_names, function(name) {
      .normalise_prediction_entry(
        frame[frame$model == name, , drop = FALSE],
        name = name,
        calibrated = calibrated
      )
    })
    out <- do.call(rbind, rows)
    row.names(out) <- NULL
    return(out)
  } else if (is.list(x)) {
    entries <- x
    if (length(entries) == 0L) {
      .abort("Prediction data are empty.")
    }
    if (is.null(names(entries)) || any(!nzchar(names(entries)))) {
      names(entries) <- paste0("model", seq_along(entries))
    }
  } else {
    .abort("Unsupported prediction input for plotting.")
  }

  rows <- lapply(names(entries), function(name) {
    entry <- entries[[name]]
    if (is.null(entry)) {
      return(NULL)
    }
    if (inherits(entry, "Learner")) {
      .abort(sprintf("Model \"%s\" is an untrained learner; supply predictions.", name))
    }
    if (!.prediction_like(entry) && !is.data.frame(entry)) {
      .abort(sprintf("Prediction entry \"%s\" is not a supported object.", name))
    }
    .normalise_prediction_entry(
      entry,
      name = name,
      calibrated = calibrated
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0L) {
    .abort("No usable prediction entries were supplied.")
  }
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

.first_prediction_value <- function(x, default = NA) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) default else x[[1L]]
}

.recycle_prediction_value <- function(x, n, name, value_name) {
  if (is.null(x)) {
    return(rep(NA, n))
  }
  if (length(x) == 1L) {
    return(rep(x, n))
  }
  if (length(x) != n) {
    .abort(sprintf(
      "Model \"%s\" has length %d for `%s`, expected %d.",
      name,
      length(x),
      value_name,
      n
    ))
  }
  x
}

.prediction_entry_frame <- function(entry, name, calibrated) {
  if (.prediction_like(entry)) {
    truth <- entry$truth
    n <- length(truth)
    prob <- if (is.null(entry$prob)) rep(NA_real_, n) else entry$prob
    calibrated_prob <- if (is.null(entry$calibrated_prob)) {
      rep(NA_real_, n)
    } else {
      entry$calibrated_prob
    }
    stage <- if (is.null(entry$calibrated)) calibrated else entry$calibrated
    stage <- .recycle_prediction_value(stage, n, name, "calibrated")
    stage <- as.logical(stage)
    if (all(is.na(calibrated_prob)) && any(!is.na(stage) & stage)) {
      calibrated_prob <- prob
      prob <- rep(NA_real_, n)
    }
    return(data.frame(
      model = name,
      prob = as.numeric(prob),
      truth = truth,
      calibrated_prob = as.numeric(calibrated_prob),
      positive = .recycle_prediction_value(
        if (is.null(entry$positive)) NA_character_ else entry$positive,
        n,
        name,
        "positive"
      ),
      set = .recycle_prediction_value(
        if (is.null(entry$set)) NA_character_ else entry$set,
        n,
        name,
        "set"
      ),
      stringsAsFactors = FALSE
    ))
  }

  frame <- .prediction_frame(entry)
  if (is.null(frame)) {
    .abort(sprintf(
      "Prediction entry \"%s\" needs `truth` and either `prob` or `calibrated_prob`.",
      name
    ))
  }
  truth <- frame$truth
  n <- length(truth)
  if ("calibrated_prob" %in% names(frame)) {
    prob <- if ("prob" %in% names(frame)) frame$prob else rep(NA_real_, n)
    calibrated_prob <- frame$calibrated_prob
  } else if ("calibrated" %in% names(frame)) {
    stage <- as.logical(frame$calibrated)
    raw_index <- !is.na(stage) & !stage
    calibrated_index <- !is.na(stage) & stage
    raw_prob <- frame$prob[raw_index]
    calibrated_prob <- frame$prob[calibrated_index]
    raw_truth <- frame$truth[raw_index]
    calibrated_truth <- frame$truth[calibrated_index]
    if (length(raw_prob) > 0L && length(calibrated_prob) > 0L) {
      if (length(raw_prob) != length(calibrated_prob) ||
          !identical(as.character(raw_truth), as.character(calibrated_truth))) {
        .abort(sprintf(
          "Model \"%s\" has raw and calibrated rows that cannot be aligned.",
          name
        ))
      }
      truth <- raw_truth
      n <- length(truth)
      prob <- raw_prob
    } else if (length(calibrated_prob) > 0L) {
      truth <- calibrated_truth
      n <- length(truth)
      prob <- rep(NA_real_, n)
    } else {
      truth <- raw_truth
      n <- length(truth)
      prob <- raw_prob
      calibrated_prob <- rep(NA_real_, n)
    }
  } else {
    prob <- frame$prob
    calibrated_prob <- rep(NA_real_, n)
    if (isTRUE(calibrated)) {
      calibrated_prob <- prob
      prob <- rep(NA_real_, n)
    }
  }

  prob <- .recycle_prediction_value(prob, n, name, "prob")
  calibrated_prob <- .recycle_prediction_value(
    calibrated_prob,
    n,
    name,
    "calibrated_prob"
  )
  positive <- if ("positive" %in% names(frame)) {
    .recycle_prediction_value(frame$positive, n, name, "positive")
  } else {
    rep(NA_character_, n)
  }
  set <- if ("set" %in% names(frame)) {
    .recycle_prediction_value(frame$set, n, name, "set")
  } else {
    rep(NA_character_, n)
  }
  data.frame(
    model = name,
    prob = suppressWarnings(as.numeric(prob)),
    truth = truth,
    calibrated_prob = suppressWarnings(as.numeric(calibrated_prob)),
    positive = as.character(positive),
    set = as.character(set),
    stringsAsFactors = FALSE
  )
}

.normalise_prediction_entry <- function(entry, name, calibrated = FALSE) {
  if (is.data.frame(entry) && "model" %in% names(entry)) {
    return(.prediction_entry_frame(entry, name, calibrated = calibrated))
  }
  if (is.data.frame(entry) || .prediction_like(entry)) {
    return(.prediction_entry_frame(entry, name, calibrated = calibrated))
  }
  .abort(sprintf("Prediction entry \"%s\" is not a supported object.", name))
}

.result_predictions <- function(x) {
  if (!inherits(x, "uniml_result")) {
    return(x)
  }
  validate_uniml_result(x)
  meta <- x$meta
  if (is.list(meta) && !is.null(meta$predictions)) {
    return(meta$predictions)
  }
  plots <- x$plots
  if (is.list(plots) && !is.null(plots$predictions)) {
    return(plots$predictions)
  }
  if (is.list(plots) && length(plots) > 0L && all(vapply(
    plots, .prediction_like, logical(1)
  ))) {
    return(plots)
  }
  NULL
}

# The comparison set of a result is the default plotting set. Pooling train,
# validation, test, and external rows into one curve would mix different data.
.default_plot_set <- function(x) {
  if (!inherits(x, "uniml_result")) {
    return(NULL)
  }
  ranking <- x$ranking
  if (is.data.frame(ranking) && nrow(ranking) > 0L && "set" %in% names(ranking)) {
    values <- unique(as.character(ranking$set))
    values <- values[!is.na(values) & nzchar(values)]
    if (length(values) > 0L) {
      return(values[[1L]])
    }
  }
  NULL
}

.filter_prediction_sets <- function(predictions, set = NULL) {
  if (is.null(set) || identical(set, "all")) {
    return(predictions)
  }
  if (!"set" %in% names(predictions)) {
    return(predictions)
  }
  available <- unique(as.character(predictions$set))
  if (!set %in% available) {
    .abort(sprintf(
      "The requested set \"%s\" is not present; available: %s.",
      set,
      paste(available, collapse = ", ")
    ))
  }
  predictions[predictions$set == set, , drop = FALSE]
}

# "best" and "all" are accepted for `models` so the performance plots use the
# same vocabulary as the SHAP plots.
.resolve_plot_models <- function(x, models = NULL) {
  if (is.null(models)) {
    return(NULL)
  }
  .assert_character_vector(models, "models")
  if (identical(models, "all")) {
    return(NULL)
  }
  if (identical(models, "best")) {
    if (inherits(x, "uniml_result")) {
      best <- tryCatch(
        .workflow_model_key(x, "best"),
        error = function(e) NULL
      )
      if (!is.null(best)) {
        return(best)
      }
    }
    return(NULL)
  }
  models
}

.filter_prediction_models <- function(predictions, models = NULL) {
  if (is.null(models)) {
    return(predictions)
  }
  .assert_character_vector(models, "models")
  missing_models <- setdiff(models, unique(predictions$model))
  if (length(missing_models) > 0L) {
    .abort(sprintf(
      "Unknown model(s) in the prediction data: %s.",
      paste(missing_models, collapse = ", ")
    ))
  }
  predictions[predictions$model %in% models, , drop = FALSE]
}

.prediction_values <- function(predictions, calibrated = FALSE,
                               fallback = FALSE) {
  .assert_scalar_logical(calibrated, "calibrated")
  .assert_scalar_logical(fallback, "fallback")
  raw <- if ("prob" %in% names(predictions)) {
    as.numeric(predictions$prob)
  } else {
    rep(NA_real_, nrow(predictions))
  }
  calibrated_prob <- if ("calibrated_prob" %in% names(predictions)) {
    as.numeric(predictions$calibrated_prob)
  } else {
    rep(NA_real_, nrow(predictions))
  }
  out <- if (isTRUE(calibrated)) calibrated_prob else raw
  if (isTRUE(fallback)) {
    replacement <- if (isTRUE(calibrated)) raw else calibrated_prob
    out[is.na(out)] <- replacement[is.na(out)]
  }
  out
}

.roc_curve_rows <- function(model, prob, truth, positive) {
  cleaned <- .clean_binary_inputs(prob, truth, positive = positive)
  if (cleaned$n < 2L || length(unique(cleaned$event)) < 2L) {
    return(NULL)
  }
  fit <- tryCatch(
    pROC::roc(
      response = cleaned$event,
      predictor = cleaned$prob,
      levels = c(0, 1),
      direction = "<",
      quiet = TRUE
    ),
    error = function(e) NULL
  )
  if (is.null(fit)) {
    return(NULL)
  }
  sensitivity <- as.numeric(fit$sensitivities)
  specificity <- as.numeric(fit$specificities)
  data.frame(
    model = model,
    specificity = specificity,
    sensitivity = sensitivity,
    fpr = 1 - specificity,
    auc = as.numeric(pROC::auc(fit)),
    stringsAsFactors = FALSE
  )
}

#' Plot ROC curves
#'
#' Accepts a `uniml_result`, a `uniml_binary` prediction, a named list of
#' prediction objects, or a data frame with `model`, `prob`, and `truth`
#' columns. ROC curves are invariant under monotone calibration, so a single
#' curve per model is drawn.
#'
#' @param x A `uniml_result` or prediction collection.
#' @param models Optional subset of model names. `"best"` keeps the top-ranked
#'   model and `"all"` keeps every model.
#' @param positive Positive class label, used when `x` is an mlr3 prediction.
#' @param palette Discrete palette: a `ggsci` name, a colour, or a colour vector.
#' @param theme A `ggplot2` theme applied to the figure.
#' @param title,subtitle,caption Optional figure labels.
#' @param set Data set to plot (`"validation"`, `"test"`, `"external"`, ...);
#'   defaults to the comparison set used for ranking.
#' @param micro For a multiclass prediction, whether to add the micro-average
#'   curve beside the one-vs-rest curves. The subtitle reports all three AUCs
#'   (Hand-Till, macro one-vs-rest, micro).
#' @param ... Reserved for method-specific arguments.
#'
#' @return A `ggplot` object.
#' @export
#' @rdname plot_roc
#' @examples
#' predictions <- data.frame(
#'   model = "lr",
#'   prob = c(0.1, 0.4, 0.6, 0.9),
#'   truth = factor(c("no", "no", "yes", "yes"), levels = c("no", "yes")),
#'   positive = "yes"
#' )
#' plot_roc(predictions)
plot_roc.uniml_result <- function(x, models = NULL, palette = "bmj",
                                  theme = theme_uniml(), title = NULL,
                                  subtitle = NULL, caption = NULL,
                                  set = NULL, ...) {
  predictions <- .normalise_predictions(.result_predictions(x))
  predictions <- .filter_prediction_sets(
    predictions,
    if (is.null(set)) .default_plot_set(x) else set
  )
  predictions <- .filter_prediction_models(
    predictions,
    .resolve_plot_models(x, models)
  )
  model_names <- unique(predictions$model)
  curves <- lapply(model_names, function(name) {
    rows <- predictions[predictions$model == name, , drop = FALSE]
    .roc_curve_rows(
      model = name,
      prob = .prediction_values(rows, calibrated = FALSE, fallback = TRUE),
      truth = rows$truth,
      positive = .first_prediction_value(rows$positive)
    )
  })
  curves <- curves[!vapply(curves, is.null, logical(1))]
  if (length(curves) == 0L) {
    .abort("No model had two outcome classes available for an ROC curve.")
  }
  curve <- do.call(rbind, curves)
  labels <- unique(curve[, c("model", "auc"), drop = FALSE])
  labels$label <- sprintf("%s (AUC = %.3f)", labels$model, labels$auc)
  curve$label <- labels$label[match(curve$model, labels$model)]
  model_levels <- unique(curve$label)
  curve$label <- factor(curve$label, levels = model_levels)
  values <- stats::setNames(
    .uniml_plot_palette(length(model_levels), palette = palette),
    model_levels
  )
  plot <- ggplot2::ggplot(
    curve,
    ggplot2::aes(x = .data$fpr, y = .data$sensitivity, colour = .data$label)
  ) +
    ggplot2::geom_abline(
      intercept = 0, slope = 1, linetype = "dashed", colour = "grey60"
    ) +
    ggplot2::geom_path(linewidth = 0.8) +
    ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::labs(x = "1 - Specificity", y = "Sensitivity", colour = NULL) +
    .uniml_scale_color(values = values) +
    theme_uniml()
  .uniml_style_plot(plot, theme, title, subtitle, caption)
}

#' @rdname plot_roc
#' @export
plot_roc.uniml_binary <- function(x, models = NULL, palette = "bmj", ...) {
  plot_roc.uniml_result(
    .as_uniml_result(x),
    models = models,
    palette = palette,
    ...
  )
}

#' @rdname plot_roc
#' @export
plot_roc.PredictionClassif <- function(x, positive = NULL, ...) {
  plot_roc(as_uniml_binary(x, positive = positive), ...)
}

#' @rdname plot_roc
#' @export
plot_roc.data.frame <- function(x, models = NULL, palette = "bmj", ...) {
  plot_roc.uniml_result(x, models = models, palette = palette, ...)
}

# Precision-recall -----------------------------------------------------------

# The precision-recall curve. `PRROC::pr.curve()` is the same code
# `mlr3measures::prauc()` calls, so the curve drawn here and the PR AUC reported
# by `calc_metrics()` come from one implementation. If PRROC is not installed the
# curve is computed by hand and the area is its trapezoid.
.pr_curve_rows <- function(model, prob, truth, positive) {
  cleaned <- .clean_binary_inputs(prob, truth, positive = positive)
  if (cleaned$n < 2L || length(unique(cleaned$event)) < 2L) {
    return(NULL)
  }
  event <- cleaned$event
  probability <- cleaned$prob
  n_pos <- sum(event == 1L)
  if (n_pos == 0L || n_pos == cleaned$n) {
    return(NULL)
  }
  fit <- if (requireNamespace("PRROC", quietly = TRUE)) {
    tryCatch(
      PRROC::pr.curve(
        scores.class0 = probability,
        weights.class0 = event,
        curve = TRUE
      ),
      error = function(e) NULL
    )
  } else {
    NULL
  }
  if (!is.null(fit)) {
    # Columns are recall (descending), precision, threshold.
    rows <- data.frame(
      model = model,
      recall = as.numeric(fit$curve[, 1L]),
      precision = as.numeric(fit$curve[, 2L]),
      threshold = as.numeric(fit$curve[, 3L]),
      pr_auc = as.numeric(fit$auc.integral),
      stringsAsFactors = FALSE
    )
  } else {
    cuts <- sort(unique(probability), decreasing = TRUE)
    flagged <- vapply(cuts, function(cut) {
      tp <- sum(probability >= cut & event == 1L)
      fp <- sum(probability >= cut & event == 0L)
      c(recall = tp / n_pos,
        precision = if (tp + fp == 0L) NA_real_ else tp / (tp + fp))
    }, numeric(2))
    rows <- data.frame(
      model = model,
      recall = as.numeric(flagged["recall", ]),
      precision = as.numeric(flagged["precision", ]),
      threshold = cuts,
      stringsAsFactors = FALSE
    )
    rows <- rows[is.finite(rows$precision), , drop = FALSE]
    rows <- rows[order(rows$recall, rows$precision), , drop = FALSE]
    area <- if (nrow(rows) < 2L) {
      NA_real_
    } else {
      sum(diff(rows$recall) *
            (utils::head(rows$precision, -1L) + utils::tail(rows$precision, -1L)) / 2)
    }
    rows$pr_auc <- area
  }
  if (nrow(rows) == 0L) {
    return(NULL)
  }
  rows <- rows[order(rows$recall, rows$precision), , drop = FALSE]
  rows$prevalence <- n_pos / cleaned$n
  rownames(rows) <- NULL
  rows
}

#' Plot precision-recall curves
#'
#' The companion figure to [plot_roc()] for an imbalanced outcome. A rare event
#' makes the ROC curve look reassuring, because the false-positive rate divides by
#' a large number of negatives; the precision-recall curve divides by the predicted
#' positives instead, so it stays honest when the event rate is low.
#'
#' The dotted line is the prevalence: a model with no information sits on it, and a
#' useful model sits above it.
#'
#' @param x A `uniml_result` or prediction collection.
#' @param models Optional subset of model names. `"best"` keeps the top-ranked
#'   model and `"all"` keeps every model.
#' @param positive Positive class label, used when `x` is an mlr3 prediction.
#' @param palette Discrete palette: a `ggsci` name, a colour, or a colour vector.
#' @param theme A `ggplot2` theme applied to the figure.
#' @param title,subtitle,caption Optional figure labels.
#' @param set Data set to plot; defaults to the comparison set used for ranking.
#' @param ... Reserved for method-specific arguments.
#'
#' @return A `ggplot` object.
#' @rdname plot_pr_curve
#' @references
#' Saito T, Rehmsmeier M (2015). The precision-recall plot is more informative than
#' the ROC plot when evaluating binary classifiers on imbalanced datasets. *PLoS
#' ONE*, 10(3), e0118432. doi:10.1371/journal.pone.0118432
#'
#' Davis J, Goadrich M (2006). The relationship between precision-recall and ROC
#' curves. *Proceedings of the 23rd International Conference on Machine Learning*,
#' 233-240. doi:10.1145/1143844.1143874
#' @export
#' @examples
#' predictions <- data.frame(
#'   model = "lr",
#'   prob = c(0.1, 0.4, 0.6, 0.9),
#'   truth = factor(c("no", "no", "yes", "yes"), levels = c("no", "yes")),
#'   positive = "yes"
#' )
#' plot_pr_curve(predictions)
plot_pr_curve.uniml_result <- function(x, models = NULL, palette = "bmj",
                                       theme = theme_uniml(), title = NULL,
                                       subtitle = NULL, caption = NULL,
                                       set = NULL, ...) {
  predictions <- .normalise_predictions(.result_predictions(x))
  predictions <- .filter_prediction_sets(
    predictions,
    if (is.null(set)) .default_plot_set(x) else set
  )
  predictions <- .filter_prediction_models(
    predictions,
    .resolve_plot_models(x, models)
  )
  model_names <- unique(predictions$model)
  curves <- lapply(model_names, function(name) {
    rows <- predictions[predictions$model == name, , drop = FALSE]
    .pr_curve_rows(
      model = name,
      prob = .prediction_values(rows, calibrated = FALSE, fallback = TRUE),
      truth = rows$truth,
      positive = .first_prediction_value(rows$positive)
    )
  })
  curves <- curves[!vapply(curves, is.null, logical(1))]
  if (length(curves) == 0L) {
    .abort("No model had two outcome classes available for a PR curve.")
  }
  curve <- do.call(rbind, curves)
  labels <- unique(curve[, c("model", "pr_auc"), drop = FALSE])
  labels$label <- sprintf("%s (PR AUC = %.3f)", labels$model, labels$pr_auc)
  curve$label <- labels$label[match(curve$model, labels$model)]
  model_levels <- unique(curve$label)
  curve$label <- factor(curve$label, levels = model_levels)
  values <- stats::setNames(
    .uniml_plot_palette(length(model_levels), palette = palette),
    model_levels
  )
  prevalence <- unique(curve$prevalence)[[1L]]
  plot <- ggplot2::ggplot(
    curve,
    ggplot2::aes(x = .data$recall, y = .data$precision, colour = .data$label)
  ) +
    ggplot2::geom_hline(
      yintercept = prevalence, linetype = "dotted", colour = "grey50"
    ) +
    ggplot2::geom_path(linewidth = 0.8) +
    ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::labs(x = "Recall (sensitivity)", y = "Precision (PPV)",
                  colour = NULL) +
    .uniml_scale_color(values = values) +
    theme_uniml()
  .uniml_style_plot(plot, theme, title, subtitle, caption)
}

#' @rdname plot_pr_curve
#' @export
plot_pr_curve.uniml_binary <- function(x, models = NULL, palette = "bmj", ...) {
  plot_pr_curve.uniml_result(
    .as_uniml_result(x),
    models = models,
    palette = palette,
    ...
  )
}

#' @rdname plot_pr_curve
#' @export
plot_pr_curve.PredictionClassif <- function(x, positive = NULL, ...) {
  plot_pr_curve(as_uniml_binary(x, positive = positive), ...)
}

#' @rdname plot_pr_curve
#' @export
plot_pr_curve.data.frame <- function(x, models = NULL, palette = "bmj", ...) {
  plot_pr_curve.uniml_result(x, models = models, palette = palette, ...)
}

.as_uniml_result <- function(x, source_class = NULL) {
  if (inherits(x, "uniml_result")) {
    return(x)
  }
  if (is.null(source_class)) {
    source_class <- class(x)[[1L]]
  }
  structure(
    list(
      data_info = NULL, preprocess = NULL, selection = NULL, tuning = NULL,
      cv = NULL, metrics = NULL, ci = NULL, calibration = NULL, tests = NULL,
      dca = NULL, ranking = NULL, shap = NULL, thresholds = NULL,
      plots = NULL, models = NULL,
      meta = list(predictions = list(model = x), source_class = source_class)
    ),
    class = c("uniml_result", "list")
  )
}

.calibration_curve_rows <- function(model, prob, truth, positive, bins) {
  cleaned <- .clean_binary_inputs(prob, truth, positive = positive)
  if (cleaned$n == 0L) {
    return(NULL)
  }
  bins <- min(as.integer(bins), cleaned$n)
  if (bins < 2L) {
    return(NULL)
  }
  prob <- cleaned$prob
  event <- cleaned$event
  breaks <- unique(stats::quantile(
    prob,
    probs = seq(0, 1, length.out = bins + 1L),
    na.rm = TRUE,
    names = FALSE
  ))
  if (length(breaks) < 2L) {
    return(NULL)
  }
  group <- cut(prob, breaks = breaks, include.lowest = TRUE, labels = FALSE)
  rows <- lapply(sort(unique(group)), function(g) {
    index <- which(group == g)
    data.frame(
      model = model,
      bin = g,
      predicted = mean(prob[index]),
      observed = mean(event[index]),
      n = length(index),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  se <- sqrt(out$observed * (1 - out$observed) / out$n)
  out$lower <- pmax(out$observed - 1.96 * se, 0)
  out$upper <- pmin(out$observed + 1.96 * se, 1)
  out[order(out$predicted), , drop = FALSE]
}

#' Plot probability-calibration curves
#'
#' When the predictions contain both raw and calibrated probabilities, a
#' faceted before/after comparison is produced.
#'
#' @param x A `uniml_result` or prediction collection.
#' @param models Optional subset of model names. `"best"` keeps the top-ranked
#'   model and `"all"` keeps every model.
#' @param positive Positive class label, used when `x` is an mlr3 prediction.
#' @param bins Number of quantile bins.
#' @param annotate Whether to add the calibration slope, intercept, and
#'   calibration-in-the-large to the caption. They are computed on the
#'   predictions themselves, not on the binned points, so they do not depend on
#'   `bins`.
#' @param palette Discrete palette: a `ggsci` name, a colour, or a colour vector.
#' @param theme A `ggplot2` theme applied to the figure.
#' @param title,subtitle,caption Optional figure labels.
#' @param set Data set to plot; defaults to the comparison set used for ranking.
#' @param ... Reserved for method-specific arguments.
#'
#' @return A `ggplot` object.
#' @export
#' @rdname plot_calibration
#' @examples
#' predictions <- data.frame(
#'   model = "lr",
#'   prob = c(0.1, 0.4, 0.6, 0.9),
#'   truth = factor(c("no", "no", "yes", "yes"), levels = c("no", "yes")),
#'   positive = "yes"
#' )
#' plot_calibration(predictions)
plot_calibration.uniml_result <- function(x, models = NULL, bins = 10L,
                                          palette = "bmj", theme = theme_uniml(),
                                          title = NULL, subtitle = NULL,
                                          caption = NULL, set = NULL,
                                          annotate = TRUE, ...) {
  .assert_scalar_number(bins, "bins", integer = TRUE, min = 2)
  accent <- .uniml_plot_palette(1L, palette = palette)
  predictions <- .normalise_predictions(.result_predictions(x))
  predictions <- .filter_prediction_sets(
    predictions,
    if (is.null(set)) .default_plot_set(x) else set
  )
  predictions <- .filter_prediction_models(
    predictions,
    .resolve_plot_models(x, models)
  )
  has_calibrated <- any(!is.na(predictions$calibrated_prob))
  rows <- unlist(lapply(unique(predictions$model), function(name) {
    rows <- predictions[predictions$model == name, , drop = FALSE]
    positive <- .first_prediction_value(rows$positive)
    stages <- list()
    if (any(!is.na(rows$prob))) {
      curve <- .calibration_curve_rows(
        model = name,
        prob = .prediction_values(rows, calibrated = FALSE),
        truth = rows$truth,
        positive = positive,
        bins = bins
      )
      if (!is.null(curve)) {
        curve$stage <- "Raw"
        stages <- c(stages, list(curve))
      }
    }
    if (has_calibrated && any(!is.na(rows$calibrated_prob))) {
      curve <- .calibration_curve_rows(
        model = name,
        prob = .prediction_values(rows, calibrated = TRUE),
        truth = rows$truth,
        positive = positive,
        bins = bins
      )
      if (!is.null(curve)) {
        curve$stage <- "Calibrated"
        stages <- c(stages, list(curve))
      }
    }
    stages
  }), recursive = FALSE)
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0L) {
    .abort("No model had enough observations for a calibration curve.")
  }
  curve <- do.call(rbind, rows)
  curve$model <- factor(curve$model, levels = unique(curve$model))
  curve$stage <- factor(curve$stage, levels = c("Raw", "Calibrated"))
  if (!has_calibrated) {
    curve$stage <- factor(curve$stage, levels = "Raw")
  }
  plot <- ggplot2::ggplot(
    curve,
    ggplot2::aes(x = .data$predicted, y = .data$observed)
  ) +
    ggplot2::geom_abline(
      intercept = 0, slope = 1, linetype = "dashed", colour = "grey60"
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
      width = 0, colour = "grey55"
    ) +
    ggplot2::geom_line(colour = accent, linewidth = 0.8) +
    ggplot2::geom_point(colour = accent, size = 2) +
    ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::labs(x = "Predicted probability", y = "Observed proportion") +
    theme_uniml()
  if (length(unique(curve$stage)) > 1L) {
    plot <- plot + ggplot2::facet_grid(stage ~ model)
  } else if (length(unique(curve$model)) > 1L) {
    plot <- plot + ggplot2::facet_wrap(~model)
  }
  if (isTRUE(annotate) && is.null(caption)) {
    annotation <- .calibration_annotation(predictions)
    if (!is.null(annotation)) {
      caption <- annotation
    }
  }
  .uniml_style_plot(plot, theme, title, subtitle, caption)
}

# The numbers a reader expects next to a calibration plot: slope and intercept
# from `logit(y) ~ logit(p)`, and calibration-in-the-large. Reported per model and
# per stage, and only when they could be estimated.
.calibration_annotation <- function(predictions) {
  rows <- lapply(unique(predictions$model), function(name) {
    subset <- predictions[predictions$model == name, , drop = FALSE]
    positive <- .first_prediction_value(subset$positive)
    stages <- list()
    for (stage in c("raw", "calibrated")) {
      prob <- if (identical(stage, "raw")) {
        subset$prob
      } else {
        subset$calibrated_prob
      }
      if (all(is.na(prob))) {
        next
      }
      cleaned <- .clean_binary_inputs(prob, subset$truth, positive = positive)
      fit <- .calibration_fit(cleaned$prob, cleaned$event)
      if (!is.finite(fit$slope)) {
        next
      }
      label <- if (identical(stage, "raw")) name else paste(name, "(calibrated)")
      stages[[length(stages) + 1L]] <- sprintf(
        "%s: slope %.2f, intercept %.2f, calibration-in-the-large %.2f",
        label, fit$slope, fit$intercept, fit$large
      )
    }
    stages
  })
  rows <- unlist(rows, use.names = FALSE)
  if (length(rows) == 0L) {
    return(NULL)
  }
  paste(rows, collapse = "\n")
}

#' @rdname plot_calibration
#' @export
plot_calibration.uniml_binary <- function(x, models = NULL, bins = 10L,
                                          palette = "bmj", ...) {
  plot_calibration.uniml_result(
    .as_uniml_result(x),
    models = models,
    bins = bins,
    palette = palette,
    ...
  )
}

#' @rdname plot_calibration
#' @export
plot_calibration.PredictionClassif <- function(x, positive = NULL, ...) {
  plot_calibration(as_uniml_binary(x, positive = positive), ...)
}

#' @rdname plot_calibration
#' @export
plot_calibration.data.frame <- function(x, models = NULL, bins = 10L,
                                        palette = "bmj", ...) {
  plot_calibration.uniml_result(
    x,
    models = models,
    bins = bins,
    palette = palette,
    ...
  )
}

.dca_plot_frame <- function(x) {
  if (inherits(x, "uniml_dca") || (is.data.frame(x) && "net_benefit" %in% names(x))) {
    return(as.data.frame(x, stringsAsFactors = FALSE))
  }
  if (.prediction_like(x)) {
    predictions <- .normalise_predictions(x, model = "model")
    return(.dca_prediction_rows(predictions))
  }
  if (inherits(x, "uniml_result")) {
    if (is.data.frame(x$dca) && nrow(x$dca) > 0L) {
      return(as.data.frame(x$dca, stringsAsFactors = FALSE))
    }
    predictions <- .normalise_predictions(.result_predictions(x))
    return(.dca_prediction_rows(predictions))
  }
  .abort("Decision-curve data are required; supply a `uniml_result` or DCA table.")
}

.dca_prediction_rows <- function(predictions) {
  rows <- unlist(lapply(unique(predictions$model), function(name) {
    model_rows <- predictions[predictions$model == name, , drop = FALSE]
    positive <- .first_prediction_value(model_rows$positive)
    out <- list()
    if (any(!is.na(model_rows$prob))) {
      out <- c(out, list(dca(
        .prediction_values(model_rows, calibrated = FALSE),
        model_rows$truth,
        positive = positive,
        model = name,
        calibrated = FALSE
      )))
    }
    if (any(!is.na(model_rows$calibrated_prob))) {
      out <- c(out, list(dca(
        .prediction_values(model_rows, calibrated = TRUE),
        model_rows$truth,
        positive = positive,
        model = name,
        calibrated = TRUE
      )))
    }
    out
  }), recursive = FALSE)
  do.call(rbind, rows)
}

#' Plot decision curves
#'
#' @param x A `uniml_result` or `uniml_dca` table.
#' @param palette Discrete palette: a `ggsci` name, a colour, or a colour vector.
#' @param theme A `ggplot2` theme applied to the figure.
#' @param title,subtitle,caption Optional figure labels.
#' @param ... Reserved for method-specific arguments.
#'
#' @return A `ggplot` object.
#' @export
#' @rdname plot_dca
#' @examples
#' plot_dca(dca(c(0.1, 0.4, 0.6, 0.9), c(0, 0, 1, 1)))
plot_dca.uniml_result <- function(x, palette = "bmj", theme = theme_uniml(),
                                  title = NULL, subtitle = NULL, caption = NULL,
                                  ...) {
  curve <- .dca_plot_frame(x)
  required <- c("model", "strategy", "threshold", "net_benefit")
  missing_columns <- setdiff(required, names(curve))
  if (length(missing_columns) > 0L) {
    .abort(sprintf(
      "Decision-curve data are missing column(s): %s.",
      paste(missing_columns, collapse = ", ")
    ))
  }
  curve$`_series` <- ifelse(
    curve$strategy == "model",
    paste0(curve$model, if (!"calibrated" %in% names(curve)) "" else
      ifelse(as.logical(curve$calibrated), " (calibrated)", " (raw)")),
    curve$model
  )
  series_levels <- unique(curve$`_series`)
  curve$`_series` <- factor(curve$`_series`, levels = series_levels)
  values <- stats::setNames(
    .uniml_plot_palette(length(series_levels), palette = palette),
    series_levels
  )
  plot <- ggplot2::ggplot(
    curve,
    ggplot2::aes(
      x = .data$threshold, y = .data$net_benefit,
      colour = .data$`_series`, linetype = .data$`_series`
    )
  ) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey60", linetype = "dotted") +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::labs(
      x = "Threshold probability", y = "Net benefit", colour = NULL,
      linetype = NULL
    ) +
    .uniml_scale_color(values = values) +
    ggplot2::scale_linetype_discrete() +
    theme_uniml()
  .uniml_style_plot(plot, theme, title, subtitle, caption)
}

#' @rdname plot_dca
#' @export
plot_dca.uniml_binary <- function(x, palette = "bmj", ...) {
  plot_dca.uniml_result(x, palette = palette, ...)
}

#' @rdname plot_dca
#' @export
plot_dca.uniml_dca <- function(x, palette = "bmj", ...) {
  plot_dca.uniml_result(x, palette = palette, ...)
}

.confusion_rows <- function(model, prob, truth, positive, threshold,
                            calibrated = FALSE) {
  metrics <- .calc_binary_metrics(
    probs = prob,
    truth = truth,
    threshold = threshold,
    positive = positive,
    metrics = "all"
  )
  total <- metrics$tp + metrics$tn + metrics$fp + metrics$fn
  out <- data.frame(
    model = model,
    calibrated = calibrated,
    prediction = factor(
      c("Positive", "Negative", "Positive", "Negative"),
      levels = c("Positive", "Negative")
    ),
    reference = factor(
      c("Positive", "Positive", "Negative", "Negative"),
      levels = c("Positive", "Negative")
    ),
    count = c(metrics$tp, metrics$fn, metrics$fp, metrics$tn)
  )
  out$percent <- if (total > 0L) 100 * out$count / total else NA_real_
  out$label <- ifelse(
    is.na(out$percent),
    as.character(out$count),
    sprintf("%d\n(%.1f%%)", out$count, out$percent)
  )
  out$threshold <- metrics$threshold
  out
}

#' Plot confusion-matrix heatmaps
#'
#' Draws counts and row percentages, faceted by model when more than one model
#' is supplied.
#'
#' @param x A `uniml_result` or prediction collection.
#' @param models Optional subset of model names. `"best"` keeps the top-ranked
#'   model and `"all"` keeps every model.
#' @param positive Positive class label, used when `x` is an mlr3 prediction.
#' @param threshold Classification threshold. When `NULL`, the configured
#'   threshold method is used.
#' @param calibrated Whether to draw the calibrated probabilities.
#' @param palette Continuous fill palette name or colour.
#' @param theme A `ggplot2` theme applied to the figure.
#' @param title,subtitle,caption Optional figure labels.
#' @param set Data set to plot; defaults to the comparison set used for ranking.
#' @param ... Reserved for method-specific arguments.
#'
#' @return A `ggplot` object.
#' @export
#' @rdname plot_confusion
#' @examples
#' predictions <- data.frame(
#'   model = "lr",
#'   prob = c(0.1, 0.4, 0.6, 0.9),
#'   truth = factor(c("no", "no", "yes", "yes"), levels = c("no", "yes")),
#'   positive = "yes"
#' )
#' plot_confusion(predictions, threshold = 0.5)
plot_confusion.uniml_result <- function(x, models = NULL, threshold = NULL,
                                        calibrated = FALSE, palette = "bmj",
                                        theme = theme_uniml(), title = NULL,
                                        subtitle = NULL, caption = NULL,
                                        set = NULL, ...) {
  .assert_scalar_logical(calibrated, "calibrated")
  if (!is.null(threshold)) {
    .assert_scalar_number(threshold, "threshold", min = 0, max = 1)
  }
  predictions <- .normalise_predictions(.result_predictions(x))
  predictions <- .filter_prediction_sets(
    predictions,
    if (is.null(set)) .default_plot_set(x) else set
  )
  predictions <- .filter_prediction_models(
    predictions,
    .resolve_plot_models(x, models)
  )
  if (nrow(predictions) == 0L ||
      !any(!is.na(.prediction_values(predictions, calibrated = calibrated)))) {
    .abort("No predictions are available for the requested calibration stage.")
  }
  rows <- lapply(unique(predictions$model), function(name) {
    model_rows <- predictions[predictions$model == name, , drop = FALSE]
    .confusion_rows(
      model = name,
      prob = .prediction_values(model_rows, calibrated = calibrated),
      truth = model_rows$truth,
      positive = .first_prediction_value(model_rows$positive),
      threshold = threshold,
      calibrated = calibrated
    )
  })
  curve <- do.call(rbind, rows)
  curve$model <- factor(curve$model, levels = unique(curve$model))
  plot <- ggplot2::ggplot(
    curve,
    ggplot2::aes(x = .data$prediction, y = .data$reference, fill = .data$count)
  ) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = .data$label), colour = "grey5") +
    ggplot2::scale_fill_gradient(
      low = "#F2F2F2",
      high = .uniml_plot_palette(3L, palette = palette)[1L],
      name = "Count"
    ) +
    ggplot2::labs(x = "Predicted class", y = "Reference class") +
    ggplot2::coord_equal() +
    theme_uniml() +
    ggplot2::theme(panel.grid = ggplot2::element_blank())
  if (length(unique(curve$model)) > 1L) {
    plot <- plot + ggplot2::facet_wrap(~model)
  }
  plot <- .uniml_style_plot(plot, theme, title, subtitle, caption)
  plot + ggplot2::theme(panel.grid = ggplot2::element_blank())
}

#' @rdname plot_confusion
#' @export
plot_confusion.uniml_binary <- function(x, models = NULL, threshold = NULL,
                                        calibrated = FALSE, palette = "bmj", ...) {
  plot_confusion.uniml_result(
    .as_uniml_result(x),
    models = models,
    threshold = threshold,
    calibrated = calibrated,
    palette = palette,
    ...
  )
}

#' @rdname plot_confusion
#' @export
plot_confusion.PredictionClassif <- function(x, positive = NULL, ...) {
  plot_confusion(as_uniml_binary(x, positive = positive), ...)
}

#' @rdname plot_confusion
#' @export
plot_confusion.data.frame <- function(x, models = NULL, threshold = NULL,
                                      calibrated = FALSE, palette = "bmj", ...) {
  plot_confusion.uniml_result(
    x,
    models = models,
    threshold = threshold,
    calibrated = calibrated,
    palette = palette,
    ...
  )
}

#' Plot a uniml result
#'
#' Dispatches to the performance plots implemented by the plotting layer.
#'
#' @param x A `uniml_result` object.
#' @param type Plot type: `"roc"`, `"calibration"`, `"dca"`, `"confusion"`,
#'   `"shap_importance"`, `"shap_beeswarm"`, `"shap_dependence"`,
#'   `"shap_waterfall"`, or `"shap_force"`.
#' @param ... Arguments passed to the selected plot function.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' \dontrun{
#' plot(result, type = "roc")
#' }
plot.uniml_result <- function(x, type = "roc", ...) {
  validate_uniml_result(x)
  .assert_choice(type, "type", c(
    "roc", "calibration", "dca", "confusion",
    "shap_importance", "shap_beeswarm", "shap_dependence", "shap_waterfall",
    "shap_force"
  ))
  switch(
    type,
    roc = plot_roc(x, ...),
    calibration = plot_calibration(x, ...),
    dca = plot_dca(x, ...),
    confusion = plot_confusion(x, ...),
    shap_importance = plot_shap_importance(x, ...),
    shap_beeswarm = plot_shap_beeswarm(x, ...),
    shap_dependence = plot_shap_dependence(x, ...),
    shap_waterfall = plot_shap_waterfall(x, ...),
    shap_force = plot_shap_force(x, ...)
  )
}
