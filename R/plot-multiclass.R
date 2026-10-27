# L7 multiclass plots --------------------------------------------------------
#
# A multiclass task has no single positive class, so the binary figures do not
# carry over unchanged. Discrimination is shown as one-vs-rest ROC curves, one
# per class, and the confusion matrix becomes a k-by-k table. Calibration and
# decision curves are deliberately absent: a multiclass calibration curve needs
# a stated convention (per-class reliability, top-label, or a multinomial
# score), and none of them is a drop-in replacement for the binary plot.

.multiclass_roc_curve <- function(prob, truth, classes, micro = TRUE) {
  rows <- lapply(classes, function(class) {
    if (requireNamespace("pROC", quietly = TRUE)) {
      event <- as.integer(truth == class)
      if (length(unique(event)) < 2L) {
        return(NULL)
      }
      roc <- pROC::roc(
        event,
        prob[, class],
        quiet = TRUE,
        direction = "<"
      )
      data.frame(
        class = class,
        specificity = rev(roc$specificities),
        sensitivity = rev(roc$sensitivities),
        stringsAsFactors = FALSE
      )
    } else {
      thresholds <- sort(unique(c(0, prob[, class], 1)), decreasing = TRUE)
      event <- truth == class
      positive <- sum(event)
      negative <- sum(!event)
      if (positive == 0L || negative == 0L) {
        return(NULL)
      }
      curve <- data.frame(
        class = class,
        specificity = vapply(
          thresholds,
          function(cut) sum(!event & prob[, class] < cut) / negative,
          numeric(1)
        ),
        sensitivity = vapply(
          thresholds,
          function(cut) sum(event & prob[, class] >= cut) / positive,
          numeric(1)
        ),
        stringsAsFactors = FALSE
      )
      curve[order(curve$specificity, curve$sensitivity), , drop = FALSE]
    }
  })
  if (isTRUE(micro)) {
    # Micro average: every (patient, class) pair as one binary observation.
    n <- nrow(prob)
    event <- rep(as.character(truth), times = length(classes)) ==
      rep(classes, each = n)
    if (length(unique(event)) == 2L) {
      micro_curve <- .micro_roc_rows(event, as.numeric(prob))
      if (!is.null(micro_curve)) {
        rows[[length(rows) + 1L]] <- micro_curve
      }
    }
  }
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0L) {
    return(NULL)
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

.micro_roc_rows <- function(event, score) {
  if (requireNamespace("pROC", quietly = TRUE)) {
    roc <- tryCatch(
      pROC::roc(as.integer(event), score, quiet = TRUE, direction = "<"),
      error = function(e) NULL
    )
    if (is.null(roc)) {
      return(NULL)
    }
    return(data.frame(
      class = "micro-average",
      specificity = rev(roc$specificities),
      sensitivity = rev(roc$sensitivities),
      stringsAsFactors = FALSE
    ))
  }
  thresholds <- sort(unique(c(0, score, 1)), decreasing = TRUE)
  positive <- sum(event)
  negative <- sum(!event)
  curve <- data.frame(
    class = "micro-average",
    specificity = vapply(thresholds, function(cut) {
      sum(!event & score < cut) / negative
    }, numeric(1)),
    sensitivity = vapply(thresholds, function(cut) {
      sum(event & score >= cut) / positive
    }, numeric(1)),
    stringsAsFactors = FALSE
  )
  curve[order(curve$specificity, curve$sensitivity), , drop = FALSE]
}

#' @rdname plot_roc
#' @export
plot_roc.uniml_multiclass <- function(x, palette = "bmj", theme = theme_uniml(),
                                      title = NULL, subtitle = NULL,
                                      caption = NULL, micro = TRUE, ...) {
  curve <- .multiclass_roc_curve(x$prob, x$truth, x$classes, micro = micro)
  if (is.null(curve)) {
    .abort("Every class needs at least one observation to draw an ROC curve.")
  }
  levels <- c(x$classes, if ("micro-average" %in% curve$class) "micro-average")
  curve$class <- factor(curve$class, levels = levels)
  metrics <- .calc_multiclass_metrics(
    x$prob,
    x$truth,
    metrics = c("auc", "auc_ovr", "micro_auc")
  )
  if (is.null(subtitle)) {
    subtitle <- sprintf(
      "Hand-Till %.3f; macro one-vs-rest %.3f; micro %.3f",
      metrics$auc, metrics$auc_ovr, metrics$micro_auc
    )
  }
  plot <- ggplot2::ggplot(
    curve,
    ggplot2::aes(
      x = 1 - .data$specificity,
      y = .data$sensitivity,
      colour = .data$class,
      linetype = .data$class
    )
  ) +
    ggplot2::geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed",
      colour = "grey60"
    ) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::scale_colour_manual(
      values = .uniml_plot_palette(length(levels), palette = palette),
      name = "Class"
    ) +
    ggplot2::scale_linetype_manual(
      values = stats::setNames(
        c(rep("solid", length(x$classes)), "dashed")[seq_along(levels)],
        levels
      ),
      name = "Class"
    ) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      x = "1 - Specificity (one-vs-rest)",
      y = "Sensitivity (one-vs-rest)"
    ) +
    theme_uniml()
  .uniml_style_plot(plot, theme, title, subtitle, caption)
}

#' @rdname plot_confusion
#' @export
plot_confusion.uniml_multiclass <- function(x, palette = "bmj",
                                            theme = theme_uniml(),
                                            title = NULL, subtitle = NULL,
                                            caption = NULL, ...) {
  metrics <- .calc_multiclass_metrics(
    x$prob,
    x$truth,
    metrics = c("confusion", "accuracy")
  )
  counts <- metrics$confusion
  frame <- data.frame(
    reference = rep(rownames(counts), times = ncol(counts)),
    prediction = rep(colnames(counts), each = nrow(counts)),
    count = as.numeric(counts),
    stringsAsFactors = FALSE
  )
  frame$reference <- factor(frame$reference, levels = rev(rownames(counts)))
  frame$prediction <- factor(frame$prediction, levels = colnames(counts))
  frame$label <- as.character(frame$count)

  if (is.null(subtitle)) {
    subtitle <- sprintf("Accuracy %.3f", metrics$accuracy)
  }
  plot <- ggplot2::ggplot(
    frame,
    ggplot2::aes(x = .data$prediction, y = .data$reference, fill = .data$count)
  ) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = .data$label), colour = "grey5") +
    ggplot2::scale_fill_gradient(
      low = "#F2F2F2",
      high = .uniml_plot_palette(3L, palette = palette)[[1L]],
      name = "Count"
    ) +
    ggplot2::labs(
      x = "Predicted class",
      y = "Reference class"
    ) +
    ggplot2::coord_equal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank()) +
    theme_uniml()
  .uniml_style_plot(plot, theme, title, subtitle, caption)
}

#' @rdname plot_calibration
#' @param scope Which calibration to draw: `"top_label"` (confidence against
#'   accuracy), `"class"` (one one-vs-rest curve per class), or `"both"`. See
#'   [multiclass_calibration()] for how the two are defined.
#' @export
plot_calibration.uniml_multiclass <- function(x, scope = c("top_label", "class",
                                                           "both"),
                                              bins = 10L, palette = "bmj",
                                              theme = theme_uniml(), title = NULL,
                                              subtitle = NULL, caption = NULL,
                                              annotate = TRUE, ...) {
  scope <- match.arg(scope)
  .assert_scalar_number(bins, "bins", integer = TRUE, min = 2)
  calibration <- multiclass_calibration(x$prob, x$truth, bins = bins)
  curve <- calibration$reliability
  if (nrow(curve) == 0L) {
    .abort("No calibration group could be formed; the predictions do not vary.")
  }
  if (!identical(scope, "both")) {
    curve <- curve[curve$scope == scope, , drop = FALSE]
  }
  curve$series <- ifelse(
    curve$scope == "top_label", "Top label", curve$class
  )
  curve$panel <- ifelse(
    curve$scope == "top_label",
    "Top label",
    "One-vs-rest (class-wise)"
  )
  levels_used <- unique(curve$series)
  colours <- .uniml_plot_palette(length(levels_used), palette = palette)
  if (identical(levels_used, "Top label")) {
    colours <- .uniml_plot_palette(1L, palette = palette)
  } else if ("Top label" %in% levels_used) {
    # The top-label curve is the reference, so it gets the first colour and the
    # classes share the rest of the palette.
    colours <- .uniml_plot_palette(length(levels_used), palette = palette)
  }
  curve$series <- factor(curve$series, levels = levels_used)
  plot <- ggplot2::ggplot(
    curve,
    ggplot2::aes(x = .data$predicted, y = .data$observed, colour = .data$series)
  ) +
    ggplot2::geom_abline(
      intercept = 0, slope = 1, linetype = "dashed", colour = "grey60"
    ) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 2) +
    ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::scale_colour_manual(values = colours, name = NULL) +
    ggplot2::labs(
      x = "Predicted probability",
      y = if (identical(scope, "top_label")) {
        "Observed proportion of the predicted class"
      } else {
        "Observed proportion"
      }
    ) +
    theme_uniml()
  if (identical(scope, "both")) {
    plot <- plot + ggplot2::facet_wrap(~panel)
  }
  if (isTRUE(annotate) && is.null(caption)) {
    summary <- calibration$summary
    number <- function(value) {
      if (is.na(value)) "NA" else formatC(value, format = "f", digits = 3)
    }
    caption <- sprintf(
      paste(
        "Top label: ECE %s, MCE %s (level bias %s).",
        "Class-wise ECE %s over %d classes, %d bins."
      ),
      number(summary$ece),
      number(summary$mce),
      number(summary$level_bias),
      number(summary$ece_classwise),
      summary$n_classes,
      summary$bins
    )
  }
  .uniml_style_plot(plot, theme, title, subtitle, caption)
}

#' @rdname plot_dca
#' @export
plot_dca.uniml_multiclass <- function(x, ...) {
  .abort(
    paste(
      "`plot_dca()` is not defined for multiclass outcomes: net benefit",
      "compares a single event against no event. Run one-vs-rest with",
      "`as_uniml_binary()` and `dca()` for each class instead."
    )
  )
}
