# L7 multiclass plots --------------------------------------------------------
#
# A multiclass task has no single positive class, so the binary figures do not
# carry over unchanged. Discrimination is shown as one-vs-rest ROC curves, one
# per class, and the confusion matrix becomes a k-by-k table. Calibration and
# decision curves are deliberately absent: a multiclass calibration curve needs
# a stated convention (per-class reliability, top-label, or a multinomial
# score), and none of them is a drop-in replacement for the binary plot.

.multiclass_roc_curve <- function(prob, truth, classes) {
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
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0L) {
    return(NULL)
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' @rdname plot_roc
#' @export
plot_roc.uniml_multiclass <- function(x, palette = "bmj", theme = theme_uniml(),
                                      title = NULL, subtitle = NULL,
                                      caption = NULL, ...) {
  curve <- .multiclass_roc_curve(x$prob, x$truth, x$classes)
  if (is.null(curve)) {
    .abort("Every class needs at least one observation to draw an ROC curve.")
  }
  curve$class <- factor(curve$class, levels = x$classes)
  plot <- ggplot2::ggplot(
    curve,
    ggplot2::aes(
      x = 1 - .data$specificity,
      y = .data$sensitivity,
      colour = .data$class
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
      values = .uniml_plot_palette(length(x$classes), palette = palette),
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
#' @export
plot_calibration.uniml_multiclass <- function(x, ...) {
  .abort(
    paste(
      "`plot_calibration()` has no multiclass method yet: a multiclass",
      "calibration curve needs a stated convention (per-class reliability,",
      "top-label, or a multinomial score). Use `calc_metrics()` for the",
      "multiclass Brier score and log loss in the meantime."
    )
  )
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
