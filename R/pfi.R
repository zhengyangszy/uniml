# L8 Permutation feature importance --------------------------------------------
#
# Permutation feature importance answers the same "which feature matters"
# question as SHAP with a completely different mechanism: shuffle one column,
# watch the score drop. The two disagreeing is itself informative — SHAP
# attributes credit under a cooperative-game model, PFI measures what the
# model actually uses on this data. The tutorial literature recommends
# reporting both.
#
# The implementation sits on `predict_uniml()`, so the preprocessing, the
# calibration, and the deployment threshold a new patient would go through are
# exactly what the permutation perturbs. Binary results are scored with the
# AUC of the model's probability; multiclass results with the Brier score
# (where a drop is an improvement, and the sign says so).

.pfi_score <- function(result, newdata, key, metric) {
  prediction <- predict_uniml(result, newdata = newdata, model = key)
  outcome <- result$meta$spec$outcome
  truth <- newdata[[outcome]]
  prob <- if (is.null(prediction$calibrated_prob)) {
    prediction$prob
  } else {
    prediction$calibrated_prob
  }
  if (identical(result$meta$spec$task_type, "binary")) {
    positive <- result$meta$spec$positive
    sorted <- order(prob)
    ranks <- rank(prob, ties.method = "average")
    cases <- as.character(truth) == as.character(positive)
    n_cases <- sum(cases)
    n_controls <- sum(!cases)
    auc <- (sum(ranks[cases]) - n_cases * (n_cases + 1) / 2) /
      (n_cases * n_controls)
    list(score = auc, higher_is_better = TRUE)
  } else {
    labels <- levels(truth)
    observed <- match(as.character(truth), labels)
    classes <- colnames(prob)
    squared <- (prob[, classes, drop = FALSE] -
                  model.matrix(~ 0 + factor(observed)))^2
    list(score = mean(rowSums(squared)), higher_is_better = FALSE)
  }
}

#' Permutation feature importance
#'
#' Shuffles one feature at a time in `newdata` and measures how much the
#' model's score changes, averaged over `B` shuffles. Answers "what does this
#' model actually use on this data" — the mechanistic counterpart to
#' [explain_shap()], and the two are worth reporting side by side.
#'
#' The model's own preprocessing, calibration, and deployment threshold are
#' applied to every permutation, exactly as they would be for a new patient.
#' Binary results are scored with the AUC of the model's probability (higher
#' is better, importance = drop); multiclass results with the Brier score
#' (lower is better, importance = increase).
#'
#' @param result A `uniml_result` from [run_workflow()].
#' @param newdata Data frame to permute. Should contain the outcome column and
#'   every original feature; rows are used as they are.
#' @param model A model key, or `"best"`.
#' @param B Number of permutations per feature.
#' @param seed Random seed.
#' @param metric Scoring metric: `"auc"` (binary, default) or `"brier"`
#'   (multiclass).
#'
#' @return A data frame with `feature`, `baseline` (the score on untouched
#'   data), `permuted` (the mean score under permutation), `importance`, and
#'   `sd` (the across-permutation standard deviation).
#' @references
#' Fisher A, Rudin C, Dominici F (2019). All models are wrong, but many are
#' useful: learning a variable's importance by computing an output sensitivity
#' to the rest of the model over holdout data. *Journal of Machine Learning
#' Research*, 20(81), 1-51.
#'
#' Bischl B, Sonabend R, Kotthoff L, Lang M (2023). *Applied Machine Learning
#' Using mlr3 in R*. CRC Press. doi:10.1201/9781003402848
#' @export
#' @examples
#' \dontrun{
#' result <- run_workflow(spec, train, validation)
#' importance <- pfi_uniml(result, validation, model = "best", B = 20)
#' plot_pfi(importance)
#' }
pfi_uniml <- function(result, newdata, model = "best", B = 20,
                      seed = 123L, metric = "auc") {
  task_type <- result$meta$spec$task_type
  metric <- match.arg(metric, c("auc", "brier"))
  if (identical(task_type, "binary") && !identical(metric, "auc")) {
    .abort("A binary result is scored with `metric = \"auc\"`.")
  }
  if (!identical(task_type, "binary") && !identical(metric, "brier")) {
    .abort("A multiclass result is scored with `metric = \"brier\"`.")
  }
  .assert_scalar_number(B, "B", integer = TRUE, min = 2)
  .assert_scalar_number(seed, "seed", integer = TRUE)
  if (!is.data.frame(newdata)) {
    .abort("`newdata` must be a data frame.")
  }
  key <- if (identical(model, "best")) {
    result$ranking$model[[which.min(result$ranking$rank)]]
  } else {
    model
  }
  if (is.null(result$models[[key]])) {
    .abort(sprintf("Unknown model \"%s\".", key))
  }
  features <- setdiff(
    names(newdata),
    c(result$meta$spec$outcome, result$meta$spec$id, result$meta$spec$exclude)
  )
  if (length(features) == 0L) {
    .abort("`newdata` has no feature columns to permute.")
  }
  baseline <- .pfi_score(result, newdata, key, metric)$score
  set.seed(as.integer(seed))
  rows <- lapply(features, function(feature) {
    scores <- vapply(seq_len(B), function(b) {
      permuted <- newdata
      permuted[[feature]] <- sample(newdata[[feature]])
      .pfi_score(result, permuted, key, metric)$score
    }, numeric(1))
    data.frame(
      feature = feature,
      baseline = baseline,
      permuted = mean(scores),
      importance = if (identical(task_type, "binary")) {
        baseline - mean(scores)
      } else {
        mean(scores) - baseline
      },
      sd = stats::sd(scores),
      stringsAsFactors = FALSE
    )
  })
  table <- do.call(rbind, rows)
  table <- table[order(-table$importance), ]
  rownames(table) <- NULL
  table
}

#' Plot permutation feature importance
#'
#' A horizontal bar chart of [pfi_uniml()]'s importance, error bars showing the
#' across-permutation standard deviation.
#'
#' @param table The data frame returned by [pfi_uniml()].
#' @param max_display How many features to show.
#' @param palette Discrete palette: a `ggsci` name, a colour, or a vector.
#' @param theme A `ggplot2` theme.
#' @param title,subtitle,caption Optional figure labels.
#'
#' @return A `ggplot` object.
#' @references
#' Fisher A, Rudin C, Dominici F (2019). All models are wrong, but many are
#' useful: learning a variable's importance by computing an output sensitivity
#' to the rest of the model over holdout data. *Journal of Machine Learning
#' Research*, 20(81), 1-51.
#' @export
#' @examples
#' \dontrun{
#' importance <- pfi_uniml(result, validation, model = "best", B = 20)
#' plot_pfi(importance)
#' }
plot_pfi <- function(table, max_display = 12L, palette = "bmj",
                     theme = theme_uniml(), title = NULL, subtitle = NULL,
                     caption = NULL) {
  if (!is.data.frame(table)) {
    .abort("`table` must be the data frame returned by `pfi_uniml()`.")
  }
  required <- c("feature", "importance", "sd")
  missing_columns <- setdiff(required, names(table))
  if (length(missing_columns) > 0L) {
    .abort(sprintf(
      "The PFI table is missing column(s): %s.",
      paste(missing_columns, collapse = ", ")
    ))
  }
  .assert_scalar_number(max_display, "max_display", integer = TRUE, min = 1)
  table <- table[order(table$importance), ]
  if (nrow(table) > max_display) {
    table <- table[seq_len(max_display), ]
  }
  table$feature <- factor(table$feature, levels = table$feature)
  colours <- .uniml_plot_palette(1, palette = palette)
  graph <- ggplot2::ggplot(
    table,
    ggplot2::aes(x = .data$importance, y = .data$feature)
  ) +
    ggplot2::geom_col(fill = colours[[1]], width = 0.62) +
    ggplot2::geom_errorbarh(
      ggplot2::aes(
        xmin = .data$importance - .data$sd,
        xmax = pmax(.data$importance + .data$sd, 0)
      ),
      width = 0.18,
      colour = "grey30"
    ) +
    ggplot2::labs(
      x = "Permutation importance (score change)",
      y = NULL,
      title = if (is.null(title)) "Permutation feature importance" else title
    ) +
    theme
  .uniml_style_plot(graph, theme, title, subtitle, caption)
}
