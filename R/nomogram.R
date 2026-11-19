# L8 Nomogram -----------------------------------------------------------------
#
# A nomogram turns a linear model into the paper figure every clinical cohort
# knows: one scale per variable, a total-points scale, and the probability that
# scale maps to. It is only defined for linear models on the logit (logistic
# regression) scale - a random forest has no coefficients to read off, and a
# multiclass outcome has no single logit. Those cases are refused with a
# message that says which models work, never silently replaced.
#
# The math stays honest to what the workflow fitted: the model lives on the
# encoded columns, so numeric variables are mapped back to their original
# units through the scaling step (a linear re-parametrisation keeps every
# coefficient exact), and one-hot columns are mapped back to their variable
# through the encoder map. Zero coefficients (LASSO shrank them away) simply
# do not appear - that is the model being honest about what it uses.

.nomogram_supported <- function(result, model) {
  key <- if (identical(model, "best")) {
    result$ranking$model[[which.min(result$ranking$rank)]]
  } else {
    model
  }
  entry <- result$models[[key]]
  if (is.null(entry)) {
    .abort(sprintf("Unknown model \"%s\".", key))
  }
  predict_type <- entry$learner$predict_type
  if (!identical(predict_type, "prob")) {
    .abort(sprintf(
      "A nomogram needs a model with class probabilities; \"%s\" predicts %s.",
      key, predict_type
    ))
  }
  task_type <- result$meta$spec$task_type
  if (!identical(task_type, "binary")) {
    .abort(sprintf(
      paste(
        "A nomogram is defined for a single logit, so binary outcomes only;",
        "this result is \"%s\". For a multiclass outcome, draw the binary",
        "nomogram of each one-vs-rest model you care about."
      ),
      task_type
    ))
  }
  list(key = key, learner = entry$learner)
}

# Coefficients of the fitted linear model on the encoded columns.
.nomogram_coefficients <- function(learner) {
  model <- learner$model
  coefficients <- tryCatch(
    {
      if (!is.null(model$glmnet.fit)) {
        # classif.cv_glmnet: the fitted cv.glmnet carries the coefficients at
        # the selected penalty. Keep the names: "(Intercept)" plus the column
        # names drive everything downstream.
        raw <- glmnet::coef.glmnet(model$glmnet.fit, s = model$lambda.min)
        stats::setNames(
          as.numeric(raw),
          dimnames(raw)[[1L]]
        )
      } else {
        raw <- stats::coef(model)
        stats::setNames(as.numeric(raw), names(raw))
      }
    },
    error = function(e) NULL
  )
  if (is.null(coefficients) || length(coefficients) == 0L) {
    .abort(paste0(
      "Could not extract coefficients from the fitted model; nomograms are ",
      "only implemented for logistic regression (the \"lr\" and \"log_reg\" ",
      "learners)."
    ))
  }
  coefficients
}

#' Nomogram table for a fitted logistic model
#'
#' Returns the building blocks of the nomogram figure: every variable with its
#' coefficient mapped back to the original units, the point scales, and the
#' mapping from total points to probability. Use [plot_nomogram()] for the
#' figure; this function is the auditable version of the same content.
#'
#' @param result A binary `uniml_result` from [run_workflow()].
#' @param model A model key, or `"best"` for the top-ranked model. Only
#'   logistic-regression models (`lr`, `log_reg`) are supported: a nomogram
#'   reads coefficients, and a random forest has none.
#'
#' @return A list with `variables` (one row per variable level with its
#'   contribution), `intercept`, `points_per_unit` (contribution per original
#'   unit for numeric variables), `total` (the total-points scale), and
#'   `probability` (total points mapped through the inverse logit).
#' @references
#' Steyerberg EW (2019). *Clinical Prediction Models: A Practical Approach to
#' Development, Validation, and Updating*, 2nd edition. Springer.
#' doi:10.1007/978-3-030-16399-0
#'
#' Harrell FE Jr (2015). *Regression Modeling Strategies*, 2nd edition.
#' Springer. doi:10.1007/978-3-319-19425-7
#' @export
#' @examples
#' \dontrun{
#' result <- run_workflow(spec, train, validation)
#' table <- nomogram_table(result, model = "lr")
#' table$variables
#' }
nomogram_table <- function(result, model = "best") {
  supported <- .nomogram_supported(result, model)
  coefficients <- .nomogram_coefficients(supported$learner)
  encoded <- names(coefficients)
  intercept <- coefficients[["(Intercept)"]]
  if (is.null(intercept)) {
    intercept <- 0
  }
  features <- coefficients[setdiff(encoded, "(Intercept)")]
  encoded <- names(features)

  # Numeric variables: undo the scaling so the figure speaks the units the
  # clinic measures in. The scaling parameters come from the preprocessor.
  scale_params <- result$preprocess$steps$scale$params$differences
  encoder_levels <- result$preprocess$steps$encode$params$levels
  if (is.null(encoder_levels)) {
    encoder_levels <- list()
  }

  # Map each encoded column to its source variable: "<variable>_<level>" for
  # encoded columns, the column itself for everything else.
  column_to_variable <- list()
  for (variable in names(encoder_levels)) {
    for (level in encoder_levels[[variable]]) {
      column_to_variable[[paste0(variable, "_", level)]] <- variable
    }
  }
  variable_of <- vapply(encoded, function(column) {
    mapped <- column_to_variable[[column]]
    if (is.null(mapped)) column else mapped
  }, character(1))
  observed_low <- result$preprocess$train

  # Raw display ranges: the observed range of the raw variable, recovered
  # through the scaling parameters for scaled numerics, or read from the
  # (unscaled) training frame when no scaling was applied.
  raw_range <- function(column) {
    difference <- scale_params[[column]]
    if (!is.null(difference)) {
      return(c(difference$center - 2 * difference$scale,
               difference$center + 2 * difference$scale))
    }
    values <- observed_low[[column]]
    if (is.null(values)) {
      return(c(0, 1))
    }
    stats::quantile(values, c(0.025, 0.975), names = FALSE)
  }
  rows <- list()
  for (variable in unique(variable_of)) {
    columns <- encoded[variable_of == variable]
    keep <- vapply(columns, function(column) features[[column]] != 0, logical(1))
    columns <- columns[keep]
    if (length(columns) == 0L) {
      next
    }
    is_categorical <- variable %in% names(encoder_levels)
    if (!is_categorical) {
      # A numeric variable on its original scale.
      column <- columns[[1L]]
      difference <- scale_params[[column]]
      contribution_per_raw <- if (!is.null(difference)) {
        features[[column]] / difference$scale
      } else {
        features[[column]]
      }
      range <- raw_range(column)
      rows[[length(rows) + 1L]] <- data.frame(
        variable = variable,
        level = NA_character_,
        value_low = range[1L],
        value_high = range[2L],
        points_low = contribution_per_raw * range[1L],
        points_high = contribution_per_raw * range[2L],
        coefficient = features[[column]],
        stringsAsFactors = FALSE
      )
    } else {
      # Categorical variables are shown relative to their reference level
      # (the first level), the way published nomograms read: the reference
      # contributes 0 points, other levels contribute the difference. With
      # full-level encoding the raw coefficients are split across redundant
      # columns, so differences - not raw coefficients - are the meaningful,
      # sample-invariant quantity.
      reference_column <- paste0(variable, "_", encoder_levels[[variable]][[1L]])
      reference <- if (reference_column %in% encoded) {
        features[[reference_column]]
      } else {
        0
      }
      for (column in columns) {
        level <- sub(paste0("^", variable, "_"), "", column)
        contribution <- features[[column]] - reference
        rows[[length(rows) + 1L]] <- data.frame(
          variable = variable,
          level = level,
          value_low = 0,
          value_high = 1,
          # Every level is drawn from the reference (0 points) out to its
          # difference; the reference level itself is a point at 0.
          points_low = 0,
          points_high = contribution,
          coefficient = features[[column]],
          stringsAsFactors = FALSE
        )
      }
    }
  }
  variables <- do.call(rbind, rows)
  if (nrow(variables) == 0L) {
    .abort("Every coefficient was shrunk to zero; there is nothing to draw.")
  }
  total_low <- intercept + sum(pmin(variables$points_low, 0))
  total_high <- intercept + sum(pmax(variables$points_high, 0))
  list(
    model = supported$key,
    variables = variables,
    intercept = intercept,
    total = c(low = total_low, high = total_high),
    probability = c(
      low = stats::plogis(total_low),
      high = stats::plogis(total_high)
    )
  )
}

#' Nomogram figure for a fitted logistic model
#'
#' Draws the classical nomogram: one scale per variable, total points, and the
#' probability that scale maps to. Only binary logistic-regression models are
#' supported - see [nomogram_table()] for the same content as data.
#'
#' @param result A binary `uniml_result` from [run_workflow()].
#' @param model A model key, or `"best"`.
#' @param palette Colour for the scales.
#' @param theme A `ggplot2` theme.
#' @param title Optional title.
#'
#' @return A `ggplot` object.
#' @references
#' Harrell FE Jr (2015). *Regression Modeling Strategies*, 2nd edition.
#' Springer. doi:10.1007/978-3-319-19425-7
#'
#' Steyerberg EW (2019). *Clinical Prediction Models: A Practical Approach to
#' Development, Validation, and Updating*, 2nd edition. Springer.
#' doi:10.1007/978-3-030-16399-0
#' @export
#' @examples
#' \dontrun{
#' result <- run_workflow(spec, train, validation)
#' plot_nomogram(result, model = "lr")
#' }
plot_nomogram <- function(result, model = "best", palette = "bmj",
                          theme = theme_uniml(), title = NULL) {
  table <- nomogram_table(result, model)
  variables <- table$variables
  # Normalize every variable scale onto 0-100 points, rms-style: the widest
  # contribution sets the ruler.
  span <- variables$points_high - variables$points_low
  zero_span <- span <= 0
  span[zero_span] <- 1
  variables$axis_low <- 0
  variables$axis_high <- ifelse(zero_span, 0,
                                100 * (variables$points_high -
                                         variables$points_low) / max(span))
  # Rows: variables from top, then total points, then probability.
  variable_rows <- seq_len(nrow(variables))
  variables$y <- max(variable_rows) + 1L - variable_rows
  total_y <- 0.25
  probability_y <- -0.75
  total_low <- 0
  total_high <- 100
  # Total points map: axis points * max_span + total_low_logit.
  logit_low <- table$total[["low"]]
  logit_span <- table$total[["high"]] - table$total[["low"]]
  prob_labels <- stats::plogis(logit_low + (seq(0, 100, by = 10) / 100) *
                                 logit_span)

  colours <- .uniml_plot_palette(1, palette = palette)
  # Bottom rows: total points and the probability that scale maps to.
  axis_points <- seq(0, 100, by = 10)
  total_labels <- round(logit_low + axis_points / 100 * logit_span, 1)
  probability_labels <- sprintf("%.2f", stats::plogis(
    logit_low + axis_points / 100 * logit_span
  ))
  rows_frame <- data.frame(
    y = c(variables$y, total_y, probability_y),
    x_low = c(variables$axis_low, rep(0, 2)),
    x_high = c(variables$axis_high, rep(100, 2))
  )
  tick_labels <- data.frame(
    y = rep(c(total_y, probability_y), each = length(axis_points)),
    x = rep(axis_points, 2),
    label = c(as.character(total_labels), probability_labels)
  )
  graph <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = rows_frame,
      ggplot2::aes(x = .data$x_low, xend = .data$x_high,
                   y = .data$y, yend = .data$y),
      colour = colours[[1]],
      linewidth = 0.9,
      lineend = "butt"
    ) +
    ggplot2::geom_segment(
      data = variables,
      ggplot2::aes(x = .data$axis_low, xend = .data$axis_high,
                   y = .data$y, yend = .data$y),
      colour = "grey55",
      linewidth = 0.4
    ) +
    ggplot2::geom_point(
      data = variables,
      ggplot2::aes(x = .data$axis_low, y = .data$y),
      colour = colours[[1]],
      size = 1.6
    ) +
    ggplot2::geom_point(
      data = variables,
      ggplot2::aes(x = .data$axis_high, y = .data$y),
      colour = colours[[1]],
      size = 1.6
    ) +
    ggplot2::geom_text(
      data = tick_labels,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
      size = 2.6,
      vjust = 1.4,
      colour = "grey25"
    ) +
    # End-point labels in the units the clinic measures in.
    ggplot2::geom_text(
      data = variables,
      ggplot2::aes(
        x = .data$axis_low, y = .data$y,
        label = ifelse(is.na(.data$level), sprintf("%.0f", .data$value_low),
                       .data$level)
      ),
      size = 2.6,
      vjust = -0.8,
      colour = "grey25"
    ) +
    ggplot2::geom_text(
      data = variables,
      ggplot2::aes(
        x = .data$axis_high, y = .data$y,
        label = ifelse(
          is.na(.data$level), sprintf("%.0f", .data$value_high),
          ifelse(abs(.data$points_high) < 1e-9, "", sprintf("%+.2f", .data$points_high))
        )
      ),
      size = 2.6,
      vjust = -0.8,
      colour = "grey25"
    ) +
    ggplot2::scale_y_continuous(
      breaks = c(variables$y, total_y, probability_y),
      labels = c(
        ifelse(is.na(variables$level), variables$variable,
               paste0(variables$variable, ": ", variables$level)),
        "Total points", "Probability"
      ),
      expand = ggplot2::expansion(mult = 0.08)
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 100, by = 10),
      labels = seq(0, 100, by = 10),
      expand = ggplot2::expansion(mult = 0.02)
    ) +
    ggplot2::labs(
      x = "Points",
      y = NULL,
      title = if (is.null(title)) {
        sprintf("Nomogram - model \"%s\"", table$model)
      } else {
        title
      }
    ) +
    theme
  .uniml_style_plot(graph, theme, title, subtitle = NULL, caption = NULL)
}
