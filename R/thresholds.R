# SHAP zero-crossing thresholds ---------------------------------------------

.threshold_matrix <- function(shap_result) {
  if (!is.list(shap_result)) {
    .abort("`shap_result` must be an `uniml_shap` object or a compatible list.")
  }
  values <- shap_result$values_matrix_raw
  if (is.null(values)) {
    values <- shap_result$values_matrix_std
  }
  features <- shap_result$feature_values_raw
  if (is.null(features)) {
    features <- shap_result$feature_values_std
  }
  if (is.null(values) || is.null(features)) {
    .abort("`shap_result` must contain SHAP values and feature values.")
  }
  values <- as.data.frame(values, stringsAsFactors = FALSE, check.names = FALSE)
  features <- as.data.frame(features, stringsAsFactors = FALSE, check.names = FALSE)
  if ("model" %in% names(values)) {
    values$model <- NULL
  }
  if ("model" %in% names(features)) {
    features$model <- NULL
  }
  if (nrow(values) != nrow(features)) {
    .abort("SHAP values and feature values must have the same number of rows.")
  }
  list(values = values, features = features)
}

.threshold_extract_vector <- function(table, column, name) {
  if (!column %in% names(table)) {
    .abort(sprintf("`%s` was not found in the SHAP result.", name))
  }
  table[[column]]
}

.threshold_interaction_groups <- function(x, interaction) {
  if (is.null(interaction)) {
    return(list(all = seq_along(x)))
  }
  if (.continuous_column(x)) {
    quantiles <- stats::quantile(
      x,
      probs = c(0.25, 0.5, 0.75),
      na.rm = TRUE,
      names = FALSE
    )
    breaks <- unique(c(-Inf, quantiles, Inf))
    if (length(breaks) < 4L) {
      return(list())
    }
    group <- cut(
      x,
      breaks = breaks,
      labels = c("Q1", "Q2", "Q3")[seq_len(length(breaks) - 1L)],
      include.lowest = TRUE
    )
    return(split(seq_along(x), group))
  }
  split(seq_along(x), as.character(x))
}

.threshold_crossings_once <- function(values, shap_values, span, grid_n) {
  keep <- !is.na(values) & !is.na(shap_values)
  values <- as.numeric(values[keep])
  shap_values <- as.numeric(shap_values[keep])
  if (length(values) < 3L || length(unique(values)) < 2L) {
    return(list(
      crossings = numeric(),
      directions = character(),
      curve = "no clear threshold"
    ))
  }

  fit <- tryCatch(
    stats::loess(shap_values ~ values, span = span),
    error = function(e) e
  )
  if (inherits(fit, "error")) {
    return(list(
      crossings = numeric(),
      directions = character(),
      curve = "no clear threshold",
      error = conditionMessage(fit)
    ))
  }
  grid <- seq(min(values), max(values), length.out = grid_n)
  predicted <- as.numeric(stats::predict(
    fit,
    newdata = data.frame(values = grid)
  ))
  predicted[!is.finite(predicted)] <- NA_real_
  if (all(is.na(predicted))) {
    return(list(
      crossings = numeric(),
      directions = character(),
      curve = "no clear threshold"
    ))
  }

  crossings <- numeric()
  directions <- character()
  for (index in seq_len(length(grid) - 1L)) {
    y1 <- predicted[index]
    y2 <- predicted[index + 1L]
    if (is.na(y1) || is.na(y2)) {
      next
    }
    change <- (y1 == 0 || y2 == 0 || sign(y1) != sign(y2))
    if (!change) {
      next
    }
    crossing <- if (y1 == 0) {
      grid[index]
    } else if (y2 == 0) {
      grid[index + 1L]
    } else {
      grid[index] - y1 * (grid[index + 1L] - grid[index]) / (y2 - y1)
    }
    if (length(crossings) > 0L &&
        any(abs(crossings - crossing) <= sqrt(.Machine$double.eps))) {
      next
    }
    crossings <- c(crossings, crossing)
    directions <- c(
      directions,
      if (is.na(y1) || y1 <= 0) "negative_to_positive" else "positive_to_negative"
    )
  }

  curve <- if (length(crossings) == 0L) {
    "no clear threshold"
  } else if (length(crossings) == 1L) {
    "monotone"
  } else if (length(unique(directions[seq_len(min(2L, length(directions)))])) == 2L) {
    "U-shaped"
  } else {
    "monotone"
  }
  list(
    crossings = crossings,
    directions = directions,
    curve = curve
  )
}

.threshold_crossing_class <- function(directions) {
  if (length(directions) == 0L) {
    return(character())
  }
  if (length(directions) == 1L) {
    return("single")
  }
  ifelse(directions == "positive_to_negative", "lower", "upper")
}

.threshold_one_stratum <- function(values, shap_values, target, interaction,
                                   stratum, span, grid_n, min_n) {
  keep <- !is.na(values) & !is.na(shap_values)
  n <- sum(keep)
  if (n < min_n) {
    return(list(zero = NULL, paper = NULL, skipped = TRUE))
  }
  result <- .threshold_crossings_once(
    values = values,
    shap_values = shap_values,
    span = span,
    grid_n = grid_n
  )
  if (length(result$crossings) == 0L) {
    paper <- data.frame(
      target = target,
      interaction = if (is.null(interaction)) NA_character_ else interaction,
      stratum = stratum,
      n = as.integer(n),
      curve_shape = result$curve,
      lower = NA_real_,
      upper = NA_real_,
      single = NA_real_,
      stringsAsFactors = FALSE
    )
    return(list(zero = NULL, paper = paper, skipped = FALSE))
  }
  classes <- .threshold_crossing_class(result$directions)
  zero <- data.frame(
    target = target,
    interaction = if (is.null(interaction)) NA_character_ else interaction,
    stratum = stratum,
    n = as.integer(n),
    crossing = as.numeric(result$crossings),
    crossing_class = classes,
    direction = result$directions,
    stringsAsFactors = FALSE
  )
  values_by_class <- function(class_name) {
    index <- which(classes == class_name)
    if (length(index) == 0L) NA_real_ else result$crossings[index[1L]]
  }
  paper <- data.frame(
    target = target,
    interaction = if (is.null(interaction)) NA_character_ else interaction,
    stratum = stratum,
    n = as.integer(n),
    curve_shape = result$curve,
    lower = values_by_class("lower"),
    upper = values_by_class("upper"),
    single = values_by_class("single"),
    stringsAsFactors = FALSE
  )
  list(zero = zero, paper = paper, skipped = FALSE)
}

#' Extract SHAP-zero clinical thresholds
#'
#' Fits a LOESS curve to the SHAP value of `target` and locates zero crossings,
#' optionally within strata of an interaction variable.
#'
#' @param shap_result A `uniml_shap` object or a list with SHAP and feature
#'   value matrices.
#' @param target Continuous target feature.
#' @param interaction Optional continuous or categorical interaction feature.
#' @param min_n Minimum observations required in a stratum.
#' @param min_unique Minimum number of unique target values.
#' @param span LOESS smoothing span.
#' @param main_grid Number of grid points for the main curve.
#' @param stratum_grid Number of grid points for interaction strata.
#'
#' @return A list with `zero_crossings` and `paper_ready`.
#' @export
#' @examples
#' shap <- list(
#'   values_matrix_raw = matrix(seq(-1, 1, length.out = 40),
#'     ncol = 1L, dimnames = list(NULL, "x")
#'   ),
#'   feature_values_raw = data.frame(x = seq(0, 1, length.out = 40))
#' )
#' extract_thresholds(shap, target = "x")
extract_thresholds <- function(shap_result, target, interaction = NULL,
                               min_n = 30L, min_unique = 10L, span = 0.75,
                               main_grid = 1000L, stratum_grid = 500L) {
  .assert_scalar_character(target, "target")
  if (!is.null(interaction)) {
    .assert_scalar_character(interaction, "interaction")
  }
  .assert_scalar_number(min_n, "min_n", integer = TRUE, min = 1)
  .assert_scalar_number(min_unique, "min_unique", integer = TRUE, min = 1)
  .assert_scalar_number(span, "span", min = 0, max = 1)
  if (span <= 0 || span > 1) {
    .abort("`span` must be greater than 0 and at most 1.")
  }
  .assert_scalar_number(main_grid, "main_grid", integer = TRUE, min = 10)
  .assert_scalar_number(stratum_grid, "stratum_grid", integer = TRUE, min = 10)

  tables <- .threshold_matrix(shap_result)
  values <- .threshold_extract_vector(tables$features, target, "target")
  shap_values <- .threshold_extract_vector(
    tables$values,
    target,
    "SHAP value for `target`"
  )
  if (!is.numeric(values)) {
    .abort("`target` must be a continuous numeric feature.")
  }
  complete <- !is.na(values) & !is.na(shap_values)
  if (sum(complete) <= min_n || length(unique(values[complete])) < min_unique) {
    .abort(sprintf(
      "`target` requires more than %d complete rows and at least %d unique values.",
      min_n,
      min_unique
    ))
  }

  interaction_values <- if (is.null(interaction)) {
    NULL
  } else {
    .threshold_extract_vector(
      tables$features,
      interaction,
      "interaction"
    )
  }
  groups <- .threshold_interaction_groups(
    if (is.null(interaction)) values else interaction_values,
    interaction
  )
  if (length(groups) == 0L) {
    warning("No usable interaction strata were found.", call. = FALSE)
    groups <- list(all = seq_along(values))
  }

  warnings <- character()
  zero_rows <- list()
  paper_rows <- list()
  for (stratum in names(groups)) {
    index <- groups[[stratum]]
    if (identical(stratum, "all") && !is.null(interaction)) {
      next
    }
    result <- .threshold_one_stratum(
      values = values[index],
      shap_values = shap_values[index],
      target = target,
      interaction = interaction,
      stratum = stratum,
      span = span,
      grid_n = if (is.null(interaction)) main_grid else stratum_grid,
      min_n = min_n
    )
    if (isTRUE(result$skipped)) {
      warnings <- c(
        warnings,
        sprintf("Stratum \"%s\" had fewer than %d rows and was skipped.", stratum, min_n)
      )
      next
    }
    if (!is.null(result$zero)) {
      zero_rows[[length(zero_rows) + 1L]] <- result$zero
    }
    paper_rows[[length(paper_rows) + 1L]] <- result$paper
  }
  if (length(warnings) > 0L) {
    warning(paste(warnings, collapse = "\n"), call. = FALSE)
  }

  empty_zero <- data.frame(
    target = character(),
    interaction = character(),
    stratum = character(),
    n = integer(),
    crossing = numeric(),
    crossing_class = character(),
    direction = character(),
    stringsAsFactors = FALSE
  )
  empty_paper <- data.frame(
    target = character(),
    interaction = character(),
    stratum = character(),
    n = integer(),
    curve_shape = character(),
    lower = numeric(),
    upper = numeric(),
    single = numeric(),
    stringsAsFactors = FALSE
  )
  list(
    zero_crossings = if (length(zero_rows)) do.call(rbind, zero_rows) else empty_zero,
    paper_ready = if (length(paper_rows)) do.call(rbind, paper_rows) else empty_paper
  )
}
