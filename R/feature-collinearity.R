# L3 collinearity removal ---------------------------------------------------

.collinearity_matrix <- function(data) {
  if (is.data.frame(data)) {
    if (ncol(data) == 0L) {
      .abort("`data` must contain at least one feature column.")
    }
    if (any(!vapply(data, is.numeric, logical(1)))) {
      .abort("All feature columns must be numeric.")
    }
    matrix <- as.matrix(data)
  } else if (is.matrix(data) && is.numeric(data)) {
    if (ncol(data) == 0L) {
      .abort("`data` must contain at least one feature column.")
    }
    matrix <- data
  } else {
    .abort("`data` must be a numeric data frame or numeric matrix.")
  }

  if (is.null(colnames(matrix)) || any(!nzchar(colnames(matrix))) ||
      anyDuplicated(colnames(matrix))) {
    .abort("`data` must have unique, non-empty column names.")
  }
  storage.mode(matrix) <- "double"
  if (anyNA(matrix) || any(!is.finite(matrix))) {
    .abort("`data` must contain complete finite numeric values.")
  }
  if (nrow(matrix) < 2L) {
    .abort("`data` must contain at least two complete rows.")
  }
  if (any(apply(matrix, 2L, stats::sd) == 0)) {
    .abort("`data` must not contain zero-variance feature columns.")
  }
  matrix
}

.collinearity_vif_fallback <- function(matrix) {
  vif <- vapply(seq_len(ncol(matrix)), function(index) {
    y <- matrix[, index]
    x <- matrix[, -index, drop = FALSE]
    fit <- stats::lm.fit(cbind(1, x), y)
    total <- sum((y - mean(y))^2)
    r2 <- if (total > 0) 1 - sum(fit$residuals^2) / total else 0
    if (r2 >= 1 - .Machine$double.eps^0.5) Inf else 1 / (1 - r2)
  }, numeric(1))
  stats::setNames(vif, colnames(matrix))
}

.collinearity_vif <- function(matrix) {
  if (ncol(matrix) < 2L) {
    return(stats::setNames(1, colnames(matrix)))
  }

  values <- tryCatch(
    as.numeric(car::vif(as.data.frame(matrix, check.names = FALSE))),
    error = function(e) NULL
  )
  if (is.null(values) || length(values) != ncol(matrix)) {
    return(.collinearity_vif_fallback(matrix))
  }
  values[!is.finite(values)] <- Inf
  stats::setNames(values, colnames(matrix))
}

.collinearity_pair <- function(correlation, threshold) {
  absolute <- abs(correlation)
  diag(absolute) <- 0
  candidates <- which(upper.tri(absolute) & absolute >= threshold, arr.ind = TRUE)
  if (nrow(candidates) == 0L) {
    return(NULL)
  }
  values <- absolute[candidates]
  first <- colnames(absolute)[candidates[, 1L]]
  second <- colnames(absolute)[candidates[, 2L]]
  order_index <- order(-values, first, second)
  selected <- candidates[order_index[1L], , drop = FALSE]
  list(
    first = colnames(absolute)[selected[1L, 1L]],
    second = colnames(absolute)[selected[1L, 2L]],
    value = absolute[selected[1L, 1L], selected[1L, 2L]]
  )
}

.collinearity_pair_removal <- function(matrix, pair) {
  absolute <- abs(stats::cor(matrix))
  candidates <- c(pair$first, pair$second)
  mean_correlation <- vapply(candidates, function(feature) {
    other <- setdiff(colnames(absolute), feature)
    mean(absolute[feature, other])
  }, numeric(1))
  order_index <- order(-mean_correlation, candidates)
  removed <- candidates[order_index[1L]]
  list(
    removed = removed,
    value = pair$value,
    mean_correlation = unname(mean_correlation[removed])
  )
}

.collinearity_details <- function(features, selected, history) {
  details <- data.frame(
    feature = features,
    selected = features %in% selected,
    removal_method = NA_character_,
    removal_value = NA_real_,
    removal_step = NA_integer_,
    stringsAsFactors = FALSE
  )
  if (nrow(history) == 0L) {
    return(details)
  }
  for (index in seq_len(nrow(history))) {
    row <- history[index, , drop = FALSE]
    details$removal_method[details$feature == row$removed] <- row$method
    details$removal_value[details$feature == row$removed] <- row$value
    details$removal_step[details$feature == row$removed] <- row$step
  }
  details
}

#' Remove collinear features
#'
#' Iteratively removes one feature from the strongest absolute correlation
#' pair and/or the feature with the largest variance inflation factor (VIF).
#' Correlations and VIF values are recomputed after each removal.
#'
#' @param data A numeric data frame or matrix with named columns.
#' @param cor_threshold Optional absolute-correlation threshold in `[0, 1]`.
#' @param vif_threshold Optional VIF threshold, at least 1.
#'
#' @return A list containing `selected_features`, `removed_features`, removal
#'   `details`, `history`, the final `correlation` matrix, and final `vif`
#'   values when VIF removal was requested.
#' @export
#' @examples
#' data <- data.frame(
#'   x1 = c(1, 2, 3, 4, 5),
#'   x2 = c(1.1, 2.0, 3.1, 4.0, 5.1),
#'   x3 = c(5, 3, 4, 1, 2)
#' )
#' remove_collinear(data, cor_threshold = 0.99)
remove_collinear <- function(data, cor_threshold = NULL, vif_threshold = NULL) {
  matrix <- .collinearity_matrix(data)
  if (!is.null(cor_threshold)) {
    .assert_scalar_number(cor_threshold, "cor_threshold", min = 0, max = 1)
  }
  if (!is.null(vif_threshold)) {
    .assert_scalar_number(vif_threshold, "vif_threshold", min = 1)
  }

  all_features <- colnames(matrix)
  current <- all_features
  history <- data.frame(
    step = integer(),
    method = character(),
    removed = character(),
    value = numeric(),
    threshold = numeric(),
    feature_1 = character(),
    feature_2 = character(),
    stringsAsFactors = FALSE
  )

  if (!is.null(cor_threshold) || !is.null(vif_threshold)) {
    repeat {
      if (length(current) < 2L) {
        break
      }
      current_matrix <- matrix[, current, drop = FALSE]
      removal <- NULL
      method <- NULL

      if (!is.null(cor_threshold)) {
        pair <- .collinearity_pair(stats::cor(current_matrix), cor_threshold)
        if (!is.null(pair)) {
          removal <- .collinearity_pair_removal(current_matrix, pair)
          method <- "correlation"
        }
      }
      if (is.null(removal) && !is.null(vif_threshold)) {
        .check_packages("car")
        vif <- .collinearity_vif(current_matrix)
        candidates <- names(vif)[is.infinite(vif) | vif > vif_threshold]
        if (length(candidates) > 0L) {
          order_index <- order(-vif[candidates], candidates)
          feature <- candidates[order_index[1L]]
          removal <- list(removed = feature, value = unname(vif[feature]))
          method <- "vif"
        }
      }
      if (is.null(removal)) {
        break
      }

      history <- rbind(
        history,
        data.frame(
          step = nrow(history) + 1L,
          method = method,
          removed = removal$removed,
          value = removal$value,
          threshold = if (identical(method, "correlation")) cor_threshold else vif_threshold,
          feature_1 = if (identical(method, "correlation")) pair$first else removal$removed,
          feature_2 = if (identical(method, "correlation")) pair$second else NA_character_,
          stringsAsFactors = FALSE
        )
      )
      current <- setdiff(current, removal$removed)
    }
  }

  selected_matrix <- matrix[, current, drop = FALSE]
  correlation <- stats::cor(selected_matrix)
  vif <- if (!is.null(vif_threshold)) .collinearity_vif(selected_matrix) else NULL
  list(
    selected_features = current,
    removed_features = setdiff(all_features, current),
    details = .collinearity_details(all_features, current, history),
    history = history,
    correlation = correlation,
    vif = vif
  )
}
