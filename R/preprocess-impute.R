# L2 imputation ---------------------------------------------------------------

#' Apply a single preprocessing step
#'
#' @param step A `uniml_step` object.
#' @param newdata A data frame or `uniml_data` object.
#' @param ... Method-specific arguments.
#'
#' @return The transformed data.
#' @export
#' @examples
#' train <- data.frame(x = c(1, NA, 3), y = c(0, 1, 0))
#' pre <- fit_preprocessor(train, outcome = "y")
#' apply_step(pre$steps$impute, train)
apply_step <- function(step, newdata, ...) {
  UseMethod("apply_step")
}

#' @rdname apply_step
#' @export
apply_step.default <- function(step, newdata, ...) {
  .no_method("apply_step", step)
}

#' @rdname apply_step
#' @export
apply_step.uniml_step_impute <- function(step, newdata, ...) {
  apply_imputer(step, newdata, ...)
}

#' @rdname apply_step
#' @export
apply_step.uniml_step_scale <- function(step, newdata, ...) {
  apply_scale(step, newdata, ...)
}

#' @rdname apply_step
#' @export
apply_step.uniml_step_encode <- function(step, newdata, ...) {
  apply_encode(step, newdata, ...)
}

#' @rdname apply_step
#' @export
apply_step.uniml_step_balance <- function(step, newdata, ...) {
  apply_balance(step, newdata, ...)
}

#' Fit an imputer
#'
#' Fits a missing-value imputer on training data. Supported methods are
#' `"median_mode"`, `"mice"`, `"missForest"`, `"missRanger"`, and `"none"`.
#' All-NA columns are dropped with a warning.
#'
#' @param data A data frame.
#' @param method Imputation method.
#' @param outcome Outcome column name, excluded from imputation.
#' @param ordered_as Ordered-factor handling.
#' @param m Number of imputations for `mice`.
#' @param max_iter Iterations passed to `mice`.
#' @param seed Random seed.
#' @param verbose Verbosity level.
#'
#' @return A fitted `uniml_step_impute` step.
#' @export
#' @examples
#' fit_imputer(data.frame(x = c(1, NA, 3), y = c(0, 1, 0)), outcome = "y")
fit_imputer <- function(data, method = c("median_mode", "mice", "missForest",
                                         "missRanger", "none"),
                        outcome = NULL, ordered_as = c("ordinal", "numeric"),
                        m = 5L, max_iter = 5L, seed = 123, verbose = 1) {
  if (!is.data.frame(data)) {
    .abort("`data` must be a data frame.")
  }
  method <- .norm_method(method, "method")
  .assert_choice(method, "method",
                 c("median_mode", "mice", "missForest", "missRanger", "none"))
  ordered_as <- match.arg(ordered_as)
  .assert_scalar_number(m, "m", integer = TRUE, min = 1)
  .assert_scalar_number(max_iter, "max_iter", integer = TRUE, min = 1)

  diagnostics <- diagnose_missing(data)
  all_na <- names(data)[vapply(data, function(z) all(is.na(z)), logical(1))]
  dropped <- intersect(all_na, setdiff(names(data), outcome))
  work <- data[, setdiff(names(data), dropped), drop = FALSE]
  columns <- .columns_with_missing(work)
  columns <- setdiff(columns, outcome)

  if (length(dropped) > 0L) {
    warning(sprintf(
      "Dropped all-NA column(s) before imputation: %s.",
      paste(dropped, collapse = ", ")
    ), call. = FALSE)
  }

  params <- list(
    fill = NULL,
    columns = columns,
    dropped = dropped,
    diagnostics = diagnostics,
    seed = seed
  )

  if (length(columns) > 0L) {
    types <- detect_var_types(work, outcome = outcome, ordered_as = ordered_as)
    type_map <- stats::setNames(types$type, types$column)
    factor_columns <- intersect(
      names(work)[vapply(work, .factorial, logical(1))],
      names(work)
    )
    predictor_columns <- setdiff(names(work), outcome)
    imputation_data <- work[, predictor_columns, drop = FALSE]

    if (!identical(method, "none")) {
      # Single-value fallback derived from the training data. It is used when
      # the fitted model (mice/missForest/missRanger) cannot be projected onto
      # new rows, so that `apply_imputer()` never touches observed values and
      # works for arbitrary `nrow(newdata)`.
      params$fallback <- lapply(work[columns], function(z) {
        if (is.numeric(z) && !is.factor(z) && !is.logical(z)) {
          stats::median(z, na.rm = TRUE)
        } else {
          .mode_value(z)
        }
      })
      # Column types drive type restoration after `mice` / forest imputation,
      # which coerce logical and integer columns to numeric.
      params$type_map <- type_map
      params$restore_types <- vapply(work[columns], function(z) {
        if (is.factor(z)) {
          "factor"
        } else if (is.logical(z)) {
          "logical"
        } else if (is.integer(z)) {
          "integer"
        } else {
          "numeric"
        }
      }, character(1))
      params$fit_levels <- lapply(work[columns], function(z) {
        if (is.factor(z)) levels(z) else NULL
      })
    }

    if (identical(method, "none")) {
      # nothing to fit
    } else if (identical(method, "median_mode")) {
      fill <- lapply(work[columns], function(z) {
        if (is.numeric(z) && !is.factor(z) && !is.logical(z)) {
          stats::median(z, na.rm = TRUE)
        } else {
          .mode_value(z)
        }
      })
      params$fill <- fill
      params$type_map <- type_map
    } else if (identical(method, "mice")) {
      .check_packages("mice")
      methods <- vapply(imputation_data, function(z) {
        if (is.factor(z) || is.character(z) || is.logical(z)) {
          if (nlevels(factor(z)) <= 2L) "logreg" else "polyreg"
        } else {
          "pmm"
        }
      }, character(1))
      # `mice` skips integer columns and stores logical columns numerically;
      # impute as numeric and restore the original types afterwards.
      for (column in names(imputation_data)) {
        if (is.integer(imputation_data[[column]]) && !is.factor(imputation_data[[column]])) {
          imputation_data[[column]] <- as.numeric(imputation_data[[column]])
        }
      }
      params$mice <- mice::mice(
        imputation_data,
        m = m,
        maxit = max_iter,
        method = methods,
        seed = seed,
        printFlag = FALSE
      )
      params$imputation_data <- imputation_data
      params$factor_columns <- intersect(unique(c(columns, factor_columns)),
                                         names(imputation_data))
    } else if (identical(method, "missForest")) {
      .check_packages("missForest")
      if (length(columns) < 2L || nrow(imputation_data) < 3L) {
        params$fill <- lapply(work[columns], function(z) {
          if (is.numeric(z) && !is.factor(z) && !is.logical(z)) {
            stats::median(z, na.rm = TRUE)
          } else {
            .mode_value(z)
          }
        })
      } else {
        params$missForest <- missForest::missForest(
          imputation_data, maxiter = max_iter, verbose = FALSE
        )
        params$factor_columns <- intersect(unique(c(columns, factor_columns)),
                                           names(imputation_data))
      }
    } else if (identical(method, "missRanger")) {
      .check_packages("missRanger")
      if (length(columns) < 2L || nrow(imputation_data) < 3L) {
        params$fill <- lapply(work[columns], function(z) {
          if (is.numeric(z) && !is.factor(z) && !is.logical(z)) {
            stats::median(z, na.rm = TRUE)
          } else {
            .mode_value(z)
          }
        })
      } else {
        params$missRanger <- missRanger::missRanger(
          imputation_data, maxiter = max_iter, seed = seed, verbose = 0
        )
        params$factor_columns <- intersect(unique(c(columns, factor_columns)),
                                           names(imputation_data))
      }
    }
  }

  .new_step("impute", method = method, params = params, data = work)
}

#' Apply an imputation step
#'
#' S3 generic dispatching on the fitted step; applied by
#' [apply_preprocessor()].
#'
#' @param object A fitted `uniml_step_impute` object.
#' @param newdata A data frame or `uniml_data` object.
#' @param ... Reserved for future use.
#'
#' @return The imputed data, matching the class of `newdata`.
#' @export
#' @examples
#' train <- data.frame(x = c(1, NA, 3), y = c(0, 1, 0))
#' step <- fit_imputer(train, outcome = "y")
#' apply_imputer(step, train)
apply_imputer <- function(object, newdata, ...) {
  UseMethod("apply_imputer")
}

#' @rdname apply_imputer
#' @export
apply_imputer.default <- function(object, newdata, ...) {
  .no_method("apply_imputer", object)
}

.impute_source_values <- function(object, column, index = 1L) {
  if (!is.null(object$params$mice)) {
    completed <- mice::complete(object$params$mice, action = index)
    return(completed[[column]])
  }
  if (!is.null(object$params$missForest)) {
    return(object$params$missForest$ximp[[column]])
  }
  if (!is.null(object$params$missRanger)) {
    return(as.data.frame(object$params$missRanger)[[column]])
  }
  NULL
}

.same_observed_values <- function(fit_data, data, columns) {
  if (nrow(fit_data) != nrow(data)) {
    return(FALSE)
  }
  for (column in columns) {
    a <- fit_data[[column]]
    b <- data[[column]]
    if (!identical(is.na(a), is.na(b))) {
      return(FALSE)
    }
    keep <- !is.na(a)
    if (any(keep) && !identical(as.character(a[keep]), as.character(b[keep]))) {
      return(FALSE)
    }
  }
  TRUE
}

.restore_imputed_column <- function(values, type = "numeric", levels = NULL) {
  if (identical(type, "factor")) {
    return(factor(as.character(values), levels = levels))
  }
  if (identical(type, "logical")) {
    return(as.logical(values))
  }
  if (identical(type, "integer")) {
    return(as.integer(round(as.numeric(values))))
  }
  values
}

.impute_frame <- function(object, newdata, index = 1L) {
  had_class <- inherits(newdata, "uniml_data")
  input <- newdata
  data <- .input_data(newdata)
  fit_data <- .prep_step_data(object)
  expected <- setdiff(names(fit_data), object$params$dropped)
  missing_columns <- setdiff(expected, names(data))
  if (length(missing_columns) > 0L) {
    .abort(sprintf(
      "Column(s) required by the fitted imputer are missing: %s.",
      paste(missing_columns, collapse = ", ")
    ))
  }
  data <- data[, expected, drop = FALSE]

  columns <- object$params$columns
  if (length(columns) > 0L) {
    # Capture the incoming column types before any values are filled, so
    # restoration is driven by the original training types rather than the
    # (possibly numeric) values returned by `mice` or the forest imputers.
    restore_types <- object$params$restore_types
    original_types <- vapply(data[columns], function(z) {
      if (is.factor(z)) {
        "factor"
      } else if (is.logical(z)) {
        "logical"
      } else if (is.integer(z)) {
        "integer"
      } else {
        "numeric"
      }
    }, character(1))
    for (column in intersect(names(original_types), names(restore_types))) {
      original_types[[column]] <- restore_types[[column]]
    }
    if (!is.null(object$params$fill)) {
      for (column in intersect(columns, names(object$params$fill))) {
        fill <- object$params$fill[[column]]
        missing <- is.na(data[[column]])
        if (any(missing)) {
          data[[column]][missing] <- fill
        }
      }
    } else if (!is.null(object$params$fallback)) {
      use_model <- .same_observed_values(fit_data, data, columns)
      for (column in columns) {
        missing <- is.na(data[[column]])
        if (!any(missing)) {
          next
        }
        values <- if (use_model) {
          .impute_source_values(object, column, index = index)
        } else {
          NULL
        }
        if (is.null(values)) {
          # Fallback to training medians/modes; observed values are never
          # overwritten and arbitrary `nrow(newdata)` is supported.
          data[[column]][missing] <- object$params$fallback[[column]]
          next
        }
        type <- original_types[[column]]
        values <- .restore_imputed_column(
          values,
          type = type,
          levels = object$params$fit_levels[[column]]
        )
        data[[column]][missing] <- values[missing]
      }
    }
  }

  rownames(data) <- NULL
  if (had_class) .restore_prep(input, data) else data
}

.imputation_index <- function(object) {
  index <- object$params$imputation_index
  if (is.null(index) || length(index) != 1L || is.na(index) || index < 1L) {
    return(1L)
  }
  as.integer(index)
}

.apply_imputer_impl <- function(object, newdata) {
  .impute_frame(object, newdata, index = .imputation_index(object))
}

#' @rdname apply_imputer
#' @export
apply_imputer.uniml_step_impute <- function(object, newdata, ...) {
  .impute_frame(object, newdata, index = .imputation_index(object))
}

.impute_completed_data <- function(imputer, data, index = 1L) {
  if (!inherits(imputer, "uniml_step_impute")) {
    .abort("`imputer` must be a fitted `uniml_step_impute` object.")
  }
  input <- data
  frames <- .input_data(data)
  raw <- .prep_step_data(imputer)
  templates <- raw[rep(seq_len(nrow(raw)), each = nrow(frames)), , drop = FALSE]
  rownames(templates) <- NULL
  completed <- if (is.null(imputer$params$mice)) {
    .apply_imputer_impl(imputer, templates)
  } else {
    mice::complete(imputer$params$mice, action = index)
  }
  columns <- names(completed)
  if (anyDuplicated(columns) > 0L || any(!nzchar(columns))) {
    .abort("The completed imputation data has invalid column names.")
  }
  for (column in intersect(columns, names(raw))) {
    values <- completed[[column]]
    reference <- raw[[column]]
    if (is.factor(reference)) {
      values <- factor(as.character(values), levels = levels(reference))
    } else if (is.logical(reference)) {
      values <- as.logical(values)
    } else if (is.integer(reference) && is.numeric(values)) {
      values <- as.integer(values)
    }
    completed[[column]] <- values
  }
  completed <- as.data.frame(completed, stringsAsFactors = FALSE, check.names = FALSE)
  rownames(completed) <- NULL
  if (inherits(input, "uniml_data")) .restore_prep(input, completed) else completed
}

#' Apply all completed imputations
#'
#' Internal counterpart of [apply_imputer()]. Mice fits return one completed
#' data set per imputation; single-imputation methods return one data set.
#'
#' @param imputer A fitted `uniml_step_impute` object.
#' @param data Data used to fit the imputer.
#'
#' @return A list of completed data sets.
#' @keywords internal
.apply_imputer_all <- function(imputer, data) {
  if (!inherits(imputer, "uniml_step_impute")) {
    .abort("`imputer` must be a fitted `uniml_step_impute` object.")
  }
  if (is.null(imputer$params$mice)) {
    return(list(.impute_frame(imputer, data, index = 1L)))
  }
  .check_packages("mice")
  m <- tryCatch(as.integer(imputer$params$mice$m), error = function(e) 1L)
  if (is.na(m) || m < 1L) {
    m <- 1L
  }
  lapply(seq_len(m), function(index) .impute_frame(imputer, data, index = index))
}

#' Impute missing values
#'
#' Convenience wrapper that fits an imputer and applies it to the same data.
#'
#' @param data A data frame or `uniml_data` object.
#' @param method Imputation method.
#' @param outcome Outcome column name.
#' @param ... Arguments passed to [fit_imputer()].
#'
#' @return The imputed data, matching the class of `data`.
#' @references
#' van Buuren S, Groothuis-Oudshoorn K (2011). mice: multivariate imputation by
#' chained equations in R. *Journal of Statistical Software*, 45(3), 1-67.
#' doi:10.18637/jss.v045.i03
#'
#' Stekhoven DJ, Buhlmann P (2012). MissForest: non-parametric missing value
#' imputation for mixed-type data. *Bioinformatics*, 28(1), 112-118.
#' doi:10.1093/bioinformatics/btr597
#' @export
#' @examples
#' impute_data(data.frame(x = c(1, NA, 3), y = c(0, 1, 0)),
#'   method = "median_mode", outcome = "y")
impute_data <- function(data, method = c("median_mode", "mice", "missForest",
                                         "missRanger", "none"),
                        outcome = NULL, ...) {
  .require_prep_input(data, "data")
  step <- fit_imputer(
    .input_data(data), method = .norm_method(method, "method"),
    outcome = .outcome_name_of(data, outcome), ...
  )
  apply_imputer(step, data)
}
