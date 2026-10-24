# L2 numeric scaling ----------------------------------------------------------

#' Fit a numeric scaling step
#'
#' Computes Z-score or Min-Max scaling parameters on training data. Only
#' continuous numeric columns are scaled; binary 0/1 numeric columns are
#' excluded. Columns with zero variance are skipped with a warning.
#'
#' @param data A data frame.
#' @param method Scaling method, `"zscore"`, `"minmax"`, or `"none"`.
#' @param vars Optional character vector restricting which columns are scaled.
#' @param outcome Outcome column name, excluded from scaling.
#' @param ordered_as Ordered-factor handling.
#' @param verbose Verbosity level.
#'
#' @return A fitted `uniml_step_scale` step.
#' @export
#' @examples
#' fit_scale(data.frame(x = c(1, 2, 3), y = c(0, 1, 0)), outcome = "y")
fit_scale <- function(data, method = c("zscore", "minmax", "none"), vars = NULL,
                      outcome = NULL, ordered_as = c("ordinal", "numeric"),
                      verbose = 1) {
  if (!is.data.frame(data)) {
    .abort("`data` must be a data frame.")
  }
  method <- .norm_method(method, "method")
  .assert_choice(method, "method", c("zscore", "minmax", "none"))
  ordered_as <- match.arg(ordered_as)
  .assert_character_vector(vars, "vars", allow_null = TRUE)
  if (!is.null(outcome)) {
    .assert_scalar_character(outcome, "outcome")
  }

  if (!is.null(vars)) {
    missing_vars <- setdiff(vars, names(data))
    if (length(missing_vars) > 0L) {
      .abort(sprintf(
        "Scaling variable(s) not found in `data`: %s.",
        paste(missing_vars, collapse = ", ")
      ))
    }
    targets <- setdiff(vars, outcome)
  } else {
    types <- detect_var_types(data, outcome = outcome, ordered_as = ordered_as)
    targets <- types$column[types$type == "continuous"]
  }

  differences <- list()
  skipped <- character()
  if (!identical(method, "none") && length(targets) > 0L) {
    candidates <- targets[vapply(data[targets], is.numeric, logical(1))]
    for (column in candidates) {
      x <- data[[column]]
      if (anyNA(x)) {
        .abort(sprintf(
          "Column \"%s\" still contains missing values; impute before scaling.",
          column
        ))
      }
      if (identical(method, "zscore")) {
        center <- mean(x)
        spread <- stats::sd(x)
        if (!is.finite(spread) || spread == 0) {
          skipped <- c(skipped, column)
          next
        }
        differences[[column]] <- list(center = center, scale = spread)
      } else {
        range_x <- range(x)
        spread <- diff(range_x)
        if (!is.finite(spread) || spread == 0) {
          skipped <- c(skipped, column)
          next
        }
        differences[[column]] <- list(center = range_x[1L], scale = spread)
      }
    }
    if (length(skipped) > 0L) {
      warning(sprintf(
        "Skipped zero-variance column(s) during scaling: %s.",
        paste(skipped, collapse = ", ")
      ), call. = FALSE)
    }
  }

  params <- list(
    differences = differences,
    targets = names(differences),
    requested = targets,
    skipped = skipped
  )
  .new_step("scale", method = method, params = params, data = data)
}

#' Apply a fitted scaling step
#'
#' @param object A fitted `uniml_step_scale` object.
#' @param newdata A data frame or `uniml_data` object.
#' @param ... Reserved for future use.
#'
#' @return The scaled data, matching the class of `newdata`.
#' @export
#' @examples
#' train <- data.frame(x = c(1, 2, 3), y = c(0, 1, 0))
#' step <- fit_scale(train, outcome = "y")
#' apply_scale(step, train)
apply_scale <- function(object, newdata, ...) {
  UseMethod("apply_scale")
}

#' @rdname apply_scale
#' @export
apply_scale.default <- function(object, newdata, ...) {
  .no_method("apply_scale", object)
}

#' @rdname apply_scale
#' @export
apply_scale.uniml_step_scale <- function(object, newdata, ...) {
  had_class <- inherits(newdata, "uniml_data")
  input <- newdata
  data <- .input_data(newdata)
  targets <- object$params$targets
  missing_columns <- setdiff(targets, names(data))
  if (length(missing_columns) > 0L) {
    .abort(sprintf(
      "Column(s) required by the fitted scaler are missing: %s.",
      paste(missing_columns, collapse = ", ")
    ))
  }
  for (column in targets) {
    difference <- object$params$differences[[column]]
    data[[column]] <- (data[[column]] - difference$center) / difference$scale
  }
  rownames(data) <- NULL
  if (had_class) .restore_prep(input, data) else data
}

#' Scale numeric variables
#'
#' Convenience wrapper that applies the fixed preprocessing order and returns
#' the scaled data.
#'
#' @param data A data frame or `uniml_data` object.
#' @param method Scaling method.
#' @param vars Optional character vector restricting which columns are scaled.
#' @param outcome Outcome column name.
#' @param ... Arguments passed to [fit_preprocessor()].
#'
#' @return The scaled data, matching the class of `data`.
#' @export
#' @examples
#' scale_numeric(data.frame(x = c(1, 2, 3), y = c(0, 1, 0)), outcome = "y")
scale_numeric <- function(data, method = c("zscore", "minmax", "none"), vars = NULL,
                          outcome = NULL, ...) {
  .require_prep_input(data, "data")
  preprocessor <- fit_preprocessor(
    data,
    impute = "none",
    scale = .norm_method(method, "method"),
    encode = "none",
    balance = "none",
    vars = vars,
    outcome = .outcome_name_of(data, outcome),
    ...
  )
  result <- preprocessor$train
  if (inherits(data, "uniml_data")) .restore_prep(data, result) else result
}
