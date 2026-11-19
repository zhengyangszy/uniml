# L2 skewness transform -------------------------------------------------------
#
# A strongly right-skewed lab value can dominate a distance-based learner, and
# the tutorial literature suggests a log transform before anything else. The
# convention here is deliberately conservative: `log1p` is applied **only** to
# numeric columns whose skewness exceeds a threshold and whose values are
# non-negative, because `log1p` of a negative value is undefined and of a
# mixed-sign column is misleading. Which columns were transformed is recorded
# in the step, and the training data the decision was made on is stored for
# audit.

.transform_skewness <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 3L) {
    return(NA_real_)
  }
  sd <- stats::sd(x)
  if (!is.finite(sd) || sd <= 0) {
    return(NA_real_)
  }
  sum((x - mean(x))^3) / (n * sd^3)
}

#' Fit a skewness transform step
#'
#' Chooses which numeric columns get a `log1p` transform: every numeric column
#' with sample skewness above `threshold` and no negative observed value.
#'
#' @param data A data frame.
#' @param method `"log1p"` or `"none"`.
#' @param threshold Skewness above which a column is transformed.
#' @param outcome Outcome column name, never transformed.
#' @param verbose Verbosity level.
#'
#' @return A fitted `uniml_step_transform` step.
#' @references
#' Steyerberg EW (2019). *Clinical Prediction Models: A Practical Approach to
#' Development, Validation, and Updating*, 2nd edition. Springer.
#' doi:10.1007/978-3-030-16399-0
#' @export
#' @examples
#' fit_transform(data.frame(x = rexp(100), y = c(0, 1)))
fit_transform <- function(data, method = c("none", "log1p"),
                          threshold = 1, outcome = NULL, verbose = 1) {
  method <- .norm_method(method, "method")
  .assert_choice(method, "method", c("none", "log1p"))
  .assert_scalar_number(threshold, "threshold", min = 0)
  columns <- character()
  if (!identical(method, "none")) {
    types <- detect_var_types(data, outcome = outcome)
    numeric_columns <- types$column[types$type %in% c("continuous", "ordered") &
                                      vapply(data[types$column], is.numeric,
                                             logical(1))]
    for (column in numeric_columns) {
      x <- data[[column]]
      if (any(x < 0, na.rm = TRUE)) {
        next
      }
      skew <- .transform_skewness(x)
      if (is.finite(skew) && skew > threshold) {
        columns <- c(columns, column)
      }
    }
  }
  params <- list(columns = columns, threshold = threshold)
  .new_step("transform", method = method, params = params, data = data)
}

#' Apply a fitted skewness transform step
#'
#' @param object A fitted `uniml_step_transform` object.
#' @param newdata A data frame or `uniml_data` object.
#' @param ... Reserved for future use.
#'
#' @return The transformed data, matching the class of `newdata`.
#' @export
#' @examples
#' train <- data.frame(x = rexp(100), y = c(0, 1))
#' step <- fit_transform(train)
#' apply_transform(step, train)
apply_transform <- function(object, newdata, ...) {
  UseMethod("apply_transform")
}

#' @rdname apply_transform
#' @export
apply_transform.default <- function(object, newdata, ...) {
  .no_method("apply_transform", object)
}

#' @rdname apply_transform
#' @export
apply_transform.uniml_step_transform <- function(object, newdata, ...) {
  had_class <- inherits(newdata, "uniml_data")
  input <- newdata
  data <- .input_data(newdata)
  if (identical(object$method, "log1p") && length(object$params$columns) > 0L) {
    for (column in object$params$columns) {
      if (column %in% names(data)) {
        data[[column]] <- log1p(data[[column]])
      }
    }
  }
  rownames(data) <- NULL
  if (had_class) .restore_prep(input, data) else data
}
