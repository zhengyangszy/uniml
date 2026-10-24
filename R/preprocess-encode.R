# L2 categorical encoding -----------------------------------------------------

#' Fit a categorical encoding step
#'
#' Builds dummy-variable mappings from training data. Binary categorical
#' columns become a single 0/1 indicator; multi-level columns expand to one
#' column per level (or per level minus the reference level with
#' `"onehot_drop"`).
#'
#' @param data A data frame.
#' @param method Encoding method, `"onehot"`, `"onehot_drop"`, or `"none"`.
#' @param outcome Outcome column name, excluded from encoding.
#' @param ordered_as Ordered-factor handling.
#' @param max_levels Maximum levels allowed before a high-cardinality warning.
#' @param verbose Verbosity level.
#'
#' @return A fitted `uniml_step_encode` step.
#' @export
#' @examples
#' fit_encode(data.frame(x = factor(c("a", "b", "a")), y = c(0, 1, 0)),
#'   outcome = "y")
fit_encode <- function(data, method = c("onehot", "onehot_drop", "none"),
                       outcome = NULL, ordered_as = c("ordinal", "numeric"),
                       max_levels = 50L, verbose = 1) {
  if (!is.data.frame(data)) {
    .abort("`data` must be a data frame.")
  }
  method <- .norm_method(method, "method")
  .assert_choice(method, "method", c("onehot", "onehot_drop", "none"))
  ordered_as <- match.arg(ordered_as)
  .assert_scalar_number(max_levels, "max_levels", integer = TRUE, min = 2)
  if (!is.null(outcome)) {
    .assert_scalar_character(outcome, "outcome")
  }

  columns <- character()
  levels_map <- list()
  if (!identical(method, "none")) {
    types <- detect_var_types(data, outcome = outcome, ordered_as = ordered_as)
    columns <- types$column[types$type %in% c("categorical", "ordered")]
    for (column in columns) {
      x <- data[[column]]
      levels_map[[column]] <- if (is.factor(x)) {
        levels(x)
      } else if (is.logical(x)) {
        c("FALSE", "TRUE")
      } else {
        sort(unique(as.character(x[!is.na(x)])))
      }
    }
    high_cardinality <- names(levels_map)[vapply(
      levels_map, function(z) length(z) > max_levels, logical(1)
    )]
    if (length(high_cardinality) > 0L) {
      warning(sprintf(
        "High-cardinality categorical column(s) (> %d levels): %s.",
        max_levels,
        paste(high_cardinality, collapse = ", ")
      ), call. = FALSE)
    }
  }

  params <- list(
    levels = levels_map,
    columns = columns,
    dropped = if (identical(method, "onehot_drop")) {
      lapply(levels_map, function(z) z[1L])
    } else {
      list()
    }
  )
  .new_step("encode", method = method, params = params, data = data)
}

#' Apply a fitted encoding step
#'
#' @param object A fitted `uniml_step_encode` object.
#' @param newdata A data frame or `uniml_data` object.
#' @param ... Reserved for future use.
#'
#' @return The encoded data, matching the class of `newdata`.
#' @export
#' @examples
#' train <- data.frame(x = factor(c("a", "b", "a")), y = c(0, 1, 0))
#' step <- fit_encode(train, outcome = "y")
#' apply_encode(step, train)
apply_encode <- function(object, newdata, ...) {
  UseMethod("apply_encode")
}

#' @rdname apply_encode
#' @export
apply_encode.default <- function(object, newdata, ...) {
  .no_method("apply_encode", object)
}

#' @rdname apply_encode
#' @export
apply_encode.uniml_step_encode <- function(object, newdata, ...) {
  had_class <- inherits(newdata, "uniml_data")
  input <- newdata
  data <- .input_data(newdata)
  mapping <- object$params$levels
  missing_columns <- setdiff(names(mapping), names(data))
  if (length(missing_columns) > 0L) {
    .abort(sprintf(
      "Column(s) required by the fitted encoder are missing: %s.",
      paste(missing_columns, collapse = ", ")
    ))
  }

  unknown <- character()
  for (column in names(mapping)) {
    seen <- unique(as.character(data[[column]][!is.na(data[[column]])]))
    if (any(!seen %in% mapping[[column]])) {
      unknown <- c(unknown, column)
    }
  }
  if (length(unknown) > 0L) {
    warning(sprintf(
      "Unseen level(s) encoded as all-zero in column(s): %s.",
      paste(unknown, collapse = ", ")
    ), call. = FALSE)
  }

  if (!identical(object$method, "none") && length(mapping) > 0L) {
    work <- data[, setdiff(names(data), names(mapping)), drop = FALSE]
    additions <- list()
    for (column in names(mapping)) {
      levels <- mapping[[column]]
      x <- as.character(data[[column]])
      if (identical(object$method, "onehot_drop")) {
        reference <- object$params$dropped[[column]]
        levels <- setdiff(levels, reference)
      }
      if (length(levels) >= 1L) {
        for (level in levels) {
          additions[[paste0(column, "_", level)]] <- as.integer(!is.na(x) & x == level)
        }
      }
    }
    data <- cbind(work, as.data.frame(additions, check.names = FALSE))
    data <- data[, names(data), drop = FALSE]
  }

  rownames(data) <- NULL
  if (had_class) .restore_prep(input, data) else data
}

#' Encode categorical variables
#'
#' Convenience wrapper returning the encoded data.
#'
#' @param data A data frame or `uniml_data` object.
#' @param method Encoding method.
#' @param outcome Outcome column name.
#' @param ... Arguments passed to [fit_preprocessor()].
#'
#' @return The encoded data, matching the class of `data`.
#' @export
#' @examples
#' encode_categorical(
#'   data.frame(x = factor(c("a", "b", "a")), y = c(0, 1, 0)),
#'   outcome = "y"
#' )
encode_categorical <- function(data, method = c("onehot", "onehot_drop", "none"),
                               outcome = NULL, ...) {
  .require_prep_input(data, "data")
  preprocessor <- fit_preprocessor(
    data,
    impute = "none",
    scale = "none",
    encode = .norm_method(method, "method"),
    balance = "none",
    outcome = .outcome_name_of(data, outcome),
    ...
  )
  result <- preprocessor$train
  if (inherits(data, "uniml_data")) .restore_prep(data, result) else result
}
