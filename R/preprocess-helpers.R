# L2 preprocessing helpers ---------------------------------------------------
#
# Internal helpers shared by the imputation, scaling, encoding, and balancing
# steps. They also carry the reusable preprocessing container built by
# `fit_preprocessor()` and consumed by `apply_preprocessor()`.

.preprocess_order <- c("impute", "scale", "encode", "balance")

.empty_preprocessor <- function() {
  structure(
    list(order = .preprocess_order, steps = list()),
    class = c("uniml_preprocessor", "list")
  )
}

.new_preprocessor <- function(steps = list()) {
  out <- structure(
    list(order = .preprocess_order, steps = steps),
    class = c("uniml_preprocessor", "list")
  )
  validate_uniml_preprocessor(out)
  out
}

.new_step <- function(kind, method = "none", params = list(), data = NULL) {
  stopifnot(kind %in% .preprocess_order)
  structure(
    list(kind = kind, method = method, params = params, data = data),
    class = c(paste0("uniml_step_", kind), "uniml_step", "list")
  )
}

.get_step <- function(preprocessor, kind) {
  preprocessor$steps[[kind]]
}

.norm_method <- function(x, name) {
  if (is.null(x)) {
    return("none")
  }
  if (!is.character(x) || length(x) < 1L || is.na(x[1L]) || !nzchar(x[1L])) {
    .abort(sprintf("`%s` must be a non-empty character value.", name))
  }
  x[1L]
}

.prep_step_data <- function(step) {
  if (is.null(step$data)) {
    .abort(sprintf(
      "The \"%s\" step has not been fitted yet; call `fit_preprocessor()` first.",
      step$kind
    ))
  }
  step$data
}

.restore_prep <- function(input, data) {
  if (!inherits(input, "uniml_data")) {
    return(data)
  }
  out <- input
  out$data <- data
  feature_cols <- names(data)[names(data) != out$outcome_name]
  out$features <- data[, feature_cols, drop = FALSE]
  out$role <- stats::setNames(
    ifelse(feature_cols %in% out$id, "id",
           ifelse(feature_cols %in% out$exclude, "exclude", "feature")),
    feature_cols
  )
  out$role <- c(out$role, stats::setNames("outcome", out$outcome_name))
  out$types <- data.frame(
    column = names(data),
    class = vapply(data, function(z) paste(class(z), collapse = "/"), character(1)),
    role = unname(out$role[names(data)]),
    stringsAsFactors = FALSE
  )
  out$meta$n <- nrow(data)
  out$meta$n_features <- ncol(out$features)
  validate_uniml_data(out)
  out
}

.require_prep_input <- function(x, name) {
  if (!is.data.frame(x) && !inherits(x, "uniml_data")) {
    .abort(sprintf(
      "`%s` must be a data frame or an object created by `as_uniml_data()`.",
      name
    ))
  }
  invisible(TRUE)
}

.input_data <- function(x) {
  if (inherits(x, "uniml_data")) x$data else x
}

.outcome_name_of <- function(x, outcome) {
  if (!is.null(outcome)) {
    .assert_scalar_character(outcome, "outcome")
    return(outcome)
  }
  if (inherits(x, "uniml_data")) {
    return(x$outcome_name)
  }
  NULL
}

.factorial <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x)
}

.continuous_column <- function(x) {
  if (!is.numeric(x) || is.factor(x) || is.logical(x)) {
    return(FALSE)
  }
  values <- unique(x[!is.na(x)])
  length(values) > 0L && !(length(values) == 2L && all(values %in% c(0, 1)))
}

.mode_value <- function(x) {
  values <- x[!is.na(x)]
  if (length(values) == 0L) {
    return(NA)
  }
  counts <- table(values)
  best <- names(counts)[which.max(counts)]
  if (is.factor(x)) {
    factor(best, levels = levels(x))
  } else if (is.logical(x)) {
    as.logical(best)
  } else {
    best
  }
}

.factor_with <- function(x, values) {
  if (is.factor(x)) {
    return(factor(as.character(values), levels = levels(x)))
  }
  if (is.logical(x)) {
    return(as.logical(values))
  }
  as.character(values)
}

.columns_with_missing <- function(data) {
  names(data)[vapply(data, function(z) anyNA(z), logical(1))]
}

.check_packages <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0L) {
    .abort(sprintf(
      "Package(s) required for this method are not installed: %s.",
      paste(missing, collapse = ", ")
    ))
  }
  invisible(TRUE)
}

#' Detect feature variable types
#'
#' @param data A data frame.
#' @param outcome Outcome column name, excluded from the result.
#' @param ordered_as How ordered factors are handled, `"ordinal"` or `"numeric"`.
#'
#' @return A data frame with `column`, `type`, and `n_levels`.
#' @export
#' @examples
#' detect_var_types(iris)
detect_var_types <- function(data, outcome = NULL, ordered_as = c("ordinal", "numeric")) {
  if (!is.data.frame(data)) {
    .abort("`data` must be a data frame.")
  }
  ordered_as <- match.arg(ordered_as)
  if (!is.null(outcome)) {
    .assert_scalar_character(outcome, "outcome")
  }
  columns <- setdiff(names(data), outcome)
  if (length(columns) == 0L) {
    return(data.frame(
      column = character(), type = character(), n_levels = integer(),
      stringsAsFactors = FALSE
    ))
  }

  types <- vapply(data[columns], function(x) {
    if (is.ordered(x) && identical(ordered_as, "numeric")) {
      "continuous"
    } else if (.continuous_column(x)) {
      "continuous"
    } else if (is.ordered(x)) {
      "ordered"
    } else {
      "categorical"
    }
  }, character(1))

  levels_count <- vapply(data[columns], function(x) {
    if (is.factor(x)) {
      nlevels(x)
    } else {
      length(unique(x[!is.na(x)]))
    }
  }, integer(1))

  data.frame(
    column = columns,
    type = unname(types),
    n_levels = unname(levels_count),
    stringsAsFactors = FALSE
  )
}

#' Diagnose missing values
#'
#' @param data A data frame.
#'
#' @return A list with `per_column` (missing count and share) and `overall`.
#' @export
#' @examples
#' diagnose_missing(data.frame(a = c(1, NA), b = c("x", "y")))
diagnose_missing <- function(data) {
  if (!is.data.frame(data)) {
    .abort("`data` must be a data frame.")
  }
  per_column <- data.frame(
    column = names(data),
    n_missing = vapply(data, function(z) sum(is.na(z)), integer(1)),
    share_missing = vapply(data, function(z) mean(is.na(z)), numeric(1)),
    stringsAsFactors = FALSE
  )
  list(
    per_column = per_column,
    overall = if (length(data) == 0L) 0 else mean(is.na(as.matrix(data))),
    n_incomplete = sum(!stats::complete.cases(data))
  )
}

#' Validate a uniml preprocessor
#'
#' @param x A `uniml_preprocessor` object.
#'
#' @return Invisibly returns `TRUE` when valid.
#' @export
#' @examples
#' train <- data.frame(x = c(1, NA, 3), y = c(0, 1, 0))
#' validate_uniml_preprocessor(fit_preprocessor(train, outcome = "y"))
validate_uniml_preprocessor <- function(x) {
  if (!inherits(x, "uniml_preprocessor")) {
    .abort("`x` must be a `uniml_preprocessor` object.")
  }
  if (!is.character(x$order) || !identical(x$order, .preprocess_order)) {
    .abort("`order` must list the preprocessing steps in supported order.")
  }
  if (!is.list(x$steps)) {
    .abort("`steps` must be a list.")
  }
  unknown <- setdiff(names(x$steps), .preprocess_order)
  if (length(unknown) > 0L) {
    .abort(sprintf("Unknown preprocessing step(s): %s.", paste(unknown, collapse = ", ")))
  }
  for (kind in names(x$steps)) {
    step <- x$steps[[kind]]
    if (!inherits(step, "uniml_step") || !identical(step$kind, kind)) {
      .abort(sprintf("`steps$%s` must be a matching `uniml_step`.", kind))
    }
    if (is.null(step$data)) {
      .abort(sprintf("`steps$%s$data` is missing; the step was not fitted.", kind))
    }
  }
  invisible(TRUE)
}

#' Print a uniml preprocessor
#'
#' @param x A `uniml_preprocessor` object.
#' @param ... Additional arguments passed to methods.
#'
#' @return Invisibly returns `x`.
#' @export
print.uniml_preprocessor <- function(x, ...) {
  validate_uniml_preprocessor(x)
  cat("<uniml_preprocessor>\n")
  for (kind in x$order) {
    step <- x$steps[[kind]]
    if (is.null(step)) {
      cat(sprintf("%s: (not fitted)\n", kind))
    } else {
      cat(sprintf("%s: %s (n = %d)\n", kind, step$method, nrow(step$data)))
    }
  }
  invisible(x)
}

#' Fit a preprocessing pipeline
#'
#' Fits imputation, scaling, encoding, and class balancing on training data in
#' the fixed order `impute -> scale -> encode -> balance`. Fitted parameters are
#' reused by [apply_preprocessor()] on validation or test data.
#'
#' @param data A data frame or `uniml_data` object.
#' @param spec Optional `uniml_spec`; preprocessing options are read from it.
#' @param impute,scale,encode,balance Optional method overrides.
#' @param m Number of imputations for `mice`.
#' @param p ROSE target proportion of the minority class.
#' @param n_neighbors SMOTE number of neighbours.
#' @param dup_size SMOTE duplication size.
#' @param vars Optional variables for scaling; defaults to type detection.
#' @param outcome Outcome column name.
#' @param ordered_as Ordered-factor handling.
#' @param max_iter Iterations passed to `mice`.
#' @param seed Random seed.
#' @param verbose Verbosity level.
#'
#' @return A fitted `uniml_preprocessor` object.
#' @export
#' @examples
#' fit_preprocessor(data.frame(x = c(1, NA, 3), y = c(0, 1, 0)), outcome = "y")
fit_preprocessor <- function(data, spec = NULL, impute = NULL, scale = NULL,
                             encode = NULL, balance = NULL, m = NULL, p = 0.5,
                             n_neighbors = 5L, dup_size = 0, vars = NULL,
                             outcome = NULL, ordered_as = NULL, max_iter = 5,
                             seed = 123, verbose = 1) {
  .require_prep_input(data, "data")
  if (!is.null(spec)) {
    validate_uniml_spec(spec)
  }
  ordered_as <- if (is.null(ordered_as)) {
    if (is.null(spec)) "ordinal" else spec$ordered_as
  } else {
    ordered_as
  }
  .assert_choice(ordered_as, "ordered_as", c("ordinal", "numeric"))
  impute <- .norm_method(
    if (is.null(impute)) if (is.null(spec)) "none" else spec$preprocess$impute else impute,
    "impute"
  )
  scale <- .norm_method(
    if (is.null(scale)) if (is.null(spec)) "none" else spec$preprocess$scale else scale,
    "scale"
  )
  encode <- .norm_method(
    if (is.null(encode)) if (is.null(spec)) "none" else spec$preprocess$encode else encode,
    "encode"
  )
  balance <- .norm_method(
    if (is.null(balance)) if (is.null(spec)) "none" else spec$preprocess$balance else balance,
    "balance"
  )
  m <- if (is.null(m)) if (is.null(spec)) 5L else spec$preprocess$m else m
  seed <- if (is.null(seed)) if (is.null(spec)) 123L else spec$seed else seed
  verbose <- if (is.null(verbose)) if (is.null(spec)) 1L else spec$verbose else verbose
  outcome_name <- .outcome_name_of(data, outcome)
  raw <- .input_data(data)
  if (!is.null(outcome_name) && !outcome_name %in% names(raw)) {
    .abort(sprintf("Outcome column \"%s\" is not present in `data`.", outcome_name))
  }

  impute_step <- fit_imputer(
    raw, method = impute, outcome = outcome_name, ordered_as = ordered_as,
    m = m, max_iter = max_iter, seed = seed, verbose = verbose
  )
  scaled <- apply_imputer(impute_step, raw)

  scale_step <- fit_scale(
    scaled, method = scale, vars = vars, outcome = outcome_name,
    ordered_as = ordered_as, verbose = verbose
  )
  scaled <- apply_scale(scale_step, scaled)

  encode_step <- fit_encode(
    scaled, method = encode, outcome = outcome_name, ordered_as = ordered_as,
    verbose = verbose
  )
  encoded <- apply_encode(encode_step, scaled)

  balance_step <- fit_balance(
    encoded, method = balance, outcome = outcome_name, p = p,
    n_neighbors = n_neighbors, dup_size = dup_size, seed = seed,
    verbose = verbose
  )
  # `apply_balance()` never transforms held-out data, so the training output is
  # the data stored by the fit step.
  balanced <- balance_step$data

  preprocessor <- .new_preprocessor(list(
    impute = impute_step,
    scale = scale_step,
    encode = encode_step,
    balance = balance_step
  ))
  preprocessor$var_info <- detect_var_types(
    raw,
    outcome = outcome_name,
    ordered_as = ordered_as
  )
  preprocessor$train <- balanced
  preprocessor
}

.apply_generic_for <- function(kind) {
  switch(
    kind,
    impute = "apply_imputer",
    scale = "apply_scale",
    encode = "apply_encode",
    balance = "apply_balance"
  )
}

#' Apply a fitted preprocessing pipeline
#'
#' @param preprocessor A fitted `uniml_preprocessor` object.
#' @param newdata A data frame or `uniml_data` object to transform.
#' @param ... Reserved for future use.
#'
#' @return The transformed data, matching the class of `newdata`.
#' @export
#' @examples
#' train <- data.frame(x = c(1, NA, 3), y = c(0, 1, 0))
#' pre <- fit_preprocessor(train, outcome = "y")
#' apply_preprocessor(pre, train)
apply_preprocessor <- function(preprocessor, newdata, ...) {
  validate_uniml_preprocessor(preprocessor)
  .require_prep_input(newdata, "newdata")
  stepped <- newdata
  for (kind in preprocessor$order) {
    generic <- .apply_generic_for(kind)
    stepped <- do.call(generic, list(preprocessor$steps[[kind]], stepped))
  }
  stepped
}
