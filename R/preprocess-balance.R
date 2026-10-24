# L2 class balancing ----------------------------------------------------------

.check_balance_k <- function(n_neighbors) {
  .assert_scalar_number(n_neighbors, "n_neighbors", integer = TRUE, min = 1)
  if (n_neighbors > 10L) {
    warning(
      "`n_neighbors` above 10 may be rejected by SMOTE on small folds.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.numeric_predictors_only <- function(data, outcome) {
  predictors <- data[, setdiff(names(data), outcome), drop = FALSE]
  length(predictors) > 0L &&
    all(vapply(predictors, function(z) is.numeric(z) && !is.factor(z), logical(1)))
}

#' Fit a class-balancing step
#'
#' Balancing is applied to the training data only; validation and test data are
#' never resampled. Supported methods are `"none"`, `"rose"`, `"smote"`, and
#' `"both"`. A failed or inapplicable resampling falls back to the input data
#' with a warning.
#'
#' @param data A data frame.
#' @param method Balancing method.
#' @param outcome Outcome column name.
#' @param p ROSE target proportion of the minority class.
#' @param n_neighbors SMOTE number of neighbours.
#' @param dup_size SMOTE duplication size; `0` balances the classes exactly.
#' @param seed Random seed.
#' @param verbose Verbosity level.
#'
#' @return A fitted `uniml_step_balance` step.
#' @export
#' @examples
#' fit_balance(data.frame(x = c(1, 2, 3, 4), y = factor(c(0, 0, 0, 1))),
#'   outcome = "y", method = "none")
fit_balance <- function(data, method = c("none", "rose", "smote", "both"),
                        outcome = NULL, p = 0.5, n_neighbors = 5L, dup_size = 0,
                        seed = 123, verbose = 1) {
  if (!is.data.frame(data)) {
    .abort("`data` must be a data frame.")
  }
  method <- .norm_method(method, "method")
  .assert_choice(method, "method", c("none", "rose", "smote", "both"))
  if (!is.null(outcome)) {
    .assert_scalar_character(outcome, "outcome")
  }
  .assert_scalar_number(p, "p", min = 0, max = 1)
  .check_balance_k(n_neighbors)

  before <- if (is.null(outcome)) NULL else table(data[[outcome]])
  result <- data
  note <- NULL
  if (!identical(method, "none") && !is.null(outcome)) {
    if (!outcome %in% names(data)) {
      .abort(sprintf("Outcome column \"%s\" is not present in `data`.", outcome))
    }
    class_table <- table(data[[outcome]])
    if (length(class_table) < 2L || any(class_table < 2L)) {
      note <- "Class balancing was skipped: each class needs at least two rows."
    } else if (identical(method, "rose")) {
      .check_packages("ROSE")
      formula <- stats::as.formula(sprintf("`%s` ~ .", outcome))
      result <- tryCatch(
        ROSE::ROSE(formula, data = data, p = p, seed = seed)$data,
        error = function(e) e
      )
      if (inherits(result, "error")) {
        note <- sprintf("ROSE failed and the input data were kept: %s.", conditionMessage(result))
        result <- data
      }
    } else if (identical(method, "smote")) {
      result <- .run_smote(data, outcome, n_neighbors, dup_size, seed)
      if (is.character(result) && length(result) == 1L) {
        note <- result
        result <- data
      }
    } else {
      .check_packages(c("ROSE", "smotefamily"))
      formula <- stats::as.formula(sprintf("`%s` ~ .", outcome))
      rose_data <- tryCatch(
        ROSE::ROSE(formula, data = data, p = p, seed = seed)$data,
        error = function(e) e
      )
      if (inherits(rose_data, "error")) {
        note <- sprintf("Balancing failed and the input data were kept: %s.",
                        conditionMessage(rose_data))
        result <- data
      } else {
        smote_data <- .run_smote(rose_data, outcome, n_neighbors, dup_size, seed)
        if (is.character(smote_data) && length(smote_data) == 1L) {
          note <- smote_data
          result <- data
        } else {
          result <- smote_data
        }
      }
    }
    if (!is.data.frame(result)) {
      note <- "Class balancing failed and the input data were kept."
      result <- data
    }
    if (!is.null(note)) {
      warning(note, call. = FALSE)
    }
    rownames(result) <- NULL
    if (!is.null(outcome) && outcome %in% names(result) && is.factor(data[[outcome]])) {
      result[[outcome]] <- factor(
        as.character(result[[outcome]]), levels = levels(data[[outcome]])
      )
    }
  }

  params <- list(
    p = p,
    n_neighbors = n_neighbors,
    dup_size = dup_size,
    seed = seed,
    before = before,
    after = if (is.null(outcome)) NULL else table(result[[outcome]]),
    note = note
  )
  .new_step("balance", method = method, params = params, data = result)
}

.run_smote <- function(data, outcome, n_neighbors, dup_size, seed) {
  .check_packages("smotefamily")
  if (!.numeric_predictors_only(data, outcome)) {
    return("SMOTE requires numeric predictors; the input data were kept.")
  }
  predictors <- data[, setdiff(names(data), outcome), drop = FALSE]
  class_values <- data[[outcome]]
  set.seed(seed)
  out <- tryCatch(
    smotefamily::SMOTE(predictors, class_values, K = n_neighbors,
                       dup_size = dup_size),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    return(sprintf("SMOTE failed and the input data were kept: %s.",
                   conditionMessage(out)))
  }
  balanced <- out$data
  names(balanced)[names(balanced) == "class"] <- outcome
  columns <- c(setdiff(names(data), outcome), outcome)
  balanced <- balanced[, columns, drop = FALSE]
  balanced
}

#' Apply a fitted class-balancing step
#'
#' Balancing never transforms validation or test data, so this method always
#' returns the fitted data unchanged.
#'
#' @param object A fitted `uniml_step_balance` object.
#' @param newdata A data frame or `uniml_data` object.
#' @param ... Reserved for future use.
#'
#' @return The data as supplied by `newdata`, unchanged.
#' @export
#' @examples
#' train <- data.frame(x = c(1, 2, 3, 4), y = c(0, 0, 0, 1))
#' step <- fit_balance(train, outcome = "y")
#' apply_balance(step, train)
apply_balance <- function(object, newdata, ...) {
  UseMethod("apply_balance")
}

#' @rdname apply_balance
#' @export
apply_balance.default <- function(object, newdata, ...) {
  .no_method("apply_balance", object)
}

#' @rdname apply_balance
#' @export
apply_balance.uniml_step_balance <- function(object, newdata, ...) {
  if (is.null(object$data)) {
    .prep_step_data(object)
  }
  newdata
}

#' Balance class frequencies
#'
#' Convenience wrapper that applies the fixed preprocessing order and returns
#' the balanced training data.
#'
#' @param data A data frame or `uniml_data` object.
#' @param method Balancing method.
#' @param outcome Outcome column name.
#' @param ... Arguments passed to [fit_preprocessor()].
#'
#' @return The balanced data, matching the class of `data`.
#' @export
#' @examples
#' balance_classes(
#'   data.frame(x = c(1, 2, 3, 4), y = factor(c(0, 0, 0, 1))),
#'   method = "none", outcome = "y"
#' )
balance_classes <- function(data, method = c("none", "rose", "smote", "both"),
                            outcome = NULL, ...) {
  .require_prep_input(data, "data")
  preprocessor <- fit_preprocessor(
    data,
    impute = "none",
    scale = "none",
    encode = "none",
    balance = .norm_method(method, "method"),
    outcome = .outcome_name_of(data, outcome),
    ...
  )
  result <- preprocessor$train
  if (inherits(data, "uniml_data")) .restore_prep(data, result) else result
}
