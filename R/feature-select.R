# L3 feature selection ------------------------------------------------------

.selection_raw <- function(data) {
  if (inherits(data, "uniml_preprocessor")) {
    validate_uniml_preprocessor(data)
    if (is.null(data$train)) {
      .abort("`data` must contain a fitted training data set.")
    }
    return(data$train)
  }
  if (inherits(data, "uniml_data")) {
    validate_uniml_data(data)
    columns <- c(names(data$features), data$outcome_name)
    return(data$data[, columns, drop = FALSE])
  }
  if (!is.data.frame(data)) {
    .abort("`data` must be a data frame, `uniml_data`, or `uniml_preprocessor`.")
  }
  data
}

.selection_outcome <- function(data, outcome) {
  .assert_scalar_character(outcome, "outcome")
  if (!outcome %in% names(data)) {
    .abort(sprintf("Outcome column \"%s\" is not present in `data`.", outcome))
  }
  values <- data[[outcome]]
  if (anyNA(values)) {
    .abort("Feature selection requires a non-missing outcome.")
  }

  if (is.factor(values)) {
    if (nlevels(values) != 2L) {
      .abort("`outcome` must be a two-level factor or a binary vector.")
    }
    labels <- levels(values)
  } else {
    labels <- sort(unique(as.character(values)))
    if (length(labels) != 2L) {
      .abort("`outcome` must contain exactly two classes.")
    }
    values <- factor(as.character(values), levels = labels)
  }

  list(
    values = factor(as.character(values), levels = labels),
    labels = labels
  )
}

.selection_matrix <- function(data, outcome = NULL) {
  if (!is.data.frame(data)) {
    .abort("`data` must be a data frame.")
  }
  if (is.null(names(data)) || any(!nzchar(names(data))) ||
      anyDuplicated(names(data))) {
    .abort("`data` must have unique, non-empty column names.")
  }
  if (!is.null(outcome)) {
    .assert_scalar_character(outcome, "outcome")
    if (!outcome %in% names(data)) {
      .abort(sprintf("Outcome column \"%s\" is not present in `data`.", outcome))
    }
    if (anyNA(data[[outcome]])) {
      .abort("Feature selection requires a non-missing outcome.")
    }
  }

  feature_names <- setdiff(names(data), outcome)
  if (length(feature_names) == 0L) {
    .abort("At least one feature column is required for selection.")
  }
  features <- data[, feature_names, drop = FALSE]
  if (anyNA(features)) {
    .abort("Feature selection requires complete feature data; impute missing values first.")
  }

  additions <- list()
  mapping <- list()
  for (column in feature_names) {
    x <- features[[column]]
    if (.factorial(x)) {
      levels <- if (is.factor(x)) levels(x) else sort(unique(as.character(x)))
      levels <- levels[!is.na(levels)]
      if (length(levels) == 0L) {
        next
      }
      mapping[[column]] <- levels
      for (level in levels) {
        name <- paste0(column, "_", level)
        if (name %in% names(additions)) {
          .abort(sprintf("Encoded feature name collision for \"%s\".", name))
        }
        additions[[name]] <- as.integer(!is.na(x) & as.character(x) == level)
      }
    } else {
      if (!is.numeric(x)) {
        .abort(sprintf("Feature \"%s\" must be numeric or categorical.", column))
      }
      additions[[column]] <- as.numeric(x)
    }
  }

  if (length(additions) == 0L) {
    .abort("No usable features remain after encoding.")
  }
  matrix <- as.matrix(as.data.frame(additions, check.names = FALSE))
  storage.mode(matrix) <- "double"
  if (anyNA(matrix)) {
    .abort("Feature selection matrix contains missing values after encoding.")
  }

  non_constant <- apply(matrix, 2L, function(z) {
    length(z) > 0L && stats::sd(z) > 0
  })
  dropped <- colnames(matrix)[!non_constant]
  matrix <- matrix[, non_constant, drop = FALSE]
  if (ncol(matrix) == 0L) {
    .abort("No non-constant features remain after encoding.")
  }

  list(
    matrix = matrix,
    mapping = mapping,
    original_features = feature_names,
    dropped_features = dropped
  )
}

.selection_mapping <- function(data, raw, design, outcome) {
  if (inherits(data, "uniml_preprocessor")) {
    step <- data$steps$encode
    if (!is.null(step)) {
      return(list(
        levels = step$params$levels,
        columns = step$params$columns,
        dropped = step$params$dropped
      ))
    }
  }

  attr_map <- attr(raw, "uniml_encoder", exact = TRUE)
  if (is.list(attr_map)) {
    levels <- if (is.null(attr_map$levels)) list() else attr_map$levels
    return(list(
      levels = levels,
      columns = if (is.null(attr_map$columns)) names(levels) else attr_map$columns,
      dropped = if (is.null(attr_map$dropped)) list() else attr_map$dropped
    ))
  }

  list(
    levels = design$mapping,
    columns = names(design$mapping),
    dropped = list()
  )
}

.selection_columns_for <- function(mapping, original) {
  levels <- mapping$levels[[original]]
  if (is.null(levels)) {
    return(original)
  }
  dropped <- mapping$dropped[[original]]
  if (!is.null(dropped)) {
    levels <- setdiff(levels, dropped)
  }
  paste0(original, "_", levels)
}

.apply_representation <- function(selected, mapping, representation) {
  selected <- unique(as.character(selected))
  if (length(selected) == 0L) {
    return(list(selected_features = character(), original_features = character()))
  }

  encoded_to_original <- list()
  if (!is.null(mapping) && length(mapping$levels) > 0L) {
    for (original in names(mapping$levels)) {
      encoded_to_original[[original]] <- .selection_columns_for(mapping, original)
    }
  }

  original_features <- character()
  for (feature in selected) {
    matched <- FALSE
    for (original in names(encoded_to_original)) {
      if (feature %in% encoded_to_original[[original]]) {
        original_features <- c(original_features, original)
        matched <- TRUE
        break
      }
    }
    if (!matched) {
      original_features <- c(original_features, feature)
    }
  }
  original_features <- unique(original_features)

  if (identical(representation, "selected_levels")) {
    return(list(
      selected_features = selected,
      original_features = original_features
    ))
  }

  full <- character()
  for (original in original_features) {
    if (original %in% names(encoded_to_original)) {
      full <- c(full, encoded_to_original[[original]])
    } else {
      full <- c(full, original)
    }
  }
  list(
    selected_features = unique(full),
    original_features = original_features
  )
}

.new_selection_result <- function(method, selected_features, details, params = list(),
                                  scores = NULL) {
  selected_features <- unique(as.character(selected_features))
  details <- as.data.frame(details, stringsAsFactors = FALSE)
  if (nrow(details) > 0L && !"feature" %in% names(details)) {
    .abort("Selection details must contain a `feature` column.")
  }
  structure(
    list(
      method = method,
      selected_features = selected_features,
      details = details,
      params = params,
      scores = scores
    ),
    class = c("uniml_selection_result", "list")
  )
}

.selection_details <- function(features, selected, ...) {
  details <- data.frame(
    feature = features,
    selected = features %in% selected,
    stringsAsFactors = FALSE
  )
  extras <- list(...)
  for (name in names(extras)) {
    value <- extras[[name]]
    if (length(value) == nrow(details)) {
      details[[name]] <- value
    }
  }
  details
}

.selection_data_for_method <- function(design, outcome) {
  result <- as.data.frame(design$matrix, check.names = FALSE)
  result[[outcome]] <- design$outcome
  result
}

.selection_methods <- function(methods) {
  .assert_character_vector(methods, "selection$methods")
  vapply(methods, function(method) {
    switch(
      method,
      univariate = "univariate",
      logistic = "univariate",
      single = "univariate",
      lasso = "lasso",
      boruta = "boruta",
      rf = "rf_importance",
      rf_importance = "rf_importance",
      importance = "rf_importance",
      rfe = "rfe",
      multivariate = "multivariate",
      .abort(sprintf("Unsupported feature-selection method: \"%s\".", method))
    )
  }, character(1))
}

.selection_combine_details <- function(method_results, combined) {
  rows <- list()
  for (method in names(method_results)) {
    details <- method_results[[method]]$details
    if (nrow(details) == 0L) {
      next
    }
    details$method <- method
    details$method_selected <- details$feature %in% method_results[[method]]$selected_features
    details$combined_selected <- details$feature %in% combined
    rows[[method]] <- details
  }
  if (length(rows) == 0L) {
    return(data.frame(
      feature = character(), method = character(), selected = logical(),
      method_selected = logical(), combined_selected = logical(),
      stringsAsFactors = FALSE
    ))
  }
  all_columns <- unique(unlist(lapply(rows, names)))
  rows <- lapply(rows, function(x) {
    for (column in setdiff(all_columns, names(x))) {
      x[[column]] <- NA
    }
    x[, all_columns, drop = FALSE]
  })
  do.call(rbind, rows)
}

.selection_glm_pvalues <- function(x, y) {
  fit <- tryCatch(
    suppressWarnings(stats::glm(y ~ x, family = stats::binomial())),
    error = function(e) NULL
  )
  if (is.null(fit)) {
    return(NA_real_)
  }
  coefficients <- summary(fit)$coefficients
  if (!"x" %in% rownames(coefficients)) {
    return(NA_real_)
  }
  unname(coefficients["x", "Pr(>|z|)"])
}

.selection_filter_scores <- function(filter) {
  scores <- filter$scores
  if (is.null(scores) || length(scores) == 0L || is.null(names(scores))) {
    .abort("The feature filter did not return named scores.")
  }
  data.frame(
    feature = names(scores),
    score = as.numeric(scores),
    stringsAsFactors = FALSE
  )
}

.selection_binomial_folds <- function(y, nfolds, seed) {
  if (!is.numeric(nfolds) || length(nfolds) != 1L || is.na(nfolds) ||
      nfolds != as.integer(nfolds) || nfolds < 3L) {
    .abort("`nfolds` must be an integer of at least 3.")
  }
  counts <- table(y)
  if (any(counts < 3L)) {
    .abort("At least three observations per outcome class are required for LASSO.")
  }
  nfolds <- min(as.integer(nfolds), min(counts))
  if (nfolds < 3L) {
    .abort("At least three observations per outcome class are required for LASSO.")
  }
  set.seed(seed)
  fold_id <- integer(length(y))
  for (class in names(counts)) {
    index <- which(y == class)
    fold_id[index] <- sample(rep(seq_len(nfolds), length.out = length(index)))
  }
  fold_id
}

#' Select features with univariable logistic regression
#'
#' Fits one logistic regression per encoded feature and retains features with
#' a p-value below `p_threshold`.
#'
#' @param data A data frame, `uniml_data`, or fitted `uniml_preprocessor`.
#' @param outcome Outcome column name.
#' @param p_threshold P-value threshold used to retain a feature.
#'
#' @return A list with `selected_features`, `details`, `params`, and `scores`.
#' @export
#' @examples
#' data <- data.frame(
#'   y = factor(c("no", "yes", "no", "yes")),
#'   x = c(0, 1, 0.2, 0.8)
#' )
#' select_univariate(data, "y", p_threshold = 1)
select_univariate <- function(data, outcome, p_threshold = 0.2) {
  raw <- .selection_raw(data)
  .assert_scalar_number(p_threshold, "p_threshold", min = 0, max = 1)
  outcome_info <- .selection_outcome(raw, outcome)
  design <- .selection_matrix(raw, outcome)
  y <- outcome_info$values
  features <- colnames(design$matrix)
  p_values <- vapply(features, function(feature) {
    .selection_glm_pvalues(design$matrix[, feature], y)
  }, numeric(1))
  selected <- features[!is.na(p_values) & p_values < p_threshold]
  details <- .selection_details(
    features,
    selected,
    p_value = unname(p_values)
  )
  .new_selection_result(
    "univariate",
    selected,
    details,
    params = list(p_threshold = p_threshold),
    scores = p_values
  )
}

#' Select features with LASSO
#'
#' Fits a cross-validated binomial LASSO using `glmnet` and retains features
#' with non-zero coefficients at the selected lambda rule.
#'
#' @param data A data frame, `uniml_data`, or fitted `uniml_preprocessor`.
#' @param outcome Outcome column name.
#' @param lambda Lambda selection rule, `"1se"` or `"min"`.
#' @param nfolds Number of cross-validation folds.
#' @param seed Random seed.
#' @param alpha Elastic-net mixing parameter (1 = LASSO).
#' @param type_measure `cv.glmnet()` loss, `"auc"`, `"deviance"`, or `"class"`.
#' @param standardize Whether predictors are standardised inside `glmnet`.
#'
#' @return A list with `selected_features`, `details`, and `params`.
#' @export
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   y = factor(rep(c("no", "yes"), 10)),
#'   x1 = stats::rnorm(20),
#'   x2 = stats::rnorm(20)
#' )
#' select_lasso(data, "y", nfolds = 3)
#' }
select_lasso <- function(data, outcome, lambda = c("1se", "min"), nfolds = 10L,
                         seed = 123, alpha = 1, type_measure = "auc",
                         standardize = TRUE) {
  .check_packages("glmnet")
  raw <- .selection_raw(data)
  lambda <- match.arg(lambda)
  .assert_scalar_number(seed, "seed", integer = TRUE)
  .assert_scalar_number(alpha, "alpha", min = 0, max = 1)
  .assert_choice(type_measure, "type_measure", c("auc", "deviance", "class"))
  .assert_scalar_logical(standardize, "standardize")
  outcome_info <- .selection_outcome(raw, outcome)
  design <- .selection_matrix(raw, outcome)
  y <- as.integer(outcome_info$values) - 1L
  fold_id <- .selection_binomial_folds(y, nfolds, seed)

  set.seed(seed)
  fit <- glmnet::cv.glmnet(
    x = design$matrix,
    y = y,
    family = "binomial",
    alpha = alpha,
    foldid = fold_id,
    type.measure = type_measure,
    standardize = standardize
  )
  glmnet_lambda <- if (identical(lambda, "1se")) "lambda.1se" else "lambda.min"
  coefficients <- as.matrix(stats::coef(fit, s = glmnet_lambda))
  coefficient <- coefficients[, 1L]
  features <- rownames(coefficients)
  keep <- features != "(Intercept)" & coefficient != 0
  selected <- features[keep]
  details <- .selection_details(
    features,
    selected,
    coefficient = unname(coefficient)
  )
  details <- details[details$feature != "(Intercept)", , drop = FALSE]
  rownames(details) <- NULL
  .new_selection_result(
    "lasso",
    selected,
    details,
    params = list(lambda = lambda, nfolds = nfolds, seed = seed),
    scores = stats::setNames(abs(coefficient[features != "(Intercept)"]),
                             features[features != "(Intercept)"])
  )
}

#' Select features with Boruta
#'
#' Runs the Boruta all-relevant algorithm on the encoded feature matrix and
#' retains `Confirmed` features by default. The importance history is stored in
#' `scores$ImpHistory` together with the final decision, so that it is kept for
#' later inspection or plotting.
#'
#' @param data A data frame, `uniml_data`, or fitted `uniml_preprocessor`.
#' @param outcome Outcome column name.
#' @param maxRuns Maximum number of Boruta runs.
#' @param num_threads Number of threads used by Boruta.
#' @param seed Random seed.
#' @param tentative Whether tentative features are also retained.
#' @param ... Additional arguments passed to `Boruta::Boruta()`.
#'
#' @return A `uniml_selection_result` with `selected_features`, `details`,
#'   `params`, and `scores`.
#' @export
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   y = factor(rep(c("no", "yes"), 10)),
#'   x1 = stats::rnorm(20),
#'   x2 = stats::rnorm(20)
#' )
#' select_boruta(data, "y", maxRuns = 11, num_threads = 1)
#' }
select_boruta <- function(data, outcome, maxRuns = 50L, num_threads = 1L,
                          seed = 123, tentative = FALSE, ...) {
  .check_packages(c("Boruta", "ranger"))
  raw <- .selection_raw(data)
  .assert_scalar_number(maxRuns, "maxRuns", integer = TRUE, min = 11)
  .assert_scalar_number(num_threads, "num_threads", integer = TRUE, min = 1)
  .assert_scalar_number(seed, "seed", integer = TRUE)
  .assert_scalar_logical(tentative, "tentative")
  outcome_info <- .selection_outcome(raw, outcome)
  design <- .selection_matrix(raw, outcome)
  predictors <- as.data.frame(design$matrix, check.names = FALSE)
  set.seed(seed)
  boruta <- Boruta::Boruta(
    x = predictors,
    y = outcome_info$values,
    maxRuns = as.integer(maxRuns),
    holdHistory = TRUE,
    num.threads = as.integer(num_threads),
    ...
  )
  statistics <- tryCatch(
    Boruta::attStats(boruta),
    error = function(e) NULL
  )
  if (is.null(statistics) || nrow(statistics) == 0L) {
    .abort("Boruta did not return feature statistics.")
  }
  features <- rownames(statistics)
  decision <- stats::setNames(as.character(statistics$decision), features)
  keep_decision <- if (isTRUE(tentative)) {
    decision %in% c("Confirmed", "Tentative")
  } else {
    decision == "Confirmed"
  }
  selected <- names(decision)[keep_decision]
  details <- data.frame(
    feature = features,
    score = as.numeric(statistics$medianImp),
    selected = features %in% selected,
    decision = as.character(statistics$decision),
    importance = as.numeric(statistics$meanImp),
    stringsAsFactors = FALSE
  )
  rownames(details) <- NULL
  .new_selection_result(
    "boruta",
    selected,
    details,
    params = list(
      maxRuns = as.integer(maxRuns),
      num_threads = as.integer(num_threads),
      seed = as.integer(seed),
      tentative = isTRUE(tentative)
    ),
    scores = list(
      finalDecision = decision,
      ImpHistory = if (is.null(boruta$ImpHistory)) {
        NULL
      } else {
        as.matrix(boruta$ImpHistory)
      }
    )
  )
}

#' Select features with random-forest importance
#'
#' Fits a ranger random forest and ranks encoded features by importance. By
#' default the top `ceiling(sqrt(number_of_features))` features are retained.
#'
#' @param data A data frame, `uniml_data`, or fitted `uniml_preprocessor`.
#' @param outcome Outcome column name.
#' @param top_k Optional number of top features to retain.
#' @param importance_threshold Optional minimum importance score.
#' @param num_trees Number of trees in the random forest.
#' @param importance Importance type supported by ranger, `"impurity"` or
#'   `"permutation"`.
#' @param seed Random seed.
#'
#' @return A list with `selected_features`, `details`, and `params`.
#' @export
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   y = factor(rep(c("no", "yes"), 10)),
#'   x1 = stats::rnorm(20),
#'   x2 = stats::rnorm(20)
#' )
#' select_rf_importance(data, "y", top_k = 1, num_trees = 20)
#' }
select_rf_importance <- function(data, outcome, top_k = NULL,
                                 importance_threshold = NULL,
                                 num_trees = 100L,
                                 importance = c("impurity", "permutation"),
                                 seed = 123) {
  .check_packages("mlr3learners")
  raw <- .selection_raw(data)
  importance <- match.arg(importance)
  if (!is.null(top_k)) {
    .assert_scalar_number(top_k, "top_k", integer = TRUE, min = 1)
  }
  if (!is.null(importance_threshold)) {
    .assert_scalar_number(importance_threshold, "importance_threshold")
  }
  .assert_scalar_number(num_trees, "num_trees", integer = TRUE, min = 1)
  .assert_scalar_number(seed, "seed", integer = TRUE)
  outcome_info <- .selection_outcome(raw, outcome)
  design <- .selection_matrix(raw, outcome)
  task_data <- data.frame(design$matrix, check.names = FALSE)
  task_data[[outcome]] <- outcome_info$values
  task <- mlr3::as_task_classif(task_data, target = outcome, positive = levels(outcome_info$values)[2L])
  learner <- mlr3::lrn(
    "classif.ranger",
    num.trees = as.integer(num_trees),
    importance = importance,
    seed = as.integer(seed)
  )
  filter <- mlr3filters::flt("importance", learner = learner)
  filter$calculate(task)
  result <- .selection_filter_scores(filter)
  result <- result[order(-result$score, result$feature), , drop = FALSE]
  if (!is.null(importance_threshold)) {
    selected <- result$feature[result$score >= importance_threshold]
  } else {
    k <- if (is.null(top_k)) ceiling(sqrt(ncol(design$matrix))) else as.integer(top_k)
    k <- min(k, nrow(result))
    selected <- result$feature[seq_len(k)]
  }
  details <- data.frame(
    feature = result$feature,
    importance = result$score,
    selected = result$feature %in% selected,
    rank = seq_len(nrow(result)),
    stringsAsFactors = FALSE
  )
  .new_selection_result(
    "rf_importance",
    selected,
    details,
    params = list(
      top_k = top_k,
      importance_threshold = importance_threshold,
      num_trees = as.integer(num_trees),
      importance = importance,
      seed = as.integer(seed)
    )
  )
}

#' Select features with recursive feature elimination
#'
#' Uses `mlr3fselect` with a recursive feature elimination selector. By default
#' the target size is `ceiling(sqrt(number_of_features))`.
#'
#' @param data A data frame, `uniml_data`, or fitted `uniml_preprocessor`.
#' @param outcome Outcome column name.
#' @param n_features Optional target number of features.
#' @param learner Optional mlr3 learner. Defaults to `classif.rpart`.
#' @param resampling Optional mlr3 resampling object.
#' @param measure Optional mlr3 measure.
#' @param seed Random seed.
#'
#' @return A list with `selected_features`, `details`, and `params`.
#' @export
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   y = factor(rep(c("no", "yes"), 10)),
#'   x1 = stats::rnorm(20),
#'   x2 = stats::rnorm(20),
#'   x3 = stats::rnorm(20)
#' )
#' select_rfe(data, "y", n_features = 2)
#' }
select_rfe <- function(data, outcome, n_features = NULL, learner = NULL,
                       resampling = NULL, measure = NULL, seed = 123) {
  raw <- .selection_raw(data)
  .assert_scalar_number(seed, "seed", integer = TRUE)
  outcome_info <- .selection_outcome(raw, outcome)
  design <- .selection_matrix(raw, outcome)
  features <- colnames(design$matrix)
  target <- if (is.null(n_features)) ceiling(sqrt(length(features))) else n_features
  .assert_scalar_number(target, "n_features", integer = TRUE, min = 1)
  target <- min(as.integer(target), length(features))
  task_data <- data.frame(design$matrix, check.names = FALSE)
  task_data[[outcome]] <- outcome_info$values
  task <- mlr3::as_task_classif(task_data, target = outcome, positive = levels(outcome_info$values)[2L])

  if (is.null(learner)) {
    learner <- mlr3::lrn("classif.rpart", predict_type = "prob")
  }
  if (!inherits(learner, "Learner")) {
    .abort("`learner` must be an mlr3 learner.")
  }
  if (is.null(resampling)) {
    resampling <- mlr3::rsmp("holdout", ratio = 0.7)
  }
  if (!inherits(resampling, "Resampling")) {
    .abort("`resampling` must be an mlr3 resampling object.")
  }
  if (is.null(measure)) {
    measure <- if ("prob" %in% learner$properties) {
      mlr3::msr("classif.auc")
    } else {
      mlr3::msr("classif.ce")
    }
  }
  if (!inherits(measure, "Measure")) {
    .abort("`measure` must be an mlr3 measure.")
  }
  if ("prob" %in% learner$properties &&
      identical(measure$predict_type, "prob") &&
      !identical(learner$predict_type, "prob")) {
    learner <- learner$clone(deep = TRUE)
    learner$predict_type <- "prob"
  }

  set.seed(seed)
  selector <- mlr3fselect::fs(
    "rfe",
    n_features = target,
    feature_number = 1L
  )
  instance <- mlr3fselect::fselect(
    selector,
    task,
    learner,
    resampling,
    measure,
    store_models = FALSE
  )
  selected <- instance$result_feature_set
  details <- data.frame(
    feature = features,
    selected = features %in% selected,
    stringsAsFactors = FALSE
  )
  .new_selection_result(
    "rfe",
    selected,
    details,
    params = list(
      n_features = target,
      learner = learner$id,
      resampling = resampling$id,
      measure = measure$id,
      seed = as.integer(seed)
    )
  )
}

#' Select features with multivariable logistic regression
#'
#' Fits a multivariable logistic model and retains features using either
#' stepwise selection or repeated p-value elimination.
#'
#' @param data A data frame, `uniml_data`, or fitted `uniml_preprocessor`.
#' @param outcome Outcome column name.
#' @param method Multivariable method, `"stepwise"` or `"pvalue"`.
#' @param p_threshold P-value threshold for `"pvalue"`.
#'
#' @return A list with `selected_features`, `details`, and `params`.
#' @export
#' @examples
#' data <- data.frame(
#'   y = factor(c("no", "yes", "no", "yes", "no", "yes")),
#'   x1 = c(0, 1, 0, 1, 0, 1),
#'   x2 = c(1, 0, 1, 0, 1, 0)
#' )
#' select_multivariate(data, "y", method = "pvalue", p_threshold = 1)
select_multivariate <- function(data, outcome,
                                method = c("stepwise", "pvalue"),
                                p_threshold = 0.2) {
  raw <- .selection_raw(data)
  method <- match.arg(method)
  .assert_scalar_number(p_threshold, "p_threshold", min = 0, max = 1)
  outcome_info <- .selection_outcome(raw, outcome)
  design <- .selection_matrix(raw, outcome)
  model_data <- data.frame(design$matrix, check.names = FALSE)
  model_data[[outcome]] <- outcome_info$values
  full <- tryCatch(
    suppressWarnings(stats::glm(
      stats::as.formula(paste(outcome, "~ .")),
      data = model_data,
      family = stats::binomial()
    )),
    error = function(e) NULL
  )
  if (is.null(full)) {
    .abort("The multivariable logistic model could not be fitted.")
  }

  if (identical(method, "stepwise")) {
    model <- tryCatch(
      suppressWarnings(stats::step(full, direction = "both", trace = 0)),
      error = function(e) NULL
    )
    if (is.null(model)) {
      .abort("Stepwise feature selection could not be completed.")
    }
    selected <- names(stats::coef(model))
    selected <- setdiff(selected, "(Intercept)")
    coefficients <- summary(model)$coefficients
  } else {
    remaining <- colnames(design$matrix)
    model <- NULL
    coefficients <- NULL
    repeat {
      if (length(remaining) == 0L) {
        selected <- character()
        break
      }
      candidate <- tryCatch(
        suppressWarnings(stats::glm(
          stats::reformulate(remaining, response = outcome),
          data = model_data,
          family = stats::binomial()
        )),
        error = function(e) NULL
      )
      if (is.null(candidate)) {
        .abort("P-value feature selection could not be completed.")
      }
      candidate_coefficients <- summary(candidate)$coefficients
      terms <- setdiff(rownames(candidate_coefficients), "(Intercept)")
      if (length(terms) == 0L) {
        selected <- character()
        model <- candidate
        coefficients <- candidate_coefficients
        break
      }
      p_values <- candidate_coefficients[terms, "Pr(>|z|)"]
      if (anyNA(p_values)) {
        worst <- terms[which(is.na(p_values))[1L]]
      } else if (all(p_values <= p_threshold)) {
        selected <- terms
        model <- candidate
        coefficients <- candidate_coefficients
        break
      } else {
        worst <- terms[which.max(p_values)]
      }
      remaining <- setdiff(remaining, worst)
    }
  }

  terms <- if (is.null(coefficients)) {
    character()
  } else {
    setdiff(rownames(coefficients), "(Intercept)")
  }
  p_values <- if (length(terms) == 0L) {
    numeric()
  } else {
    unname(coefficients[terms, "Pr(>|z|)"])
  }
  coefficient <- if (length(terms) == 0L) {
    numeric()
  } else {
    unname(coefficients[terms, "Estimate"])
  }
  details <- .selection_details(
    colnames(design$matrix),
    selected,
    p_value = stats::setNames(
      ifelse(colnames(design$matrix) %in% terms, p_values[match(colnames(design$matrix), terms)], NA_real_),
      colnames(design$matrix)
    ),
    coefficient = stats::setNames(
      ifelse(colnames(design$matrix) %in% terms, coefficient[match(colnames(design$matrix), terms)], NA_real_),
      colnames(design$matrix)
    )
  )
  .new_selection_result(
    "multivariate",
    selected,
    details,
    params = list(method = method, p_threshold = p_threshold)
  )
}

#' Combine feature-selection results
#'
#' Combines a list of selected feature vectors or selection result objects.
#'
#' @param selections A non-empty list of character vectors or selection results.
#' @param how Combination rule, `"intersect"` or `"union"`.
#'
#' @return A sorted character vector of selected feature names.
#' @export
#' @examples
#' combine_selections(list(c("x1", "x2"), c("x2", "x3")), how = "intersect")
combine_selections <- function(selections, how = c("intersect", "union")) {
  how <- match.arg(how)
  if (!is.list(selections) || length(selections) == 0L) {
    .abort("`selections` must be a non-empty list.")
  }
  sets <- lapply(selections, function(x) {
    if (is.character(x)) {
      return(unique(x))
    }
    if (is.list(x) && !is.null(x$selected_features)) {
      return(unique(x$selected_features))
    }
    .abort("Each selection must be a character vector or a selection result.")
  })
  if (any(vapply(sets, function(x) anyNA(x) || any(!nzchar(x)), logical(1)))) {
    .abort("Selected feature names must be non-missing and non-empty.")
  }
  result <- if (identical(how, "intersect")) {
    Reduce(intersect, sets)
  } else {
    unique(unlist(sets, use.names = FALSE))
  }
  sort(result)
}

#' Run feature selection from a uniml specification
#'
#' Runs one or more feature-selection methods on the training/encoded data,
#' combines their results, and maps selected encoded columns back to original
#' variables when encoding metadata is available.
#'
#' @param data A data frame, `uniml_data`, or fitted `uniml_preprocessor`.
#' @param spec A `uniml_spec` object.
#'
#' @return A list containing selected encoded and original features, method
#'   details, and optional collinearity results.
#' @export
#' @examples
#' \dontrun{
#' spec <- uniml_spec(task_type = "binary", outcome = "y", positive = "yes")
#' spec <- set_feature_selection(spec, methods = "lasso")
#' data <- data.frame(
#'   y = factor(rep(c("no", "yes"), 10)),
#'   x1 = stats::rnorm(20),
#'   x2 = stats::rnorm(20)
#' )
#' select_features(data, spec)
#' }
select_features <- function(data, spec) {
  validate_uniml_spec(spec)
  selection <- spec$selection
  if (is.null(selection) || is.null(selection$methods)) {
    .abort("`spec$selection$methods` must be supplied.")
  }
  raw <- .selection_raw(data)
  outcome_info <- .selection_outcome(raw, spec$outcome)
  design <- .selection_matrix(raw, spec$outcome)
  design$outcome <- outcome_info$values
  mapping <- .selection_mapping(data, raw, design, spec$outcome)

  collinearity <- NULL
  candidates <- colnames(design$matrix)
  if (!is.null(selection$cor_threshold) || !is.null(selection$vif_threshold)) {
    collinearity <- remove_collinear(
      design$matrix,
      cor_threshold = selection$cor_threshold,
      vif_threshold = selection$vif_threshold
    )
    candidates <- collinearity$selected_features
    if (length(candidates) == 0L) {
      .abort("No features remain after collinearity removal.")
    }
  }

  method_data <- .selection_data_for_method(
    list(
      matrix = design$matrix[, candidates, drop = FALSE],
      outcome = design$outcome
    ),
    spec$outcome
  )
  methods <- unique(.selection_methods(selection$methods))
  method_results <- vector("list", length(methods))
  names(method_results) <- methods
  extra_args <- if (is.null(selection$args)) list() else selection$args
  for (method in methods) {
    arguments <- list(data = method_data, outcome = spec$outcome)
    switch(
      method,
      univariate = {
        arguments$p_threshold <- selection$p_threshold
      },
      lasso = {
        arguments$lambda <- selection$lambda
        arguments$seed <- spec$seed
      },
      boruta = {
        arguments$maxRuns <- selection$maxRuns
        arguments$num_threads <- as.integer(spec$n_workers)
        arguments$seed <- spec$seed
      },
      rf_importance = {
        arguments$top_k <- selection$k
        arguments$seed <- spec$seed
      },
      rfe = {
        arguments$n_features <- selection$k
        arguments$seed <- spec$seed
      },
      multivariate = {
        arguments$method <- selection$multivariate
        arguments$p_threshold <- selection$p_threshold
      }
    )
    extra <- extra_args[[method]]
    if (length(extra) > 0L) {
      extra <- extra[setdiff(names(extra), names(arguments))]
      arguments[names(extra)] <- extra
    }
    method_results[[method]] <- do.call(
      switch(
        method,
        univariate = select_univariate,
        lasso = select_lasso,
        boruta = select_boruta,
        rf_importance = select_rf_importance,
        rfe = select_rfe,
        multivariate = select_multivariate
      ),
      arguments
    )
  }

  combined <- combine_selections(method_results, how = selection$combine)
  if (length(combined) == 0L) {
    .abort("Feature selection returned no features.")
  }
  represented <- .apply_representation(
    combined,
    mapping,
    selection$representation
  )
  list(
    selected_features = represented$selected_features,
    original_features = represented$original_features,
    representation = selection$representation,
    combine = selection$combine,
    methods = method_results,
    details = .selection_combine_details(method_results, combined),
    collinearity = collinearity,
    mapping = mapping
  )
}
