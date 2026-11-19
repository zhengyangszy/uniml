# Feature selection - mlr3filters-backed filters and sequential search --------
#
# Five filters are thin, uniform wrappers over `mlr3filters::flt()`: the
# permutation filter is Breiman's (2001) original importance idea, the
# information-gain filter is Shannon's information gain as scored on the
# task, and the rest follow the filter keys the mlr3 community maintains
# (Becker et al., doi:10.32614/CRAN.package.mlr3filters). One filter-key, one
# wrapper, one convention for the top-k threshold.
#
# `select_sequential()` exposes the wrapper method the tutorial calls
# sequential forward selection through `mlr3fselect::fselect()` with a
# `sequential` fselector, so the wrapper resamples a learner on the encoded
# feature matrix instead of scoring features one at a time.

.mlr3filter_task <- function(data, outcome) {
  raw <- .selection_raw(data)
  outcome_info <- .selection_outcome(raw, outcome)
  design <- .selection_matrix(raw, outcome)
  frame <- .selection_data_for_method(design, outcome)
  frame[[outcome]] <- outcome_info$values
  task <- tryCatch(
    mlr3::as_task_classif(frame, target = outcome),
    error = function(e) NULL
  )
  if (is.null(task)) {
    .abort(paste0(
      "The feature filters need a factor outcome column that forms a valid ",
      "classification task."
    ))
  }
  list(task = task, features = colnames(design$matrix), frame = frame)
}

.mlr3filter_top_k <- function(scores, k) {
  scores <- scores[is.finite(scores)]
  if (length(scores) == 0L) {
    .abort("The filter produced no finite scores.")
  }
  if (is.null(k)) {
    k <- ceiling(sqrt(length(scores)))
  } else {
    .assert_scalar_number(k, "k", integer = TRUE, min = 1)
    k <- min(as.integer(k), length(scores))
  }
  selected <- utils::head(names(sort(scores, decreasing = TRUE)), k)
  list(selected = selected, scores = scores)
}

.mlr3filter_result <- function(method, selected, scores, details, params) {
  .new_selection_result(
    method,
    selected,
    details,
    params = params,
    scores = stats::setNames(as.numeric(scores), names(scores))
  )
}

#' Select features with a mlr3filters filter
#'
#' The shared engine behind [select_information_gain()],
#' [select_jmim()], [select_permutation()], [select_auc_filter()], and
#' [select_correlation()]: each builds a classification task from the encoded
#' feature matrix, scores every feature with the chosen `mlr3filters` filter,
#' and keeps the top `k` (by default roughly the square root of the number of
#' features, the same convention the other importance-based selectors use).
#'
#' @param data A data frame, `uniml_data`, or fitted `uniml_preprocessor`.
#' @param outcome Outcome column name.
#' @param k Number of features to keep; `NULL` keeps about the square root of
#'   the feature count.
#' @param learner Learner for the permutation filter (`rpart` by default).
#' @param seed Random seed.
#'
#' @return A `uniml_selection_result`.
#' @references
#' Breiman L (2001). Random forests. *Machine Learning*, 45(1), 5-32.
#' doi:10.1023/A:1010933404324
#'
#' Becker M, Schratz P, Lang M, Bischl B, Binder M, Zobolas J (2026).
#' mlr3filters: Filter Based Feature Selection for 'mlr3'. R package version
#' 0.9.1. doi:10.32614/CRAN.package.mlr3filters
#' @keywords internal
.mlr3filter_select <- function(data, outcome, filter_key, k = NULL,
                               learner = NULL, seed = 123L) {
  prepared <- .mlr3filter_task(data, outcome)
  filter <- if (is.null(learner)) {
    mlr3filters::flt(filter_key)
  } else {
    mlr3filters::flt(filter_key, learner = learner)
  }
  filter$calculate(prepared$task)
  chosen <- .mlr3filter_top_k(filter$scores, k)
  details <- .selection_details(
    prepared$features,
    chosen$selected,
    score = unname(chosen$scores[prepared$features])
  )
  .mlr3filter_result(
    filter_key,
    chosen$selected,
    chosen$scores,
    details,
    params = list(k = length(chosen$selected), seed = seed)
  )
}

#' Select features by information gain
#'
#' Keeps the top-`k` features by information gain: the reduction in outcome
#' uncertainty (mutual information between feature and outcome), as scored by
#' `mlr3filters::flt("information_gain")`.
#'
#' @param data A data frame, `uniml_data`, or fitted `uniml_preprocessor`.
#' @param outcome Outcome column name.
#' @param k Number of features to keep; `NULL` keeps about the square root of
#'   the feature count.
#'
#' @return A `uniml_selection_result` with `selected_features`, `details`,
#'   `params`, and `scores`.
#' @references
#' Cover TM, Thomas JA (2006). *Elements of Information Theory*, 2nd edition.
#' Wiley. doi:10.1002/047174882X
#'
#' Becker M, Schratz P, Lang M, Bischl B, Binder M, Zobolas J (2026).
#' mlr3filters: Filter Based Feature Selection for 'mlr3'. R package version
#' 0.9.1. doi:10.32614/CRAN.package.mlr3filters
#' @export
#' @examples
#' data <- data.frame(
#'   x1 = rnorm(100), x2 = rnorm(100),
#'   y = factor(rbinom(100, 1, 0.5))
#' )
#' select_information_gain(data, "y")
select_information_gain <- function(data, outcome, k = NULL) {
  .mlr3filter_select(data, outcome, "information_gain", k = k)
}

#' Select features by joint mutual information maximisation
#'
#' Keeps the top-`k` features by the JMIM criterion, as scored by
#' `mlr3filters::flt("jmim")`. JMIM maximises the joint mutual information of
#' a feature with the already-selected set, which restrains the selection from
#' piling onto redundant features.
#'
#' @inheritParams select_information_gain
#'
#' @return A `uniml_selection_result`.
#' @references
#' Becker M, Schratz P, Lang M, Bischl B, Binder M, Zobolas J (2026).
#' mlr3filters: Filter Based Feature Selection for 'mlr3'. R package version
#' 0.9.1. doi:10.32614/CRAN.package.mlr3filters
#' @export
#' @examples
#' data <- data.frame(
#'   x1 = rnorm(100), x2 = rnorm(100),
#'   y = factor(rbinom(100, 1, 0.5))
#' )
#' select_jmim(data, "y")
select_jmim <- function(data, outcome, k = NULL) {
  .mlr3filter_select(data, outcome, "jmim", k = k)
}

#' Select features by single-feature AUC
#'
#' Keeps the top-`k` features by the area under the ROC curve of the feature
#' alone against the outcome, as scored by `mlr3filters::flt("auc")`. Binary
#' outcomes only - the AUC is defined for one positive class.
#'
#' @inheritParams select_information_gain
#'
#' @return A `uniml_selection_result`.
#' @references
#' Becker M, Schratz P, Lang M, Bischl B, Binder M, Zobolas J (2026).
#' mlr3filters: Filter Based Feature Selection for 'mlr3'. R package version
#' 0.9.1. doi:10.32614/CRAN.package.mlr3filters
#' @export
#' @examples
#' data <- data.frame(
#'   x1 = rnorm(100), x2 = rnorm(100),
#'   y = factor(rbinom(100, 1, 0.5))
#' )
#' select_auc_filter(data, "y")
select_auc_filter <- function(data, outcome, k = NULL) {
  task <- .mlr3filter_task(data, outcome)$task
  if (length(task$class_names) != 2L) {
    .abort(paste0(
      "The single-feature AUC filter is defined for binary outcomes only; ",
      "this task has ", length(task$class_names), " classes."
    ))
  }
  .mlr3filter_select(data, outcome, "auc", k = k)
}

#' Select features by permutation importance
#'
#' Keeps the top-`k` features by Breiman's permutation importance: shuffle the
#' feature and watch a small `rpart` model's performance drop, as scored by
#' `mlr3filters::flt("permutation")` with its default resampling.
#'
#' @inheritParams select_information_gain
#' @param learner Learner whose performance the permutation perturbs; defaults
#'   to a fast classification tree.
#' @param seed Random seed.
#'
#' @return A `uniml_selection_result`.
#' @references
#' Breiman L (2001). Random forests. *Machine Learning*, 45(1), 5-32.
#' doi:10.1023/A:1010933404324
#'
#' Becker M, Schratz P, Lang M, Bischl B, Binder M, Zobolas J (2026).
#' mlr3filters: Filter Based Feature Selection for 'mlr3'. R package version
#' 0.9.1. doi:10.32614/CRAN.package.mlr3filters
#' @export
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   x1 = rnorm(100), x2 = rnorm(100),
#'   y = factor(rbinom(100, 1, 0.5))
#' )
#' select_permutation(data, "y")
#' }
select_permutation <- function(data, outcome, k = NULL,
                               learner = mlr3::lrn("classif.rpart"),
                               seed = 123L) {
  .mlr3filter_select(data, outcome, "permutation", k = k,
                     learner = learner, seed = seed)
}

#' Select features by correlation
#'
#' Keeps the top-`k` features by the absolute Pearson correlation between the
#' feature and the binary outcome encoded as 0/1. Computed directly here:
#' `mlr3filters`'s correlation filter is defined for numeric targets only.
#'
#' @inheritParams select_information_gain
#'
#' @return A `uniml_selection_result`.
#' @references
#' Becker M, Schratz P, Lang M, Bischl B, Binder M, Zobolas J (2026).
#' mlr3filters: Filter Based Feature Selection for 'mlr3'. R package version
#' 0.9.1. doi:10.32614/CRAN.package.mlr3filters
#' @export
#' @examples
#' data <- data.frame(
#'   x1 = rnorm(100), x2 = rnorm(100),
#'   y = factor(rbinom(100, 1, 0.5))
#' )
#' select_correlation(data, "y")
select_correlation <- function(data, outcome, k = NULL) {
  prepared <- .mlr3filter_task(data, outcome)
  y_num <- as.numeric(prepared$task$truth()) - 1L
  scores <- vapply(prepared$features, function(column) {
    x <- prepared$frame[[column]]
    if (!is.numeric(x)) {
      return(NA_real_)
    }
    stats::cor(x, y_num, use = "complete.obs")
  }, numeric(1))
  names(scores) <- prepared$features
  chosen <- .mlr3filter_top_k(abs(scores), k)
  details <- .selection_details(
    prepared$features,
    chosen$selected,
    score = unname(abs(scores[prepared$features]))
  )
  .mlr3filter_result(
    "correlation",
    chosen$selected,
    scores,
    details,
    params = list(k = length(chosen$selected))
  )
}

#' Select features by sequential search
#'
#' Wrapper feature selection through `mlr3fselect::fselect()` with a
#' `sequential` fselector: forward selection starts from no features and adds
#' whichever feature improves the cross-validated score most; backward
#' selection starts from all of them and removes greedily.
#'
#' @param data A data frame, `uniml_data`, or fitted `uniml_preprocessor`.
#' @param outcome Outcome column name.
#' @param direction `"forward"` or `"backward"`.
#' @param folds Cross-validation folds for the inner score.
#' @param seed Random seed.
#'
#' @return A `uniml_selection_result`.
#' @references
#' Bischl B, Sonabend R, Kotthoff L, Lang M (2023). *Applied Machine Learning
#' Using mlr3 in R*. CRC Press. doi:10.1201/9781003402848
#' @export
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   x1 = rnorm(100), x2 = rnorm(100),
#'   y = factor(rbinom(100, 1, 0.5))
#' )
#' select_sequential(data, "y", direction = "forward")
#' }
select_sequential <- function(data, outcome,
                              direction = c("forward", "backward"),
                              folds = 3L, seed = 123L) {
  direction <- match.arg(direction)
  .assert_scalar_number(folds, "folds", integer = TRUE, min = 2)
  .assert_scalar_number(seed, "seed", integer = TRUE)
  prepared <- .mlr3filter_task(data, outcome)
  task <- prepared$task
  binary <- length(task$class_names) == 2L
  measure <- if (binary) mlr3::msr("classif.auc") else mlr3::msr("classif.ce")
  learner <- mlr3::lrn("classif.rpart", predict_type = "prob")
  resampling <- mlr3::rsmp("cv", folds = folds)
  resampling$instantiate(task)
  set.seed(seed)
  instance <- mlr3fselect::fselect(
    fselector = mlr3fselect::fs(
      "sequential",
      strategy = switch(direction, forward = "sfs", backward = "sbs")
    ),
    task = task,
    learner = learner,
    resampling = resampling,
    measure = measure
  )
  selected <- instance$result_feature_names
  if (length(selected) == 0L) {
    .abort(paste0(
      "Sequential search found no feature that improved the score. Try the ",
      "other direction, more folds, or the correlation/information-gain ",
      "filters instead."
    ))
  }
  details <- .selection_details(
    prepared$features,
    selected,
    score = NA_real_
  )
  .mlr3filter_result(
    sprintf("sequential_%s", direction),
    selected,
    stats::setNames(numeric(0), character(0)),
    details,
    params = list(direction = direction, folds = folds, seed = seed)
  )
}
