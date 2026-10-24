# L8 SHAP computation --------------------------------------------------------

.shap_require_packages <- function(native = FALSE) {
  packages <- c("kernelshap", "shapviz")
  if (isTRUE(native)) {
    packages <- c(packages, "xgboost")
  }
  .check_packages(packages)
  invisible(TRUE)
}

.shap_model_records <- function(x) {
  if (inherits(x, "Learner")) {
    return(list(list(name = x$id, learner = x)))
  }
  if (!is.list(x)) {
    .abort("`x` must contain one or more trained mlr3 learners.")
  }
  if (inherits(x, "uniml_result")) {
    x <- x$models
  }
  if (inherits(x$learner, "Learner")) {
    name <- x$name
    if (is.null(name)) {
      name <- x$learner$id
    }
    return(list(list(name = name, learner = x$learner)))
  }
  candidates <- if (!is.null(x$models) && is.list(x$models)) x$models else x
  if (length(candidates) == 0L) {
    .abort("`x` must contain at least one trained mlr3 learner.")
  }
  item_names <- names(candidates)
  records <- lapply(seq_along(candidates), function(index) {
    item <- candidates[[index]]
    learner <- if (inherits(item, "Learner")) {
      item
    } else if (is.list(item) && inherits(item$learner, "Learner")) {
      item$learner
    } else {
      NULL
    }
    if (is.null(learner)) {
      return(NULL)
    }
    name <- if (!is.null(item_names) && nzchar(item_names[index])) {
      item_names[index]
    } else {
      learner$id
    }
    list(name = name, learner = learner)
  })
  records <- records[!vapply(records, is.null, logical(1))]
  if (length(records) == 0L) {
    .abort("`x` must contain one or more trained mlr3 learners.")
  }
  records
}

.shap_trained_feature_names <- function(learner) {
  task <- learner$state$train_task
  if (is.null(task) || !inherits(task, "TaskClassif")) {
    return(NULL)
  }
  task$feature_names
}

.shap_data_bundle <- function(x, data, raw_data, feature_names) {
  if (is.null(data) && is.list(x)) {
    data <- x$data
  }
  if (is.null(raw_data) && is.list(x)) {
    raw_data <- x$raw_data
  }
  if (is.null(data)) {
    .abort("`data` is required for SHAP computation.")
  }
  if (!is.data.frame(data) && !is.matrix(data)) {
    .abort("`data` must be a data frame or matrix.")
  }
  data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  if (!is.null(feature_names)) {
    .assert_character_vector(feature_names, "feature_names")
    missing <- setdiff(feature_names, names(data))
    if (length(missing) > 0L) {
      .abort(sprintf(
        "Feature(s) not found in `data`: %s.",
        paste(missing, collapse = ", ")
      ))
    }
    data <- data[, feature_names, drop = FALSE]
  }
  if (nrow(data) == 0L || ncol(data) == 0L) {
    .abort("`data` must contain at least one row and one feature.")
  }

  if (is.null(raw_data)) {
    raw_data <- data
  } else {
    if (!is.data.frame(raw_data) && !is.matrix(raw_data)) {
      .abort("`raw_data` must be NULL, a data frame, or a matrix.")
    }
    raw_data <- as.data.frame(
      raw_data,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    missing_raw <- setdiff(names(data), names(raw_data))
    if (length(missing_raw) > 0L) {
      .abort(sprintf(
        "Feature(s) not found in `raw_data`: %s.",
        paste(missing_raw, collapse = ", ")
      ))
    }
    raw_data <- raw_data[, names(data), drop = FALSE]
  }
  list(data = data, raw_data = raw_data)
}

.shap_row_sample <- function(n, n_explain, seed) {
  if (n_explain >= n) {
    return(seq_len(n))
  }
  set.seed(as.integer(seed))
  sort(sample.int(n, as.integer(n_explain)))
}

.shap_positive_label <- function(learner, positive = NULL) {
  if (!is.null(positive)) {
    return(as.character(positive))
  }
  task <- learner$state$train_task
  if (!is.null(task) && !is.null(task$positive)) {
    return(as.character(task$positive))
  }
  NULL
}

.shap_positive_index <- function(probs, positive = NULL) {
  if (ncol(probs) == 1L) {
    return(1L)
  }
  if (is.null(positive)) {
    return(ncol(probs))
  }
  index <- match(as.character(positive), colnames(probs))
  if (is.na(index)) {
    .abort(sprintf(
      "Positive class \"%s\" is not present in the prediction probabilities.",
      positive
    ))
  }
  index
}

.shap_prediction_probability <- function(learner, newdata, positive = NULL) {
  newdata <- as.data.frame(newdata, check.names = FALSE)
  prediction <- learner$predict_newdata(newdata)
  if (is.null(prediction$prob) || ncol(prediction$prob) == 0L) {
    .abort("The learner did not return classification probabilities.")
  }
  index <- .shap_positive_index(prediction$prob, positive = positive)
  as.numeric(prediction$prob[, index])
}

.shap_numeric_matrix <- function(data) {
  non_numeric <- names(data)[!vapply(data, function(z) {
    is.numeric(z) || is.logical(z)
  }, logical(1))]
  if (length(non_numeric) > 0L) {
    .abort(sprintf(
      "Native SHAP requires numeric encoded features; non-numeric column(s): %s.",
      paste(non_numeric, collapse = ", ")
    ))
  }
  matrix <- as.matrix(data)
  storage.mode(matrix) <- "double"
  matrix
}

.shap_positive_index_native <- function(learner, positive = NULL) {
  task <- learner$state$train_task
  if (is.null(task)) {
    return(NULL)
  }
  class_names <- task$class_names
  if (is.null(positive)) {
    return(NULL)
  }
  index <- match(as.character(positive), class_names)
  if (is.na(index)) {
    return(NULL)
  }
  as.integer(index)
}

.shap_native_object <- function(learner, data, positive = NULL) {
  model <- learner$model
  native <- if (inherits(model, "xgb.Booster")) {
    get("shapviz.xgb.Booster", envir = asNamespace("shapviz"))
  } else if (inherits(model, "lgb.Booster")) {
    get("shapviz.lgb.Booster", envir = asNamespace("shapviz"))
  } else {
    return(NULL)
  }
  matrix <- .shap_numeric_matrix(data)
  class_index <- .shap_positive_index_native(learner, positive = positive)
  native(
    model,
    X_pred = matrix,
    X = data,
    which_class = class_index
  )
}

# Model-agnostic Kernel SHAP through `kernelshap`. Every row is explained
# against a background sample, so the cost grows with rows x features x `bg_n`;
# `hybrid_degree = 1` keeps the exact part at 2p coalitions and is roughly half
# the cost of the default degree 2. Users can override any `kernelshap()`
# argument through `...`.
.shap_kernel_object <- function(learner, data, positive = NULL, bg_n = 100L,
                                seed = 123L, parallel = FALSE, ...) {
  wrapper <- function(object, newdata) {
    .shap_prediction_probability(
      object,
      newdata,
      positive = .shap_positive_label(object, positive = positive)
    )
  }
  settings <- list(
    object = learner,
    X = data,
    bg_X = data,
    pred_fun = wrapper,
    bg_n = as.integer(bg_n),
    hybrid_degree = 1L,
    parallel = parallel,
    verbose = FALSE,
    seed = as.integer(seed)
  )
  overrides <- list(...)
  if (length(overrides) > 0L) {
    settings[names(overrides)] <- overrides
  }
  explanation <- do.call(kernelshap::kernelshap, settings)
  shapviz::shapviz(explanation)
}

.shap_object_matrix <- function(object) {
  values <- shapviz::get_shap_values(object)
  if (is.list(values)) {
    if (length(values) != 1L) {
      .abort("Binary SHAP explanations must have exactly one SHAP matrix.")
    }
    values <- values[[1L]]
  }
  if (!is.matrix(values)) {
    values <- as.matrix(values)
  }
  storage.mode(values) <- "double"
  values
}

.shap_object_features <- function(object) {
  values <- shapviz::get_feature_values(object)
  as.data.frame(values, stringsAsFactors = FALSE, check.names = FALSE)
}

.shap_rows_by_model <- function(items, matrix_fun, model_names) {
  rows <- lapply(seq_along(items), function(index) {
    item <- items[[index]]
    if (is.null(item)) {
      return(NULL)
    }
    value <- matrix_fun(item)
    if (is.null(value)) {
      return(NULL)
    }
    out <- as.data.frame(value, stringsAsFactors = FALSE, check.names = FALSE)
    out$model <- model_names[[index]]
    out
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0L) {
    return(NULL)
  }
  columns <- unique(unlist(lapply(rows, names), use.names = FALSE))
  columns <- c(setdiff(columns, "model"), "model")
  rows <- lapply(rows, function(row) {
    row[setdiff(columns, names(row))] <- NA
    row[, columns, drop = FALSE]
  })
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

.shap_importance_table <- function(items, model_names) {
  rows <- lapply(seq_along(items), function(index) {
    item <- items[[index]]
    if (is.null(item)) {
      return(NULL)
    }
    values <- .shap_object_matrix(item)
    importance <- colMeans(abs(values), na.rm = TRUE)
    importance[!is.finite(importance)] <- 0
    ordering <- order(-importance, names(importance))
    data.frame(
      model = model_names[[index]],
      feature = names(importance)[ordering],
      mean_abs_shap = as.numeric(importance[ordering]),
      direction = ifelse(colMeans(values, na.rm = TRUE)[ordering] >= 0, "+", "-"),
      rank = seq_along(ordering),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0L) {
    return(NULL)
  }
  do.call(rbind, rows)
}

.shap_combine_objects <- function(objects) {
  if (length(objects) == 1L) {
    return(objects[[1L]])
  }
  columns <- lapply(objects, function(object) colnames(.shap_object_matrix(object)))
  if (length(unique(columns)) == 1L) {
    return(shapviz::mshapviz(objects))
  }
  .abort(
    paste0(
      "SHAP objects cannot be combined because their feature columns differ. ",
      "Use one model at a time or provide models trained on the same features."
    )
  )
}

.shap_value_rows <- function(items, model_names) {
  .shap_rows_by_model(
    items,
    matrix_fun = .shap_object_matrix,
    model_names = model_names
  )
}

.shap_feature_rows <- function(items, model_names) {
  .shap_rows_by_model(
    items,
    matrix_fun = .shap_object_features,
    model_names = model_names
  )
}

.explain_shap_uniml_binary <- function(x, data = NULL, raw_data = NULL,
                                       feature_names = NULL, n_explain = 100L,
                                       bg_n = 100L, seed = 123L,
                                       positive = NULL,
                                       parallel = FALSE, ...) {
  .shap_require_packages()
  .assert_scalar_number(n_explain, "n_explain", integer = TRUE, min = 1)
  .assert_scalar_number(bg_n, "bg_n", integer = TRUE, min = 1)
  .assert_scalar_number(seed, "seed", integer = TRUE)
  .assert_scalar_logical(parallel, "parallel")

  records <- .shap_model_records(x)
  bundle <- .shap_data_bundle(
    x,
    data = data,
    raw_data = raw_data,
    feature_names = feature_names
  )
  rows <- .shap_row_sample(nrow(bundle$data), n_explain, seed)
  if (length(rows) < nrow(bundle$data)) {
    bundle$data <- bundle$data[rows, , drop = FALSE]
    bundle$raw_data <- bundle$raw_data[rows, , drop = FALSE]
  }
  model_names <- vapply(records, `[[`, character(1), "name")
  if (anyDuplicated(model_names) > 0L) {
    .abort("Model names must be unique for SHAP computation.")
  }

  objects_std <- vector("list", length(records))
  objects_raw <- vector("list", length(records))
  errors <- list()
  for (index in seq_along(records)) {
    record <- records[[index]]
    learner <- record$learner
    model_features <- .shap_trained_feature_names(learner)
    if (is.null(model_features)) {
      model_features <- names(bundle$data)
    }
    missing <- setdiff(model_features, names(bundle$data))
    if (length(missing) > 0L) {
      errors[[record$name]] <- sprintf(
        "Feature(s) missing for model \"%s\": %s.",
        record$name,
        paste(missing, collapse = ", ")
      )
      next
    }
    model_data <- bundle$data[, model_features, drop = FALSE]
    model_raw_data <- bundle$raw_data[, model_features, drop = FALSE]
    result <- tryCatch(
      {
        native <- .shap_native_object(
          learner,
          data = model_data,
          positive = positive
        )
        if (is.null(native)) {
          native <- .shap_kernel_object(
            learner,
            data = model_data,
            positive = positive,
            bg_n = bg_n,
            seed = seed + index - 1L,
            parallel = parallel,
            ...
          )
        }
        values <- .shap_object_matrix(native)
        raw_object <- shapviz::shapviz(values, X = model_raw_data)
        list(std = native, raw = raw_object)
      },
      error = function(e) e
    )
    if (inherits(result, "error")) {
      errors[[record$name]] <- conditionMessage(result)
      next
    }
    objects_std[[index]] <- result$std
    objects_raw[[index]] <- result$raw
  }

  available <- !vapply(objects_std, is.null, logical(1))
  names(objects_std) <- model_names
  names(objects_raw) <- model_names
  available_names <- model_names[available]
  available_std <- objects_std[available]
  available_raw <- objects_raw[available]

  if (length(errors) > 0L) {
    warning(
      paste(
        "SHAP computation was skipped for some models:",
        paste(
          vapply(
            names(errors),
            function(name) sprintf("%s (%s)", name, errors[[name]]),
            character(1)
          ),
          collapse = "; "
        )
      ),
      call. = FALSE
    )
  }

  out <- structure(
    list(
      objects = if (length(available_std) > 0L) .shap_combine_objects(available_std) else NULL,
      objects_raw = if (length(available_raw) > 0L) .shap_combine_objects(available_raw) else NULL,
      values_matrix_std = .shap_value_rows(available_std, available_names),
      values_matrix_raw = .shap_value_rows(available_raw, available_names),
      feature_values_std = .shap_feature_rows(available_std, available_names),
      feature_values_raw = .shap_feature_rows(available_raw, available_names),
      importance = .shap_importance_table(available_std, available_names),
      errors = errors,
      models = available_names,
      n_explain = nrow(bundle$data),
      bg_n = as.integer(bg_n),
      seed = as.integer(seed)
    ),
    class = c("uniml_shap", "list")
  )
  if (is.null(out$objects)) {
    warning("No model produced a supported SHAP explanation.", call. = FALSE)
  }
  out
}

#' @rdname explain_shap
#' @param data Feature data used for SHAP computation.
#' @param raw_data Optional raw feature data aligned with `data`.
#' @param feature_names Optional feature subset.
#' @param n_explain Number of rows to explain. Larger values are subsampled with
#'   `seed`, because the cost of Kernel SHAP grows linearly with the number of
#'   explained rows.
#' @param bg_n Number of background rows used to integrate out inactive
#'   features.
#' @param seed Random seed.
#' @param positive Positive class label.
#' @param parallel Whether `kernelshap` may loop over rows in parallel with a
#'   registered `future` backend.
#' @param task_type Task type. Only `"binary"` is implemented.
#' @param ... Additional arguments passed to `kernelshap::kernelshap()`, such as
#'   `exact`, `hybrid_degree`, `m`, `tol`, or `max_iter`.
#' @export
explain_shap.Learner <- function(x, data = NULL, raw_data = NULL,
                                 feature_names = NULL, n_explain = 100L,
                                 bg_n = 100L, seed = 123L, positive = NULL,
                                 parallel = FALSE, task_type = "binary", ...) {
  .assert_choice(task_type, "task_type", "binary")
  .explain_shap_uniml_binary(
    x,
    data = data,
    raw_data = raw_data,
    feature_names = feature_names,
    n_explain = n_explain,
    bg_n = bg_n,
    seed = seed,
    positive = positive,
    parallel = parallel,
    ...
  )
}

#' @rdname explain_shap
#' @export
explain_shap.list <- function(x, data = NULL, raw_data = NULL,
                              feature_names = NULL, n_explain = 100L,
                              bg_n = 100L, seed = 123L, positive = NULL,
                              parallel = FALSE, task_type = "binary", ...) {
  .assert_choice(task_type, "task_type", "binary")
  .explain_shap_uniml_binary(
    x,
    data = data,
    raw_data = raw_data,
    feature_names = feature_names,
    n_explain = n_explain,
    bg_n = bg_n,
    seed = seed,
    positive = positive,
    parallel = parallel,
    ...
  )
}

#' @rdname explain_shap
#' @export
explain_shap.uniml_result <- function(x, data = NULL, raw_data = NULL,
                                      feature_names = NULL, n_explain = 100L,
                                      bg_n = 100L, seed = 123L,
                                      positive = NULL,
                                      parallel = FALSE,
                                      task_type = "binary",
                                      ...) {
  validate_uniml_result(x)
  if (is.null(x$models) || length(x$models) == 0L) {
    .abort("`x$models` must contain one or more trained mlr3 learners.")
  }
  out <- explain_shap.Learner(
    x,
    data = data,
    raw_data = raw_data,
    feature_names = feature_names,
    n_explain = n_explain,
    bg_n = bg_n,
    seed = seed,
    positive = positive,
    parallel = parallel,
    task_type = task_type,
    ...
  )
  x$shap <- out
  x
}

#' Print a SHAP result
#'
#' @param x A `uniml_shap` object.
#' @param ... Additional arguments passed to methods.
#'
#' @return Invisibly returns `x`.
#' @export
print.uniml_shap <- function(x, ...) {
  if (!inherits(x, "uniml_shap")) {
    .abort("`x` must be a `uniml_shap` object.")
  }
  cat("<uniml_shap>\n")
  cat(sprintf(
    "models: %s\n",
    if (length(x$models)) paste(x$models, collapse = ", ") else "none"
  ))
  if (!is.null(x$importance)) {
    cat(sprintf("importance rows: %d\n", nrow(x$importance)))
  }
  if (length(x$errors)) {
    cat(sprintf("skipped models: %d\n", length(x$errors)))
  }
  invisible(x)
}
