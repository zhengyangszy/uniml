# L6 result assembly, ranking, and export ------------------------------------

.empty_result_table <- function(columns) {
  out <- as.data.frame(
    stats::setNames(replicate(length(columns), character(), simplify = FALSE), columns),
    stringsAsFactors = FALSE
  )
  row.names(out) <- NULL
  out
}

.as_result_table <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.data.frame(x)) {
    out <- as.data.frame(x, stringsAsFactors = FALSE)
  } else if (is.matrix(x)) {
    out <- as.data.frame(x, stringsAsFactors = FALSE)
  } else if (is.atomic(x)) {
    out <- data.frame(value = x, stringsAsFactors = FALSE)
  } else if (is.list(x)) {
    out <- tryCatch(
      as.data.frame(x, stringsAsFactors = FALSE),
      error = function(e) NULL
    )
  } else {
    out <- NULL
  }
  if (!is.null(out)) {
    row.names(out) <- NULL
  }
  out
}

.metric_direction <- function(metric) {
  if (metric %in% c("brier", "ece", "mce", "nlr")) {
    "lower"
  } else {
    "higher"
  }
}

# Composite ranking metrics are declared by the task type (L1), so a metric set
# is never assumed to be the binary one. A task type that declares none is ranked
# on an explicit metric only; `rank_models()` reports that instead of guessing.
.composite_metric_names <- function(task_type = "binary") {
  .assert_scalar_character(task_type, "task_type")
  get_task_type(task_type)$composite_metrics
}

.comparison_set <- function(metrics, select_on = "auto") {
  .assert_choice(select_on, "select_on", c("auto", "validation", "test"))
  if (is.null(metrics) || nrow(metrics) == 0L || !"set" %in% names(metrics)) {
    return(NULL)
  }
  available <- unique(as.character(metrics$set))
  if (identical(select_on, "auto")) {
    # Selection uses the internal validation set; test and external sets are
    # reserved for reporting.
    if ("validation" %in% available) {
      return("validation")
    }
    if ("test" %in% available) {
      return("test")
    }
    return(NULL)
  }
  if (!select_on %in% available) {
    .abort(sprintf("The comparison set \"%s\" is not present in the metrics.", select_on))
  }
  select_on
}

.metrics_for_ranking <- function(metrics, select_on = "auto") {
  table <- .as_result_table(metrics)
  if (is.null(table)) {
    return(.empty_result_table(c("model", "set", "probability")))
  }
  table <- .as_result_table(table)
  if (!"model" %in% names(table)) {
    .abort("Metric results must contain a `model` column.")
  }
  if ("set" %in% names(table)) {
    chosen <- .comparison_set(table, select_on = select_on)
    if (is.null(chosen)) {
      return(table[0, , drop = FALSE])
    }
    table <- table[as.character(table$set) == chosen, , drop = FALSE]
  } else {
    table$set <- "comparison"
  }
  if ("probability" %in% names(table)) {
    table$probability <- tolower(as.character(table$probability))
    priority <- ifelse(
      table$probability == "calibrated",
      1L,
      ifelse(table$probability == "raw", 2L, 3L)
    )
  } else if ("calibrated" %in% names(table) &&
             any(!is.na(table$calibrated))) {
    calibrated <- !is.na(table$calibrated) & as.logical(table$calibrated)
    table$probability <- ifelse(calibrated, "calibrated", "raw")
    priority <- ifelse(calibrated, 1L, 2L)
  } else {
    table$probability <- NA_character_
    priority <- rep(1L, nrow(table))
  }
  ordering <- order(as.character(table$model), priority, na.last = TRUE)
  table <- table[ordering, , drop = FALSE]
  table <- table[
    !duplicated(as.character(table$model)),
    ,
    drop = FALSE
  ]
  row.names(table) <- NULL
  table
}

.ranking_score <- function(metrics, metric = "composite", task_type = "binary") {
  .assert_scalar_character(metric, "best_model_metric")
  if (identical(metric, "composite")) {
    candidates <- intersect(.composite_metric_names(task_type), names(metrics))
    if (length(candidates) == 0L) {
      .abort(sprintf(
        paste(
          "Task type \"%s\" declares no composite metric that is present in these",
          "metrics; set `best_model_metric` to an explicit metric instead."
        ),
        task_type
      ))
    }
    values <- matrix(
      as.numeric(as.matrix(metrics[, candidates, drop = FALSE])),
      nrow = nrow(metrics),
      ncol = length(candidates),
      dimnames = list(NULL, candidates)
    )
    score <- rowMeans(values, na.rm = TRUE)
    score[is.nan(score)] <- NA_real_
    return(score)
  }
  if (!metric %in% names(metrics)) {
    .abort(sprintf("`best_model_metric` is not available: %s.", metric))
  }
  as.numeric(metrics[[metric]])
}

.data_info_task_type <- function(data_info) {
  if (is.null(data_info) || !is.list(data_info)) {
    return(NULL)
  }
  type <- data_info$task_type
  if (is.null(type) && !is.null(data_info$spec)) {
    type <- data_info$spec$task_type
  }
  if (is.null(type) || !is.character(type) || length(type) != 1L || !nzchar(type)) {
    return(NULL)
  }
  type
}

# The task type decides which metrics are averaged into the composite score. A
# `uniml_result` carries it in `data_info`; a bare metric table falls back to
# "binary", which is what `rank_models()` did before multiclass existed.
.ranking_task_type <- function(x, task_type) {
  if (!identical(task_type, "auto")) {
    return(task_type)
  }
  if (inherits(x, "uniml_result")) {
    type <- .data_info_task_type(x$data_info)
    if (!is.null(type)) {
      return(type)
    }
  }
  "binary"
}

#' Rank models on a comparison set
#'
#' Ranks one row per model using either a single metric or the default
#' composite score. The composite score is the mean of AUC, sensitivity,
#' specificity, F2, balanced accuracy, and Youden's index when those metrics
#' are available.
#'
#' @param x An `uniml_result` object or a metric data frame.
#' @param select_on Comparison set: `"auto"`, `"validation"`, or `"test"`.
#'   `"auto"` uses the validation set when present and otherwise the test set, so
#'   the test set stays reporting-only whenever a validation set exists.
#' @param best_model_metric Metric used for ranking, or `"composite"`.
#' @param task_type Task type whose composite metric set is used, or `"auto"`.
#'   `"auto"` reads the task type from a `uniml_result` and otherwise assumes
#'   `"binary"`, which keeps the historical behaviour for plain metric tables.
#'
#' @return A data frame with model scores, ranks, metric, and comparison set.
#' @export
#' @examples
#' metrics <- data.frame(
#'   model = c("lr", "rf"), set = "validation",
#'   auc = c(0.80, 0.84), sensitivity = c(0.72, 0.76),
#'   specificity = c(0.70, 0.73), f2 = c(0.70, 0.75),
#'   balanced_accuracy = c(0.71, 0.75), youden = c(0.42, 0.49)
#' )
#' rank_models(metrics)
rank_models <- function(x, select_on = "auto", best_model_metric = "composite",
                        task_type = "auto") {
  if (inherits(x, "uniml_result")) {
    validate_uniml_result(x)
    metrics <- x$metrics
  } else {
    metrics <- x
  }
  task_type <- .ranking_task_type(x, task_type)
  table <- .metrics_for_ranking(metrics, select_on = select_on)
  if (nrow(table) == 0L) {
    return(data.frame(
      model = character(),
      probability = character(),
      score = numeric(),
      rank = integer(),
      metric = character(),
      set = character(),
      stringsAsFactors = FALSE
    ))
  }
  score <- tryCatch(
    .ranking_score(table, metric = best_model_metric, task_type = task_type),
    error = function(e) e
  )
  if (inherits(score, "error")) {
    message <- conditionMessage(score)
    if (grepl("not available", message, fixed = TRUE)) {
      return(data.frame(
        model = as.character(table$model),
        probability = as.character(table$probability),
        score = NA_real_,
        rank = NA_integer_,
        metric = best_model_metric,
        set = as.character(table$set),
        stringsAsFactors = FALSE
      ))
    }
    stop(score)
  }
  direction <- .metric_direction(best_model_metric)
  ordering <- if (identical(direction, "lower")) {
    order(score, table$model, na.last = TRUE)
  } else {
    order(-score, table$model, na.last = TRUE)
  }
  rank <- rep(NA_integer_, length(score))
  rank[ordering[!is.na(score[ordering])]] <- seq_len(sum(!is.na(score)))
  data.frame(
    model = as.character(table$model),
    probability = as.character(table$probability),
    score = as.numeric(score),
    rank = rank,
    metric = best_model_metric,
    set = as.character(table$set),
    stringsAsFactors = FALSE,
    row.names = NULL
  )[order(rank, table$model, na.last = TRUE), , drop = FALSE]
}

.assemble_data_info <- function(data_info = NULL) {
  if (is.null(data_info)) {
    return(NULL)
  }
  if (inherits(data_info, "uniml_data")) {
    return(list(
      n = data_info$meta$n,
      n_features = data_info$meta$n_features,
      outcome = data_info$outcome_name,
      positive = data_info$positive,
      role = data_info$role,
      types = data_info$types,
      warnings = data_info$warnings
    ))
  }
  data_info
}

.assemble_ranking <- function(metrics, ranking, select_on, best_model_metric,
                              task_type = "binary") {
  if (!is.null(ranking)) {
    return(ranking)
  }
  if (is.null(metrics)) {
    return(NULL)
  }
  tryCatch(
    rank_models(
      metrics,
      select_on = select_on,
      best_model_metric = best_model_metric,
      task_type = task_type
    ),
    error = function(e) NULL
  )
}

#' Assemble a complete uniml result
#'
#' Combines the outputs of the modelling stages into a `uniml_result`.
#' Missing stages are preserved as `NULL`; ranking is attempted whenever
#' metric results are available.
#'
#' @param data_info Optional data information, usually a `uniml_data` object.
#' @param preprocess Optional fitted preprocessor.
#' @param selection Optional feature-selection result.
#' @param tuning Optional tuning result or list of tuning results.
#' @param cv Optional cross-validation result.
#' @param metrics Optional metric data frame.
#' @param ci Optional confidence-interval table.
#' @param calibration Optional calibration result or list.
#' @param tests Optional post-model test results.
#' @param dca Optional decision-curve table.
#' @param ranking Optional precomputed ranking.
#' @param shap Optional SHAP results.
#' @param thresholds Optional threshold tables.
#' @param plots Optional plot object list.
#' @param models Optional final model list.
#' @param meta Optional metadata list.
#' @param select_on Comparison set used for ranking.
#' @param best_model_metric Metric used for ranking.
#'
#' @return A `uniml_result`.
#' @examples
#' result <- assemble_result(
#'   data_info = list(n_train = 100L, outcome = "y"),
#'   metrics = data.frame(
#'     model = c("lr", "cart"),
#'     set = "validation",
#'     probability = "raw",
#'     auc = c(0.82, 0.78)
#'   )
#' )
#' result
#' @export
assemble_result <- function(data_info = NULL, preprocess = NULL, selection = NULL,
                            tuning = NULL, cv = NULL, metrics = NULL, ci = NULL,
                            calibration = NULL, tests = NULL, dca = NULL,
                            ranking = NULL, shap = NULL, thresholds = NULL,
                            plots = NULL, models = NULL, meta = list(),
                            select_on = "auto",
                            best_model_metric = "composite") {
  if (!is.list(meta)) {
    .abort("`meta` must be a list.")
  }
  .assert_choice(select_on, "select_on", c("auto", "validation", "test"))
  .assert_scalar_character(best_model_metric, "best_model_metric")
  if (is.null(meta$created)) {
    meta$created <- as.character(Sys.time())
  }
  if (is.null(meta$session_info)) {
    meta$session_info <- utils::sessionInfo()
  }
  ranking_task_type <- .data_info_task_type(data_info)
  if (is.null(ranking_task_type)) {
    ranking_task_type <- "binary"
  }
  ranking <- .assemble_ranking(
    metrics = .as_result_table(metrics),
    ranking = .as_result_table(ranking),
    select_on = select_on,
    best_model_metric = best_model_metric,
    task_type = ranking_task_type
  )
  new_uniml_result(
    data_info = .assemble_data_info(data_info),
    preprocess = preprocess,
    selection = selection,
    tuning = tuning,
    cv = cv,
    metrics = .as_result_table(metrics),
    ci = .as_result_table(ci),
    calibration = calibration,
    tests = tests,
    dca = .as_result_table(dca),
    ranking = .as_result_table(ranking),
    shap = shap,
    thresholds = thresholds,
    plots = plots,
    models = models,
    meta = meta
  )
}

.write_table_utf8 <- function(x, path) {
  table <- .as_result_table(x)
  if (is.null(table)) {
    table <- .empty_result_table(character())
  }
  utils::write.csv(
    table,
    file = path,
    row.names = FALSE,
    fileEncoding = "UTF-8",
    na = ""
  )
  invisible(path)
}

.ensure_export_dir <- function(dir) {
  .assert_scalar_character(dir, "dir")
  if (!dir.exists(dir) && !dir.create(dir, recursive = TRUE, showWarnings = FALSE)) {
    .abort(sprintf("Could not create output directory: %s.", dir))
  }
  invisible(normalizePath(dir, winslash = "/", mustWork = TRUE))
}

.export_stage <- function(root, stage, files) {
  stage_dir <- file.path(root, stage)
  .ensure_export_dir(stage_dir)
  paths <- character()
  for (name in names(files)) {
    value <- files[[name]]
    if (is.null(value)) {
      next
    }
    paths <- c(paths, .write_table_utf8(value, file.path(stage_dir, name)))
  }
  paths
}

.selection_export <- function(selection) {
  if (is.null(selection) || !is.list(selection)) {
    return(list())
  }
  selected <- selection$selected_features
  if (!is.null(selected)) {
    selected <- data.frame(
      feature = as.character(selected),
      stringsAsFactors = FALSE
    )
  }
  details <- selection$details
  if (is.list(details) && !is.data.frame(details) && length(details) > 0L) {
    details <- tryCatch(
      do.call(rbind, lapply(names(details), function(name) {
        value <- details[[name]]
        table <- .as_result_table(value)
        if (is.null(table)) {
          return(NULL)
        }
        if (!"method" %in% names(table)) {
          table$method <- name
        }
        table
      })),
      error = function(e) NULL
    )
  }
  list(
    "selected_features.csv" = selected,
    "by_method_detail.csv" = details
  )
}

.preprocess_export <- function(preprocess) {
  if (is.null(preprocess)) {
    return(list())
  }
  if (!is.list(preprocess)) {
    return(list("var_info.csv" = preprocess))
  }
  steps <- preprocess$steps
  scale_params <- NULL
  impute_report <- NULL
  encoder_map <- NULL
  if (is.list(steps)) {
    if (!is.null(steps$scale$params$differences)) {
      scale_params <- do.call(rbind, lapply(
        names(steps$scale$params$differences),
        function(column) {
          item <- steps$scale$params$differences[[column]]
          data.frame(
            column = column,
            center = item$center,
            scale = item$scale,
            method = steps$scale$method,
            stringsAsFactors = FALSE
          )
        }
      ))
    }
    if (!is.null(steps$impute$params)) {
      impute_report <- .as_result_table(steps$impute$params$report)
    }
    if (!is.null(steps$encode$params$levels)) {
      encoder_map <- do.call(rbind, lapply(
        names(steps$encode$params$levels),
        function(column) {
          data.frame(
            column = column,
            level = as.character(steps$encode$params$levels[[column]]),
            method = steps$encode$method,
            stringsAsFactors = FALSE
          )
        }
      ))
    }
  }
  list(
    "var_info.csv" = preprocess$var_info,
    "scaling_params.csv" = scale_params,
    "impute_report.csv" = impute_report,
    "encoder_map.csv" = encoder_map
  )
}

.tuning_export <- function(tuning) {
  if (is.null(tuning)) {
    return(list())
  }
  if (is.data.frame(tuning)) {
    return(list("best_hyperparameters.csv" = tuning))
  }
  if (!is.list(tuning)) {
    return(list())
  }
  items <- if (!is.null(tuning$learner_id)) list(tuning) else tuning
  rows <- lapply(names(items), function(name) {
    item <- items[[name]]
    if (!is.list(item)) {
      return(NULL)
    }
    params <- item$best_params
    if (is.null(params)) {
      params <- list()
    }
    data.frame(
      model = if (!is.null(item$learner_id)) item$learner_id else name,
      status = if (is.null(item$status)) NA_character_ else item$status,
      best_metric = if (is.null(item$best_metric)) NA_real_ else item$best_metric,
      parameters = paste(
        paste(names(params), unlist(params), sep = "="),
        collapse = "; "
      ),
      stringsAsFactors = FALSE
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  list("best_hyperparameters.csv" = if (length(rows)) do.call(rbind, rows) else NULL)
}

.cv_export <- function(cv) {
  if (is.null(cv)) {
    return(list())
  }
  if (is.data.frame(cv)) {
    return(list("cv_summary.csv" = cv))
  }
  if (!is.list(cv)) {
    return(list())
  }
  list(
    "cv_summary.csv" = cv$summary,
    "cv_detailed.csv" = cv$detailed
  )
}

.calibration_export <- function(calibration) {
  if (is.null(calibration)) {
    return(list())
  }
  if (inherits(calibration, "uniml_calibration")) {
    params <- .as_result_table(calibration$parameters)
    ece_brier <- .as_result_table(calibration$metrics_after)
    return(list(
      "params.csv" = params,
      "ece_brier.csv" = ece_brier,
      "hl.csv" = NULL
    ))
  }
  if (!is.list(calibration)) {
    return(list())
  }
  if (!is.null(calibration$metrics)) {
    params <- data.frame(
      model = names(calibration$parameters),
      method = if (is.null(calibration$method)) NA_character_ else calibration$method,
      stringsAsFactors = FALSE
    )
    # A task type that has a per-time-point summary (survival reports the ICI and
    # the recalibration coefficients there) ships it next to the curve table so
    # neither is trapped inside R.
    return(list(
      "params.csv" = params,
      "ece_brier.csv" = calibration$metrics,
      "summary.csv" = calibration$summary,
      "hl.csv" = NULL
    ))
  }
  list(
    "params.csv" = calibration$params,
    "ece_brier.csv" = calibration$ece_brier,
    "hl.csv" = calibration$hl
  )
}

.calibration_metrics_table <- function(calibration) {
  if (is.null(calibration)) {
    return(NULL)
  }
  if (inherits(calibration, "uniml_calibration")) {
    rows <- list()
    if (!is.null(calibration$metrics_before)) {
      before <- .as_result_table(calibration$metrics_before)
      if (!is.null(before)) {
        before$stage <- "before"
        rows$before <- before
      }
    }
    if (!is.null(calibration$metrics_after)) {
      after <- .as_result_table(calibration$metrics_after)
      if (!is.null(after)) {
        after$stage <- "after"
        rows$after <- after
      }
    }
    if (length(rows) == 0L) {
      return(NULL)
    }
    columns <- unique(unlist(lapply(rows, names), use.names = FALSE))
    rows <- lapply(rows, function(row) {
      row[setdiff(columns, names(row))] <- NA
      row[, columns, drop = FALSE]
    })
    result <- do.call(rbind, rows)
    row.names(result) <- NULL
    return(result)
  }
  if (is.list(calibration) && !is.null(calibration$ece_brier)) {
    return(.as_result_table(calibration$ece_brier))
  }
  if (is.list(calibration) && !is.null(calibration$metrics)) {
    return(.as_result_table(calibration$metrics))
  }
  NULL
}

# One row per model with the raw and calibrated classification thresholds, so
# that exported tables are enough to redraw predictions without the RDS.
.model_thresholds_table <- function(models) {
  if (is.null(models) || length(models) == 0L) {
    return(NULL)
  }
  rows <- list()
  for (key in names(models)) {
    thresholds <- models[[key]]$thresholds
    if (!is.list(thresholds)) {
      next
    }
    rows[[length(rows) + 1L]] <- data.frame(
      model = key,
      threshold_raw = thresholds$raw,
      threshold_calibrated = thresholds$calibrated,
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0L) {
    return(NULL)
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

.test_export <- function(tests) {
  if (is.null(tests) || !is.list(tests)) {
    return(list())
  }
  delong <- tests$delong
  if (is.list(delong) && !is.data.frame(delong) && !is.null(delong$summary)) {
    delong <- delong$summary
  }
  hl <- tests$hl
  if (is.list(hl) && length(hl) > 0L) {
    if (is.data.frame(hl)) {
      if (!"probability" %in% names(hl)) {
        hl$probability <- "raw"
      }
    } else {
      probability_tracks <- intersect(names(hl), c("raw", "calibrated"))
      tracks <- if (length(probability_tracks) > 0L) {
        stats::setNames(probability_tracks, probability_tracks)
      } else {
        list(raw = hl)
      }
      rows <- lapply(names(tracks), function(probability) {
        items <- tracks[[probability]]
        if (!is.list(items) || length(items) == 0L) {
          return(NULL)
        }
        item_rows <- lapply(names(items), function(name) {
          item <- items[[name]]
          if (!is.list(item)) {
            return(NULL)
          }
          data.frame(
            model = name,
            probability = probability,
            statistic = if (is.null(item$statistic)) NA_real_ else item$statistic,
            df = if (is.null(item$df)) NA_integer_ else item$df,
            p_value = if (is.null(item$p_value)) NA_real_ else item$p_value,
            n = if (is.null(item$n)) NA_integer_ else item$n,
            n_groups = if (is.null(item$n_groups)) NA_integer_ else item$n_groups,
            method = if (is.null(item$method)) NA_character_ else item$method,
            message = if (is.null(item$message)) NA_character_ else item$message,
            stringsAsFactors = FALSE
          )
        })
        item_rows <- item_rows[!vapply(item_rows, is.null, logical(1))]
        if (length(item_rows) == 0L) NULL else do.call(rbind, item_rows)
      })
      rows <- rows[!vapply(rows, is.null, logical(1))]
      hl <- if (length(rows) > 0L) do.call(rbind, rows) else NULL
    }
  }
  list(
    "delong.csv" = delong,
    "hl.csv" = hl,
    "nri_idi.csv" = tests$nri_idi
  )
}

.shap_export <- function(shap) {
  if (is.null(shap) || !is.list(shap)) {
    return(list())
  }
  list(
    "importance.csv" = shap$importance,
    "values_matrix_std.csv" = shap$values_matrix_std,
    "values_matrix_raw.csv" = shap$values_matrix_raw,
    "feature_values_std.csv" = shap$feature_values_std,
    "feature_values_raw.csv" = shap$feature_values_raw
  )
}

.model_objects <- function(models) {
  if (is.null(models)) {
    return(list())
  }
  if (inherits(models, "Learner")) {
    return(list(model = models))
  }
  if (!is.list(models)) {
    return(list())
  }
  out <- list()
  for (name in names(models)) {
    item <- models[[name]]
    learner <- if (inherits(item, "Learner")) {
      item
    } else if (is.list(item) && inherits(item$learner, "Learner")) {
      item$learner
    } else {
      NULL
    }
    if (!is.null(learner)) {
      out[[name]] <- learner
    }
  }
  out
}

.export_plots <- function(root, plots, plot_args = list()) {
  if (is.null(plots) || !is.list(plots) || length(plots) == 0L) {
    return(character())
  }
  plot_dir <- file.path(root, "plots")
  .ensure_export_dir(plot_dir)
  settings <- utils::modifyList(
    list(width = 7, height = 5, dpi = 300, units = "in", device = "jpeg"),
    plot_args
  )
  paths <- character()
  for (name in names(plots)) {
    plot <- plots[[name]]
    if (is.null(plot) || !inherits(plot, c("ggplot", "patchwork"))) {
      next
    }
    extension <- if (identical(settings$device, "png")) "png" else "jpg"
    path <- file.path(plot_dir, paste0(name, ".", extension))
    result <- tryCatch(
      {
        do.call(
          ggplot2::ggsave,
          c(list(filename = path, plot = plot), settings)
        )
        path
      },
      error = function(e) NULL
    )
    if (!is.null(result)) {
      paths <- c(paths, result)
    }
  }
  paths
}

#' Save and read a fitted result
#'
#' `save_result()` writes the whole `uniml_result` — the fitted learners, the
#' fitted preprocessor, the selected features, and the specification — to a single
#' file. That file is what someone else needs to score new patients and to compute
#' their own analyses on it, including the subgroup metrics this package
#' deliberately does not report.
#'
#' The saved object is a plain R object: `readRDS()` reads it, and
#' [predict_uniml()] scores new data with it. `save_result()` exists so the file
#' has a version stamp and is validated on the way out and back in, rather than
#' so it can do something `saveRDS()` cannot.
#'
#' @param result A `uniml_result` object.
#' @param path File to write or read. The `.rds` extension is added when missing.
#'
#' @return `save_result()` returns the normalised path; `read_result()` returns
#'   the `uniml_result`.
#' @references
#' R Core Team (2024). *R: A Language and Environment for Statistical Computing*.
#' Section "Serialization".
#' @export
#' @examples
#' \dontrun{
#' result <- run_workflow(spec, train, validation)
#' path <- save_result(result, tempfile(fileext = ".rds"))
#' restored <- read_result(path)
#'
#' # Subgroup analysis is yours to define: score the subgroup, then use the same
#' # metric function the workflow uses.
#' subgroup <- validation[validation$age_group == "older", ]
#' scored <- predict_uniml(restored, subgroup)
#' calc_metrics(scored, subgroup$y, positive = "yes")
#' }
save_result <- function(result, path) {
  validate_uniml_result(result)
  .assert_scalar_character(path, "path")
  if (!grepl("\\.rds$", path, ignore.case = TRUE)) {
    path <- paste0(path, ".rds")
  }
  directory <- dirname(path)
  if (!dir.exists(directory) &&
      !dir.create(directory, recursive = TRUE, showWarnings = FALSE)) {
    .abort(sprintf("Could not create the directory: %s.", directory))
  }
  processed <- result
  attr(processed, "uniml_version") <- as.character(
    utils::packageVersion("uniml")
  )
  saveRDS(processed, path, version = 3)
  invisible(normalizePath(path, winslash = "/", mustWork = TRUE))
}

#' @rdname save_result
#' @export
read_result <- function(path) {
  .assert_scalar_character(path, "path")
  if (!file.exists(path)) {
    .abort(sprintf("No such file: %s.", path))
  }
  result <- tryCatch(
    readRDS(path),
    error = function(e) {
      .abort(sprintf("Could not read %s: %s", path, conditionMessage(e)))
    }
  )
  if (!inherits(result, "uniml_result")) {
    .abort(sprintf("%s does not contain a `uniml_result`.", path))
  }
  validate_uniml_result(result)
  version <- attr(result, "uniml_version")
  current <- as.character(utils::packageVersion("uniml"))
  if (!is.null(version) && !identical(version, current)) {
    warning(
      sprintf(
        "The result was written by uniml %s; this is %s. Re-fit if the layout changed.",
        version, current
      ),
      call. = FALSE
    )
  }
  result
}

#' Export a uniml result
#'
#' Writes the complete result and all available stage tables using the
#' directory layout defined by the package specification. Missing stages are
#' skipped rather than treated as errors.
#'
#' @param result A `uniml_result` object.
#' @param dir Output directory.
#' @param include_plots Whether to save available ggplot objects as JPEG files.
#'
#' @param include_plots Whether to save available ggplot objects as image files.
#' @param plot_args Optional list of `ggplot2::ggsave()` arguments (`width`,
#'   `height`, `dpi`, `units`, `device`, ...).
#'
#' @return Invisibly returns a list with the output directory, written files,
#'   and skipped stages.
#' @examples
#' \dontrun{
#' result <- run_workflow(spec, train, validation)
#' exported <- export_results(result, dir = tempfile("uniml-"))
#' basename(exported$files)
#' }
#' @export
export_results <- function(result, dir = "output", include_plots = TRUE,
                           plot_args = list()) {
  validate_uniml_result(result)
  .assert_scalar_logical(include_plots, "include_plots")
  .assert_named_list(plot_args, "plot_args")
  root <- .ensure_export_dir(dir)
  paths <- character()
  skipped <- character()
  paths <- c(paths, .write_table_utf8(
    data.frame(
      component = names(result),
      present = !vapply(result, is.null, logical(1)),
      stringsAsFactors = FALSE
    ),
    file.path(root, "export_manifest.csv")
  ))
  paths <- c(paths, .write_rds_utf8(result, file.path(root, "result.rds")))

  stages <- list(
    preprocess = .preprocess_export(result$preprocess),
    selection = .selection_export(result$selection),
    tuning = .tuning_export(result$tuning),
    cv = .cv_export(result$cv),
    performance = list(
      "metrics_train.csv" = if (!is.null(result$metrics) && "set" %in% names(result$metrics)) {
        result$metrics[result$metrics$set == "train", , drop = FALSE]
      } else {
        NULL
      },
      "metrics_validation.csv" = if (!is.null(result$metrics) && "set" %in% names(result$metrics)) {
        result$metrics[result$metrics$set == "validation", , drop = FALSE]
      } else {
        NULL
      },
      "metrics_test.csv" = if (!is.null(result$metrics) && "set" %in% names(result$metrics)) {
        result$metrics[result$metrics$set == "test", , drop = FALSE]
      } else {
        NULL
      },
      "metrics_external.csv" = if (!is.null(result$metrics) && "set" %in% names(result$metrics)) {
        external <- result$metrics[result$metrics$set == "external", , drop = FALSE]
        if (nrow(external) > 0L) external else NULL
      } else {
        NULL
      },
      "metrics_ci.csv" = result$ci,
      "before_after_calibration.csv" = .calibration_metrics_table(result$calibration),
      "thresholds.csv" = .model_thresholds_table(result$models)
    ),
    predictions = list("predictions.csv" = result$meta$predictions),
    ranking = list(
      "leaderboard.csv" = result$ranking,
      "overall_rank.csv" = result$ranking
    ),
    calibration = .calibration_export(result$calibration),
    tests = .test_export(result$tests),
    dca = list("dca_curve.csv" = result$dca),
    thresholds = list(
      "zero_crossings.csv" = result$thresholds$zero_crossings,
      "paper_ready_thresholds.csv" = result$thresholds$paper_ready
    ),
    meta = list("failures.csv" = result$meta$failures),
    reporting = list("tripod_ai.csv" = as.data.frame(tripod_ai_check(result))),
    shap = .shap_export(result$shap)
  )
  for (stage in names(stages)) {
    files <- stages[[stage]]
    if (length(files) == 0L) {
      skipped <- c(skipped, stage)
      next
    }
    paths <- c(paths, .export_stage(root, stage, files))
  }

  model_dir <- file.path(root, "models")
  .ensure_export_dir(model_dir)
  model_paths <- .model_objects(result$models)
  if (length(model_paths) == 0L) {
    skipped <- c(skipped, "models")
  } else {
    for (name in names(model_paths)) {
      path <- file.path(model_dir, paste0("final_", name, ".rds"))
      paths <- c(paths, .write_rds_utf8(model_paths[[name]], path))
    }
  }
  if (isTRUE(include_plots)) {
    plot_paths <- .export_plots(root, result$plots, plot_args = plot_args)
    paths <- c(paths, plot_paths)
    if (length(plot_paths) == 0L) {
      skipped <- c(skipped, "plots")
    }
  }
  invisible(list(
    dir = root,
    files = paths,
    skipped = unique(skipped)
  ))
}

.write_rds_utf8 <- function(object, path) {
  saveRDS(object, file = path)
  path
}
