test_that("run_workflow() supports all leakage policies and ranks models", {
  skip_if_not_installed("rpart")
  data <- make_workflow_data()
  split <- make_workflow_split(data)

  expectations <- list(
    strict_in = c(
      "imputation", "scaling", "encoding", "selection", "balance", "tuning"
    ),
    strict_out = character(),
    custom = c("selection", "balance", "tuning")
  )

  for (mode in names(expectations)) {
    spec <- make_workflow_spec(mode)
    result <- run_workflow(spec, split$train, split$validation)

    expect_s3_class(result, "uniml_result")
    expect_identical(result$meta$leakage_policy, mode)
    expect_setequal(result$cv$in_cv, expectations[[mode]])
    expect_true(is.data.frame(result$cv$detailed))
    expect_true(nrow(result$ranking) >= 1L)
    expect_identical(result$ranking$model[result$ranking$rank == 1L], "cart")
    expect_s3_class(rank_models(result$metrics), "data.frame")
    expect_true("cart" %in% names(result$models))
  }
})

test_that("run_workflow() respects select_on and the prediction table contract", {
  skip_if_not_installed("rpart")
  data <- make_workflow_data()
  split <- make_workflow_split(data)
  spec <- make_workflow_spec("strict_out")

  result <- run_workflow(spec, split$train, split$validation, split$test)
  predictions <- result$meta$predictions

  expect_identical(
    names(predictions),
    c("model", "prob", "calibrated_prob", "truth", "positive", "set")
  )
  expect_setequal(unique(predictions$set), c("train", "validation", "test"))
  # `auto` keeps the test set for reporting only.
  expect_identical(result$ranking$set, rep("validation", nrow(result$ranking)))
  expect_true(all(result$metrics$set %in% c("train", "validation", "test")))
  expect_identical(
    rank_models(result$metrics)$set,
    rep("validation", nrow(result$ranking))
  )

  without_test <- run_workflow(spec, split$train, split$validation)
  expect_identical(
    without_test$ranking$set,
    rep("validation", nrow(without_test$ranking))
  )
})

test_that("using the test set for selection is explicit and warned about", {
  skip_if_not_installed("rpart")
  data <- make_workflow_data()
  split <- make_workflow_split(data)
  spec <- make_workflow_spec("strict_out")
  spec <- set_evaluation(spec, select_on = "test")

  expect_warning(
    result <- run_workflow(spec, split$train, split$validation, split$test),
    "test set is used for model selection"
  )
  expect_identical(result$ranking$set, rep("test", nrow(result$ranking)))
})

test_that("run_workflow() never fits preprocessing or selection on validation/test", {
  skip_if_not_installed("rpart")
  data <- make_workflow_data()
  split <- make_workflow_split(data)
  spec <- make_workflow_spec("strict_out")

  result <- run_workflow(spec, split$train, split$validation, split$test)
  reference_preprocessor <- fit_preprocessor(
    split$train,
    impute = spec$preprocess$impute,
    scale = spec$preprocess$scale,
    encode = spec$preprocess$encode,
    balance = spec$preprocess$balance,
    outcome = spec$outcome,
    ordered_as = spec$ordered_as,
    seed = spec$seed
  )

  expect_equal(
    result$preprocess$steps$scale$params,
    reference_preprocessor$steps$scale$params
  )
  expect_identical(
    result$selection$selected_features,
    select_features(reference_preprocessor$train, spec)$selected_features
  )

  altered_validation <- split$validation
  altered_test <- split$test
  altered_validation$x1 <- altered_validation$x1 + 100
  altered_test$x1 <- altered_test$x1 + 100
  altered <- run_workflow(spec, split$train, altered_validation, altered_test)

  expect_equal(
    result$preprocess$steps$scale$params,
    altered$preprocess$steps$scale$params
  )
  expect_identical(
    result$selection$selected_features,
    altered$selection$selected_features
  )
})

test_that("run_workflow() is reproducible for probabilities and metrics", {
  skip_if_not_installed("rpart")
  data <- make_workflow_data()
  split <- make_workflow_split(data)
  spec <- make_workflow_spec("strict_out")

  first <- run_workflow(spec, split$train, split$validation, split$test)
  second <- run_workflow(spec, split$train, split$validation, split$test)

  expect_equal(
    first$meta$predictions,
    second$meta$predictions,
    tolerance = 1e-12
  )
  expect_equal(first$metrics, second$metrics, tolerance = 1e-12)
  expect_equal(first$cv$detailed, second$cv$detailed, tolerance = 1e-12)
})

test_that("run_workflow() assembles calibration, tests, and decision curves", {
  skip_if_not_installed("rpart")
  data <- make_workflow_data()
  split <- make_workflow_split(data)
  spec <- make_workflow_spec("strict_out")
  spec <- set_evaluation(
    spec,
    ci = "none",
    calibration = "platt",
    tests = c("delong", "hl"),
    dca = TRUE,
    select_on = "auto"
  )

  result <- run_workflow(spec, split$train, split$validation)

  expect_false(is.null(result$calibration))
  expect_true(is.data.frame(result$calibration$metrics))
  expect_setequal(unique(result$calibration$metrics$stage), c("before", "after"))
  expect_false(is.null(result$tests$delong))
  expect_false(is.null(result$tests$hl))
  expect_false(is.null(result$dca))
  expect_setequal(unique(result$dca$calibrated), c(FALSE, TRUE))
  model_rows <- setdiff(unique(result$dca$model), c("Treat All", "Treat None"))
  expect_true(length(model_rows) > 0L)
  expect_true(all(c("Treat All", "Treat None") %in% unique(result$dca$model)))
})

test_that("single-class validation and test sets return defined metrics only", {
  skip_if_not_installed("rpart")
  data <- make_workflow_data()
  split <- make_workflow_split(data)
  spec <- make_workflow_spec("strict_out")
  single_validation <- split$validation[split$validation$y == "yes", , drop = FALSE]
  single_test <- split$test[split$test$y == "yes", , drop = FALSE]

  result <- run_workflow(spec, split$train, single_validation, single_test)
  selected <- result$metrics[result$metrics$set == "validation", , drop = FALSE]

  expect_true(all(is.na(selected$auc)))
  expect_true(all(is.na(selected$pr_auc)))
  expect_true(all(is.na(selected$specificity)))
  expect_true(all(is.finite(selected$accuracy)))
  expect_true(all(is.finite(selected$sensitivity)))
  expect_true(is.finite(selected$brier))
  expect_true(nrow(result$ranking) >= 1L)
})

test_that("run_workflow() pools mice imputations and keeps compatibility fields", {
  skip_if_not_installed("rpart")
  skip_if_not_installed("mice")
  data <- make_workflow_mice_data()
  split <- make_workflow_split(data)
  spec <- make_workflow_mice_spec()

  result <- run_workflow(spec, split$train, split$validation, split$test)
  entry <- result$models$cart

  expect_length(entry$learners, 2L)
  expect_length(entry$preprocessors, 2L)
  expect_length(entry$selections, 2L)
  expect_identical(entry$imputations, c(1L, 2L))
  expect_identical(entry$learner, entry$learners[[1L]])
  expect_true(is.list(entry$best_params))
  expect_true(all(vapply(entry$learners, inherits, logical(1), "Learner")))
  expect_true(all(result$meta$predictions$prob >= 0))
  expect_true(all(result$meta$predictions$prob <= 1))

  # Cross-validation runs per imputation and is pooled with Rubin's rules.
  expect_setequal(unique(result$cv$detailed$imputation), c(1L, 2L))
  expect_gt(nrow(result$cv$summary), 0L)
  expect_true(all(result$cv$summary$n == 2L))
  expect_true("se" %in% names(result$cv$summary))
  expect_true("df" %in% names(result$cv$summary))
})

test_that(".workflow_pool_ci() handles all-missing bootstrap intervals", {
  intervals <- lapply(seq_len(2L), function(index) {
    data.frame(
      metric = "auc",
      mean = NA_real_,
      lower = NA_real_,
      upper = NA_real_,
      n_effective = NA_integer_,
      stringsAsFactors = FALSE
    )
  })

  pooled <- .workflow_pool_ci(intervals, "cart", "validation")

  expect_identical(nrow(pooled), 1L)
  expect_true(is.na(pooled$mean))
  expect_true(is.na(pooled$n_effective))
  expect_identical(pooled$formatted, "NA")
})

test_that("SHAP model and plot selection follow explain settings", {
  skip_if_not_installed("rpart")
  skip_if_not_installed("shapviz")
  data <- make_workflow_data()
  data$cat <- factor(data$cat)
  spec <- make_workflow_spec("strict_out")
  task <- make_task(data, spec)
  learner <- mlr3::lrn("classif.rpart", predict_type = "prob")
  learner$train(task)
  result <- assemble_result(
    models = list(
      cart = list(learner = learner),
      rf = list(learner = learner)
    )
  )
  spec <- set_explain(spec, model = "all", plots = c("importance", "beeswarm"))

  selected <- .workflow_shap_learners(result, spec)
  expect_identical(names(selected), c("cart", "rf"))

  values <- matrix(
    c(0.4, -0.2, 0.3, -0.5, 0.6, 0.1, 0.2, 0.5),
    nrow = 4L,
    dimnames = list(NULL, c("x1", "x2"))
  )
  features <- data.frame(
    x1 = c(1, 2, 3, 4),
    x2 = c(0, 1, 0, 1)
  )
  result$shap <- list(objects = shapviz::shapviz(values, features))

  plots <- .workflow_plots(result, spec)
  expect_identical(names(plots), c("importance", "beeswarm"))

  single <- set_explain(spec, model = "all", plots = "importance")
  expect_identical(names(.workflow_plots(result, single)), "importance")
})

test_that("workflow skips SHAP when explain is disabled", {
  skip_if_not_installed("rpart")
  data <- make_workflow_data()
  split <- make_workflow_split(data)
  spec <- make_workflow_spec("strict_out")
  spec <- set_explain(spec, model = "none", plots = "importance")

  result <- run_workflow(spec, split$train, split$validation)

  expect_null(result$shap)
  expect_null(result$plots)
})

test_that("workflow degrades SHAP failures to a warning", {
  skip_if_not_installed("rpart")
  data <- make_workflow_data()
  data$cat <- factor(data$cat)
  spec <- make_workflow_spec("strict_out")
  task <- make_task(data, spec)
  learner <- mlr3::lrn("classif.rpart", predict_type = "prob")
  learner$train(task)
  result <- assemble_result(models = list(cart = list(learner = learner)))
  spec <- set_explain(spec, model = "best", plots = "importance")

  expect_warning(
    shap <- .workflow_shap(result, spec),
    "SHAP computation failed"
  )
  expect_null(shap)
})

test_that("cross validation yields fold rows and honours repeats", {
  skip_if_not_installed("rpart")
  data <- make_workflow_data()
  split <- make_workflow_split(data)
  spec <- make_workflow_spec("strict_out")
  spec <- set_tuning(spec, folds = 2L, repeats = 2L, evals = 1L)

  result <- run_workflow(spec, split$train, split$validation)

  expect_gt(nrow(result$cv$detailed), 0L)
  expect_setequal(unique(result$cv$detailed$repetition), c(1L, 2L))
  expect_setequal(unique(result$cv$detailed$fold), c(1L, 2L))
  expect_equal(nrow(result$cv$detailed), 4L)
  expect_true("n" %in% names(result$cv$summary))
  expect_true(all(result$cv$summary$n == 2L))
  expect_identical(result$cv$in_cv, character())

  expect_true("failures" %in% names(result$meta))
  expect_true(is.data.frame(result$meta$failures))
  expect_equal(nrow(result$meta$failures), 0L)
})

test_that("calibration is fitted out of fold and keeps calibrated probabilities", {
  skip_if_not_installed("rpart")
  data <- make_workflow_data()
  split <- make_workflow_split(data)
  spec <- make_workflow_spec("strict_out")
  spec <- set_evaluation(
    spec,
    ci = "none",
    calibration = "platt",
    calibration_args = list(folds = 2L, bins = 5L, fit = "out_of_fold"),
    tests = character(),
    dca = TRUE,
    select_on = "validation"
  )

  result <- run_workflow(spec, split$train, split$validation)

  expect_identical(result$meta$calibration_fit, "out_of_fold")
  expect_false(anyNA(result$meta$predictions$calibrated_prob))
  expect_true(all(result$meta$predictions$calibrated_prob >= 0 &
                    result$meta$predictions$calibrated_prob <= 1))
  expect_setequal(
    unique(result$calibration$metrics$stage),
    c("before", "after")
  )
  expect_true(any(result$dca$calibrated))
  expect_true(any(!result$dca$calibrated))
  expect_true(is.finite(result$models$cart$thresholds$raw))
  expect_true(is.finite(result$models$cart$thresholds$calibrated))
})

test_that("preprocessing step arguments from the spec reach the engine", {
  skip_if_not_installed("rpart")
  data <- make_workflow_data()
  split <- make_workflow_split(data)
  spec <- make_workflow_spec("strict_out")
  spec <- set_preprocess(spec, scale = "zscore", scale_args = list(vars = "x1"))

  result <- run_workflow(spec, split$train, split$validation)

  scaled <- names(result$preprocess$steps$scale$params$differences)
  expect_identical(scaled, "x1")
})

test_that("NRI and IDI are computed against the reference model", {
  skip_if_not_installed("rpart")
  data <- make_workflow_data()
  split <- make_workflow_split(data)
  spec <- make_workflow_spec("strict_out")
  spec <- set_learners(spec, c("cart", "classif.log_reg"), tune = FALSE)
  spec <- set_evaluation(
    spec,
    ci = "none",
    calibration = "none",
    tests = "delong",
    dca = FALSE,
    select_on = "validation",
    reference_model = "cart"
  )

  result <- run_workflow(spec, split$train, split$validation)
  table <- result$tests$nri_idi

  expect_true(is.data.frame(table))
  expect_setequal(table$model, c("cart", "classif.log_reg"))
  expect_true(all(table$reference == "cart"))
  expect_true(all(c("nri", "idi", "nri_categorical", "nri_lower", "idi_upper") %in%
                    names(table)))
  reference_row <- table[table$model == "cart", ]
  expect_equal(reference_row$nri, 0)
  expect_equal(reference_row$idi, 0)
  expect_true(is.na(reference_row$nri_lower))
  expect_identical(unique(table$set), "validation")

  expect_error(
    run_workflow(
      set_evaluation(spec, reference_model = "missing_model"),
      split$train,
      split$validation
    ),
    "Reference model"
  )
})

test_that("SHAP zero-crossing thresholds reach the result and the export", {
  skip_if_not_installed("rpart")
  skip_if_not_installed("shapviz")
  data <- make_workflow_data(n = 120L)
  split <- make_workflow_split(data)
  spec <- make_workflow_spec("strict_out")
  spec <- set_explain(
    spec,
    model = "best",
    plots = "importance",
    thresholds = list(targets = "x1", min_n = 10L, min_unique = 3L)
  )

  result <- run_workflow(spec, split$train, split$validation)

  expect_false(is.null(result$thresholds))
  expect_true(is.data.frame(result$thresholds$zero_crossings))
  expect_true(is.data.frame(result$thresholds$paper_ready))
  expect_true("x1" %in% result$thresholds$paper_ready$target)

  output <- file.path(tempdir(), "uniml-threshold-test")
  unlink(output, recursive = TRUE)
  export_results(
    result,
    dir = output,
    plot_args = list(width = 4, height = 3, dpi = 120, device = "png")
  )
  expect_true(file.exists(file.path(output, "thresholds", "zero_crossings.csv")))
  expect_true(file.exists(file.path(output, "thresholds", "paper_ready_thresholds.csv")))
  expect_true(any(grepl("plots/.*\\.png$", list.files(output, recursive = TRUE))))
})

test_that("plot helpers accept custom palettes, themes, and labels", {
  skip_if_not_installed("rpart")
  data <- make_workflow_data()
  split <- make_workflow_split(data)
  spec <- make_workflow_spec("strict_out")
  spec <- set_evaluation(
    spec,
    ci = "none",
    calibration = "none",
    tests = character(),
    dca = TRUE,
    select_on = "validation"
  )

  result <- run_workflow(spec, split$train, split$validation)
  roc <- plot_roc(
    result,
    palette = c("#123456", "#654321"),
    theme = theme_uniml(base_size = 13),
    title = "ROC title"
  )
  expect_s3_class(roc, "ggplot")
  expect_identical(roc$labels$title, "ROC title")
  expect_equal(roc$theme$text$size, 13)

  dca <- plot_dca(result, title = "DCA title")
  expect_s3_class(dca, "ggplot")
  expect_identical(dca$labels$title, "DCA title")
})

test_that("SHAP waterfall plot follows explain settings", {
  skip_if_not_installed("rpart")
  skip_if_not_installed("shapviz")
  data <- make_workflow_data()
  split <- make_workflow_split(data)
  spec <- make_workflow_spec("strict_out")
  spec <- set_explain(spec, model = "best", plots = "waterfall")

  result <- run_workflow(spec, split$train, split$validation)

  expect_true("waterfall" %in% names(result$plots))
  expect_s3_class(result$plots$waterfall, "ggplot")
  expect_equal(nrow(result$meta$failures), 0L)
})

test_that("glmnet falls back to logistic regression with a single feature", {
  skip_if_not_installed("rpart")
  skip_if_not_installed("glmnet")
  set.seed(9)
  data <- data.frame(
    y = factor(rep(c("no", "yes"), length.out = 60), levels = c("no", "yes")),
    x1 = stats::rnorm(60)
  )
  spec <- make_workflow_spec("strict_out")
  spec <- set_learners(spec, c("lr", "cart"), tune = FALSE)
  spec <- set_tuning(spec, folds = 2L, evals = 1L)

  expect_warning(
    result <- run_workflow(spec, data[1:45, ], data[46:60, ]),
    "failed or were downgraded"
  )

  expect_true("lr" %in% names(result$models))
  expect_identical(result$models$lr$learner$id, "classif.log_reg")
  expect_true(any(result$meta$failures$stage == "fallback"))
  expect_true(all(is.finite(result$metrics$auc)))
})

test_that("strict_in refits preprocessing for out-of-fold calibration", {
  skip_if_not_installed("rpart")
  data <- make_workflow_data()
  data$x1[c(2L, 9L, 17L)] <- NA_real_
  split <- make_workflow_split(data)
  spec <- make_workflow_spec("strict_in")
  spec <- set_evaluation(
    spec,
    ci = "none",
    calibration = "platt",
    calibration_args = list(folds = 2L, bins = 5L, fit = "out_of_fold"),
    tests = character(),
    dca = FALSE,
    select_on = "validation"
  )

  result <- run_workflow(spec, split$train, split$validation)

  expect_identical(result$meta$calibration_fit, "out_of_fold")
  expect_setequal(
    unique(result$calibration$metrics$stage),
    c("before", "after")
  )
  expect_false(anyNA(result$meta$predictions$calibrated_prob))
  expect_true(is.finite(result$models$cart$thresholds$raw))
})

test_that("the external cohort is optional and reporting-only", {
  skip_if_not_installed("rpart")
  data <- make_workflow_data(n = 150L)
  split <- make_workflow_split(data)
  external <- data[91:150, , drop = FALSE]
  spec <- make_workflow_spec("strict_out")

  three <- run_workflow(
    spec,
    split$train,
    split$validation,
    external = external
  )
  expect_setequal(
    unique(three$meta$predictions$set),
    c("train", "validation", "external")
  )
  expect_true("external" %in% three$metrics$set)
  expect_identical(three$ranking$set, rep("validation", nrow(three$ranking)))
  expect_identical(three$data_info$n_external, nrow(external))

  four <- run_workflow(
    spec,
    split$train,
    split$validation,
    test = split$test,
    external = external
  )
  expect_setequal(
    unique(four$meta$predictions$set),
    c("train", "validation", "test", "external")
  )
  expect_identical(four$ranking$set, rep("validation", nrow(four$ranking)))
  expect_true(all(four$metrics$set %in% c("train", "validation", "test", "external")))

  output <- file.path(tempdir(), "uniml-external-test")
  unlink(output, recursive = TRUE)
  export_results(four, dir = output, include_plots = FALSE)
  expect_true(file.exists(file.path(output, "performance", "metrics_external.csv")))
})
