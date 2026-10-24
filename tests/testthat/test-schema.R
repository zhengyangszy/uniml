.sample_data <- function(n = 80L, seed = 1L) {
  set.seed(seed)
  data.frame(
    PT = seq_len(n),
    y = factor(rep(c("Yes", "No"), length.out = n), levels = c("Yes", "No")),
    age = stats::rnorm(n, 60, 10),
    bmi = stats::rnorm(n, 25, 4),
    sex = factor(rep(c("M", "F"), length.out = n)),
    stringsAsFactors = FALSE
  )
}

test_that("uniml_spec() builds a valid spec with documented defaults", {
  spec <- uniml_spec(task_type = "binary", outcome = "y", positive = "Yes")

  expect_s3_class(spec, "uniml_spec")
  expect_identical(spec$task_type, "binary")
  expect_identical(spec$outcome, "y")
  expect_identical(spec$positive, "Yes")
  expect_null(spec$id)
  expect_null(spec$exclude)
  expect_null(spec$labels)

  expect_identical(spec$leakage_policy, "custom")
  expect_identical(spec$in_cv, c("selection", "balance", "tuning"))
  expect_false(spec$nested_tuning)
  expect_identical(spec$seed, 123)
  expect_identical(spec$ordered_as, "ordinal")
  expect_identical(spec$outcome_na_action, "drop")
  expect_identical(spec$threshold_method, "youden")
  expect_identical(spec$cost, c(fp = 1, fn = 1))
  expect_identical(spec$select_on, "validation")
  expect_identical(spec$best_model_metric, "composite")
  expect_identical(spec$verbose, 1L)
  expect_identical(spec$n_workers, 1L)
  expect_identical(spec$future_strategy, "multisession")

  expect_identical(spec$preprocess$impute, "median_mode")
  expect_identical(spec$preprocess$m, 5L)
  expect_identical(spec$preprocess$scale, "zscore")
  expect_identical(spec$preprocess$encode, "onehot")
  expect_identical(spec$preprocess$balance, "none")

  expect_identical(spec$selection$methods, "lasso")
  expect_identical(spec$selection$lambda, "1se")
  expect_identical(spec$selection$p_threshold, 0.2)
  expect_identical(spec$selection$maxRuns, 50L)
  expect_identical(spec$selection$multivariate, "stepwise")
  expect_identical(spec$selection$combine, "intersect")
  expect_identical(spec$selection$representation, "full_levels")

  expect_identical(spec$tuning$tuner, "random_search")
  expect_identical(spec$tuning$folds, 5L)
  expect_identical(spec$tuning$repeats, 1L)
  expect_identical(spec$tuning$evals, 30L)
  expect_identical(spec$tuning$metric, "auc")
  expect_false(spec$tuning$nested)

  expect_identical(spec$evaluation$metrics, "all")
  expect_identical(spec$evaluation$ci, "bootstrap")
  expect_identical(spec$evaluation$calibration, "none")
  expect_identical(spec$evaluation$calibration_args$fit, "out_of_fold")
  expect_identical(spec$evaluation$tests, c("delong", "hl"))
  expect_true(spec$evaluation$dca)
  expect_identical(spec$preprocess$impute_args, list())
  expect_identical(spec$selection$args, list())
  expect_identical(spec$tuning$terminator, "evals")

  expect_identical(spec$explain$model, "best")
  expect_identical(
    spec$explain$plots,
    c("importance", "beeswarm", "dependence", "waterfall")
  )
  expect_identical(spec$output$dir, "output")

  expect_true(validate_uniml_spec(spec))
  expect_identical(unclass(print(spec)), unclass(spec))
})

test_that("uniml_spec() accepts role columns and override fields", {
  spec <- uniml_spec(
    task_type = "binary", outcome = "y", positive = "Yes",
    id = "PT", exclude = "HTADDT", labels = c(y = "Outcome"),
    seed = 7, verbose = 2
  )
  expect_identical(spec$id, "PT")
  expect_identical(spec$exclude, "HTADDT")
  expect_identical(spec$labels, c(y = "Outcome"))
  expect_identical(spec$seed, 7)
  expect_identical(spec$verbose, 2)
  expect_identical(spec$output$verbose, 2)
})

test_that("uniml_spec() applies leakage-policy shortcuts", {
  strict_in <- uniml_spec(
    task_type = "binary", outcome = "y", positive = "Yes",
    leakage_policy = "strict_in"
  )
  expect_identical(
    strict_in$in_cv,
    c("imputation", "scaling", "encoding", "selection", "balance", "tuning")
  )

  strict_out <- uniml_spec(
    task_type = "binary", outcome = "y", positive = "Yes",
    leakage_policy = "strict_out"
  )
  expect_identical(strict_out$in_cv, character())
})

test_that("uniml_spec() rejects invalid arguments", {
  expect_error(uniml_spec(task_type = "binary", positive = "Yes"), "outcome")
  expect_error(uniml_spec(task_type = "binary", outcome = "y"), "positive")
  expect_error(uniml_spec(outcome = "y", positive = "Yes"), "task_type")
  expect_error(
    uniml_spec(task_type = "multiclass", outcome = "y", positive = "Yes"),
    "task_type"
  )
  expect_error(
    uniml_spec(task_type = "binary", outcome = "y", positive = NA_character_),
    "positive"
  )
  expect_error(
    uniml_spec(task_type = "binary", outcome = "y", positive = "Yes", seed = "a"),
    "seed"
  )
  expect_error(
    uniml_spec(task_type = "binary", outcome = "y", positive = "Yes",
               leakage_policy = "loose"),
    "leakage_policy"
  )
  expect_error(
    uniml_spec(task_type = "binary", outcome = "y", positive = "Yes",
               threshold_method = "median"),
    "threshold_method"
  )
  expect_error(
    uniml_spec(task_type = "binary", outcome = "y", positive = "Yes",
               threshold_method = "fixed"),
    "fixed_threshold"
  )
  expect_error(
    uniml_spec(task_type = "binary", outcome = "y", positive = "Yes",
               verbose = 3),
    "verbose"
  )
  expect_error(
    uniml_spec(task_type = "binary", outcome = "y", positive = "Yes",
               n_workers = 0),
    "n_workers"
  )
  expect_error(
    uniml_spec(task_type = "binary", outcome = "y", positive = "Yes",
               select_on = "train"),
    "select_on"
  )
  expect_error(
    uniml_spec(task_type = "binary", outcome = "y", positive = "Yes",
               unknown_field = 1),
    "Unknown specification field"
  )
  expect_error(
    uniml_spec(task_type = "binary", outcome = "y", positive = "Yes",
               id = "y"),
    "outcome column"
  )
})

test_that("as_uniml_data() validates and annotates roles", {
  spec <- uniml_spec(task_type = "binary", outcome = "y", positive = "Yes", id = "PT")
  d <- as_uniml_data(.sample_data(), spec)

  expect_s3_class(d, "uniml_data")
  expect_true(validate_uniml_data(d))
  expect_identical(unname(d$role[c("PT", "y", "age")]), c("id", "outcome", "feature"))
  expect_identical(d$outcome_name, "y")
  expect_identical(d$positive, "Yes")
  expect_s3_class(d$outcome, "factor")
  expect_identical(levels(d$outcome), c("Yes", "No"))
  expect_identical(setdiff(names(d$features), "y"), c("age", "bmi", "sex"))
  expect_false("PT" %in% names(d$features))
  expect_identical(d$meta$n, 80L)
  expect_identical(d$meta$n_features, 3L)
  expect_identical(d$meta$n_dropped, 0L)
  expect_true(is.data.frame(d$types))
  expect_identical(unclass(print(d)), unclass(d))
})

test_that("as_uniml_data() drops missing outcomes by default", {
  raw <- .sample_data()
  raw$y[3] <- NA
  spec <- uniml_spec(task_type = "binary", outcome = "y", positive = "Yes")
  d <- as_uniml_data(raw, spec)
  expect_identical(d$meta$n_dropped, 1L)
  expect_identical(nrow(d$data), 79L)
  expect_false(anyNA(d$outcome))

  keep_spec <- uniml_spec(task_type = "binary", outcome = "y", positive = "Yes",
                          outcome_na_action = "keep")
  d_keep <- as_uniml_data(raw, keep_spec)
  expect_identical(d_keep$meta$n_dropped, 0L)
  expect_identical(nrow(d_keep$data), 80L)
  expect_true(anyNA(d_keep$outcome))
})

test_that("as_uniml_data() warns on degenerate feature columns", {
  raw <- .sample_data()
  raw$all_na <- NA_real_
  raw$constant <- 1
  raw$dup_age <- raw$age
  spec <- uniml_spec(task_type = "binary", outcome = "y", positive = "Yes")
  expect_warning(d <- as_uniml_data(raw, spec), "All-NA")
  expect_true(length(d$warnings) >= 1L)
  expect_true(any(grepl("Constant", d$warnings)))
})

test_that("as_uniml_data() rejects malformed inputs", {
  spec <- uniml_spec(task_type = "binary", outcome = "y", positive = "Yes")
  expect_error(as_uniml_data(list(y = 1), spec), "data frame")
  expect_error(as_uniml_data(data.frame(x = 1), spec), "not found")
  expect_error(
    as_uniml_data(data.frame(y = c("Yes", "No", "Maybe")), spec),
    "exactly two"
  )
  expect_error(
    as_uniml_data(.sample_data(), uniml_spec("binary", "y", "Unknown")),
    "positive"
  )
  expect_error(
    as_uniml_data(.sample_data()[, "y", drop = FALSE], spec),
    "At least one feature"
  )
})

test_that("make_split() produces two- and three-segment splits", {
  spec <- uniml_spec(task_type = "binary", outcome = "y", positive = "Yes", id = "PT")
  d <- as_uniml_data(.sample_data(n = 100L), spec)

  two <- make_split(d, prop = c(0.7, 0.3), seed = 123)
  expect_s3_class(two, "uniml_split")
  expect_true(validate_uniml_split(two))
  expect_null(two$test)
  expect_identical(two$meta$sizes, c(train = 70L, validation = 30L))
  expect_identical(sum(two$meta$sizes), 100L)

  three <- make_split(d, prop = c(0.6, 0.2, 0.2), seed = 123)
  expect_false(is.null(three$test))
  expect_identical(unname(three$meta$sizes), c(60L, 20L, 20L))
  expect_true(validate_uniml_split(three))
  expect_identical(unclass(print(three)), unclass(three))
})

test_that("make_split() is reproducible for the same seed", {
  spec <- uniml_spec(task_type = "binary", outcome = "y", positive = "Yes", id = "PT")
  d <- as_uniml_data(.sample_data(n = 100L), spec)

  s1 <- make_split(d, prop = c(0.6, 0.4), seed = 42)
  s2 <- make_split(d, prop = c(0.6, 0.4), seed = 42)
  s3 <- make_split(d, prop = c(0.6, 0.4), seed = 43)

  expect_identical(s1$train$data, s2$train$data)
  expect_identical(s1$validation$data, s2$validation$data)
  expect_false(identical(s1$train$data, s3$train$data))
})

test_that("make_split() stratifies by outcome when requested", {
  spec <- uniml_spec(task_type = "binary", outcome = "y", positive = "Yes", id = "PT")
  d <- as_uniml_data(.sample_data(n = 200L), spec)

  s <- make_split(d, prop = c(0.5, 0.25, 0.25), stratify = TRUE, seed = 1)
  rates <- vapply(
    list(s$train, s$validation, s$test),
    function(part) mean(as.character(part$outcome) == "Yes"),
    numeric(1)
  )
  expect_true(all(abs(rates - 0.5) < 0.1))
  expect_true(all(vapply(
    list(s$train, s$validation, s$test),
    function(part) length(unique(part$outcome)) == 2L,
    logical(1)
  )))
})

test_that("make_split() rejects invalid proportions and missing outcomes", {
  spec <- uniml_spec(task_type = "binary", outcome = "y", positive = "Yes")
  d <- as_uniml_data(.sample_data(n = 40L), spec)
  expect_error(make_split(d, prop = c(0.5, 0.4)), "sum to 1")
  expect_error(make_split(d, prop = c(0.5, 0.5, 0.5)), "sum to 1")
  expect_error(make_split(d, prop = c(1, 0, 0)), "positive")
  expect_error(make_split(d, prop = c(0.5, 0.5), stratify = "yes"), "stratify")

  raw <- .sample_data(n = 40L)
  raw$y[1] <- NA
  keep_spec <- uniml_spec(task_type = "binary", outcome = "y", positive = "Yes",
                          outcome_na_action = "keep")
  d_keep <- as_uniml_data(raw, keep_spec)
  expect_error(make_split(d_keep, prop = c(0.5, 0.5)), "non-missing outcome")
})

test_that("new_uniml_result() builds, prints, and summarises", {
  res <- new_uniml_result(metrics = data.frame(auc = 0.8), meta = list(seed = 123))
  expect_s3_class(res, "uniml_result")
  expect_true(validate_uniml_result(res))
  expect_true(is.null(res$shap))
  expect_identical(res$metrics$auc, 0.8)
  expect_identical(unclass(print(res)), unclass(res))

  s <- summary(res)
  expect_s3_class(s, "summary.uniml_result")
  expect_true("metrics" %in% s$populated)
  expect_true("shap" %in% s$empty)
  expect_identical(unclass(print(s)), unclass(s))

  expect_error(new_uniml_result(nope = 1), "Unknown result component")
  expect_error(
    new_uniml_result(stats::setNames(list(1), "")),
    "must be named"
  )
})

test_that("spec setters update and re-validate the spec", {
  base <- uniml_spec(task_type = "binary", outcome = "y", positive = "Yes")

  p <- set_preprocess(base, impute = "mice", m = 3, scale = "minmax",
                      encode = "onehot_drop", balance = "smote")
  expect_s3_class(p, "uniml_spec")
  expect_identical(p$preprocess$impute, "mice")
  expect_identical(p$preprocess$m, 3)
  expect_identical(p$preprocess$scale, "minmax")
  expect_identical(p$preprocess$encode, "onehot_drop")
  expect_identical(p$preprocess$balance, "smote")

  sel <- set_feature_selection(p, methods = c("lasso", "boruta"),
                               lambda = "min", p_threshold = 0.1,
                               combine = "union", k = 5)
  expect_identical(sel$selection$methods, c("lasso", "boruta"))
  expect_identical(sel$selection$lambda, "min")
  expect_identical(sel$selection$p_threshold, 0.1)
  expect_identical(sel$selection$combine, "union")
  expect_identical(sel$selection$k, 5)

  lr <- set_learners(sel, c("lr", "rf"), tune = FALSE)
  expect_identical(lr$learners$learners, c("lr", "rf"))
  expect_false(lr$learners$tune)

  lk <- set_leakage_policy(lr, mode = "strict_in")
  expect_identical(lk$leakage_policy, "strict_in")
  expect_identical(
    lk$in_cv,
    c("imputation", "scaling", "encoding", "selection", "balance", "tuning")
  )
  lk_out <- set_leakage_policy(lr, mode = "strict_out")
  expect_identical(lk_out$in_cv, character())

  tu <- set_tuning(lk, tuner = "grid_search", folds = 3, repeats = 2,
                   evals = 10, metric = "pr_auc", nested = TRUE, seed = 9)
  expect_identical(tu$tuning$tuner, "grid_search")
  expect_identical(tu$tuning$folds, 3)
  expect_identical(tu$tuning$repeats, 2)
  expect_true(tu$tuning$nested)
  expect_true(tu$nested_tuning)
  expect_identical(tu$seed, 9)

  ev <- set_evaluation(tu, metrics = c("auc", "brier"), ci = "none",
                       threshold_method = "cost", cost = c(fp = 1, fn = 2),
                       reference_model = "lr", best_model = "auc",
                       calibration = "isotonic", tests = "delong",
                       dca = FALSE, select_on = "validation")
  expect_identical(ev$evaluation$metrics, c("auc", "brier"))
  expect_identical(ev$evaluation$ci, "none")
  expect_identical(ev$threshold_method, "cost")
  expect_identical(ev$cost, c(fp = 1, fn = 2))
  expect_identical(ev$reference_model, "lr")
  expect_identical(ev$best_model_metric, "auc")
  expect_identical(ev$evaluation$calibration, "isotonic")
  expect_false(ev$evaluation$dca)
  expect_identical(ev$select_on, "validation")

  ex <- set_explain(
    ev,
    model = "rf",
    plots = c("importance", "beeswarm", "force")
  )
  expect_identical(ex$explain$model, "rf")
  expect_identical(ex$explain$plots, c("importance", "beeswarm", "force"))

  lb <- set_labels(ex, c(y = "Outcome", age = "Age"))
  expect_identical(lb$labels, c(y = "Outcome", age = "Age"))
  expect_null(set_labels(lb)$labels)

  ou <- set_output(lb, verbose = 2, n_workers = 4, future_strategy = "sequential",
                   dir = "out")
  expect_identical(ou$verbose, 2)
  expect_identical(ou$n_workers, 4)
  expect_identical(ou$future_strategy, "sequential")
  expect_identical(ou$output$dir, "out")
  expect_true(validate_uniml_spec(ou))
})

test_that("spec setters forward step-level arguments and reject unknown names", {
  base <- uniml_spec(task_type = "binary", outcome = "y", positive = "Yes")

  pp <- set_preprocess(
    base,
    impute = "mice",
    impute_args = list(max_iter = 7, m = 3),
    scale_args = list(vars = c("x1", "x2")),
    encode_args = list(max_levels = 20),
    balance_args = list(p = 0.4)
  )
  expect_identical(pp$preprocess$impute_args$max_iter, 7)
  expect_identical(pp$preprocess$scale_args$vars, c("x1", "x2"))
  expect_identical(pp$preprocess$encode_args$max_levels, 20)
  expect_identical(pp$preprocess$balance_args$p, 0.4)
  expect_error(set_preprocess(base, impute_args = list(maxit = 3)), "impute_args")
  expect_error(set_preprocess(base, scale_args = list(columns = "x1")), "scale_args")

  sel <- set_feature_selection(
    base,
    methods = c("lasso", "boruta"),
    args = list(lasso = list(nfolds = 5, alpha = 0.8),
                boruta = list(tentative = TRUE))
  )
  expect_identical(sel$selection$args$lasso$nfolds, 5)
  expect_identical(sel$selection$args$boruta$tentative, TRUE)
  expect_error(
    set_feature_selection(base, args = list(lasso = list(folds = 5))),
    "args\\$lasso"
  )
  expect_error(
    set_feature_selection(base, args = list(svm = list(k = 1))),
    "unknown method"
  )

  tun <- set_tuning(base, terminator = "seconds", seconds = 30)
  expect_identical(tun$tuning$terminator, "seconds")
  expect_identical(tun$tuning$seconds, 30)
  expect_error(set_tuning(base, terminator = "evals2"), "terminator")

  ev <- set_evaluation(base, calibration = "platt",
                       calibration_args = list(folds = 3, bins = 8))
  expect_identical(ev$evaluation$calibration_args$folds, 3)
  expect_identical(ev$evaluation$calibration_args$bins, 8)
  expect_identical(ev$evaluation$calibration_args$fit, "out_of_fold")
  expect_error(
    set_evaluation(base, calibration_args = list(fold = 3)),
    "calibration_args"
  )
})

test_that("spec setters reject invalid values", {
  base <- uniml_spec(task_type = "binary", outcome = "y", positive = "Yes")
  expect_error(set_preprocess(base, impute = "mean"), "preprocess\\$impute")
  expect_error(set_feature_selection(base, lambda = "2se"), "selection\\$lambda")
  expect_error(set_feature_selection(base, k = 0), "selection\\$k")
  expect_error(set_learners(base, character()), "non-empty")
  expect_error(set_learners(base, "lr", tune = NA), "tune")
  expect_error(set_leakage_policy(base, mode = "strict"), "mode")
  expect_error(set_tuning(base, folds = 1), "tuning\\$folds")
  expect_error(set_evaluation(base, ci = "jackknife"), "evaluation\\$ci")
  expect_error(set_explain(base, plots = "chart"), "explain\\$plots")
  expect_error(set_labels(base, c("Outcome")), "labels")
  expect_error(set_output(base, n_workers = 0), "n_workers")
})
