make_tune_fixture <- function(n = 90L, seed = 7L) {
  set.seed(seed)
  y <- factor(rep(c("no", "yes"), length.out = n), levels = c("no", "yes"))
  data.frame(
    y = y,
    x1 = ifelse(y == "yes", 1.4, -1.4) + stats::rnorm(n),
    x2 = stats::rnorm(n),
    x3 = stats::rnorm(n),
    stringsAsFactors = FALSE
  )
}

make_tuning_setup <- function(metric = "auc", nests = FALSE,
                              search_space = paradox::ps(
                                maxdepth = paradox::p_int(lower = 2L, upper = 3L)
                              )) {
  data <- make_tune_fixture()
  spec <- uniml_spec(task_type = "binary", outcome = "y", positive = "yes")
  spec <- set_tuning(spec, metric = metric, nested = nests, seed = 123)
  spec <- set_learners(
    spec,
    "cart",
    tune = TRUE,
    search_space = search_space
  )
  task <- make_task(data, spec)
  learner_spec <- resolve_learners(spec, task = task)[[1L]]
  list(task = task, learner_spec = learner_spec)
}

test_that("tune_model() performs random search and reports its status", {
  setup <- make_tuning_setup()

  result <- tune_model(
    setup$task,
    setup$learner_spec,
    resampling = mlr3::rsmp("cv", folds = 3L),
    tuner = "random_search",
    terminator = mlr3tuning::trm("evals", n_evals = 2L),
    seed = 123
  )

  expect_s3_class(result, "uniml_tuning_result")
  expect_identical(result$status, "tuned")
  expect_identical(result$learner_id, "cart")
  expect_true(length(result$best_params) >= 1L)
  expect_true(is.finite(result$best_metric))
  expect_true(all(is.finite(result$cv_scores$classif.auc)))
  expect_s3_class(result$cv_result, "ResampleResult")
  expect_true(nrow(result$archive$data) >= 1L)
})

test_that("tune_model() supports grid search and repeated CV", {
  setup <- make_tuning_setup()

  result <- tune_model(
    setup$task,
    setup$learner_spec,
    resampling = mlr3::rsmp("repeated_cv", folds = 3L, repeats = 2L),
    tuner = "grid_search",
    terminator = mlr3tuning::trm("evals", n_evals = 2L),
    seed = 123
  )

  expect_identical(result$status, "tuned")
  expect_equal(nrow(result$cv_scores), 6L)
  expect_true(all(is.finite(result$cv_scores$classif.auc)))
})

test_that("tune_model() supports nested tuning", {
  setup <- make_tuning_setup(nests = TRUE)

  result <- tune_model(
    setup$task,
    setup$learner_spec,
    resampling = mlr3::rsmp("cv", folds = 3L),
    tuner = "random_search",
    terminator = mlr3tuning::trm("evals", n_evals = 2L),
    nested = TRUE,
    seed = 123
  )

  expect_identical(result$status, "nested_tuned")
  expect_true(result$nested)
  expect_true(length(result$best_params) >= 1L)
  expect_s3_class(result$cv_result, "ResampleResult")
})

test_that("tune_model() is reproducible with the same seed", {
  setup <- make_tuning_setup(
    search_space = paradox::ps(
      cp = paradox::p_dbl(lower = 0.001, upper = 0.05),
      maxdepth = paradox::p_int(lower = 2L, upper = 4L)
    )
  )
  resampling <- mlr3::rsmp("cv", folds = 3L)
  terminator <- mlr3tuning::trm("evals", n_evals = 3L)

  first <- tune_model(
    setup$task,
    setup$learner_spec,
    resampling = resampling,
    tuner = "random_search",
    terminator = terminator,
    seed = 2026
  )
  second <- tune_model(
    setup$task,
    setup$learner_spec,
    resampling = resampling,
    tuner = "random_search",
    terminator = terminator,
    seed = 2026
  )

  expect_equal(first$best_params, second$best_params, tolerance = 1e-12)
  expect_equal(first$best_metric, second$best_metric, tolerance = 1e-12)
  expect_equal(
    first$cv_scores$classif.auc,
    second$cv_scores$classif.auc,
    tolerance = 1e-12
  )
})

test_that("tune_model() reports an untuned learner when tuning is disabled", {
  setup <- make_tuning_setup()
  setup$learner_spec$tune <- FALSE

  result <- tune_model(
    setup$task,
    setup$learner_spec,
    resampling = mlr3::rsmp("cv", folds = 3L),
    tuner = "random_search",
    terminator = mlr3tuning::trm("evals", n_evals = 1L),
    seed = 123
  )

  expect_identical(result$status, "untuned")
  expect_true(is.na(result$best_metric))
  expect_s3_class(result$cv_result, "ResampleResult")
})

test_that("tune_model() rejects unsupported kappa tuning", {
  setup <- make_tuning_setup(metric = "kappa")

  expect_error(
    tune_model(
      setup$task,
      setup$learner_spec,
      resampling = mlr3::rsmp("cv", folds = 3L),
      terminator = mlr3tuning::trm("evals", n_evals = 1L)
    ),
    "kappa"
  )
})

test_that("Bayesian tuning is available when mlr3mbo is installed", {
  skip_if_not_installed("mlr3mbo")
  skip_if_not_installed("DiceKriging")
  setup <- make_tuning_setup()

  result <- tune_model(
    setup$task,
    setup$learner_spec,
    resampling = mlr3::rsmp("cv", folds = 3L),
    tuner = "bayesian",
    terminator = mlr3tuning::trm("evals", n_evals = 2L),
    seed = 123
  )

  expect_identical(result$status, "tuned")
})
