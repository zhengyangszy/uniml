make_registry_fixture <- function(n = 60L, seed = 42L) {
  set.seed(seed)
  y <- factor(rep(c("no", "yes"), length.out = n), levels = c("no", "yes"))
  data.frame(
    y = y,
    x1 = ifelse(y == "yes", 1.2, -1.2) + stats::rnorm(n),
    x2 = stats::rnorm(n),
    x3 = stats::rnorm(n),
    x4 = stats::rnorm(n),
    x5 = stats::rnorm(n),
    stringsAsFactors = FALSE
  )
}

test_that("resolve_learners() resolves built-in learners", {
  data <- make_registry_fixture()
  spec <- uniml_spec(task_type = "binary", outcome = "y", positive = "yes")
  spec <- set_learners(spec, c("lr", "cart"), tune = TRUE)
  task <- make_task(data, spec)

  learners <- resolve_learners(spec, task = task)

  expect_identical(names(learners), c("lr", "cart"))
  expect_true(all(vapply(learners, inherits, logical(1), "uniml_learner_spec")))
  expect_true(all(vapply(
    learners,
    function(x) identical(x$learner$predict_type, "prob"),
    logical(1)
  )))
  expect_true(all(vapply(learners, `[[`, logical(1), "tune")))
  expect_true(all(vapply(
    learners,
    function(x) inherits(x$search_space, "ParamSet"),
    logical(1)
  )))
})

test_that("resolve_learners() supports external mlr3 learner keys", {
  data <- make_registry_fixture()
  spec <- uniml_spec(task_type = "binary", outcome = "y", positive = "yes")
  spec <- set_learners(spec, "classif.rpart", tune = FALSE)
  task <- make_task(data, spec)

  learners <- resolve_learners(spec, task = task)

  expect_identical(names(learners), "classif.rpart")
  expect_identical(learners[[1L]]$source, "external")
  expect_identical(learners[[1L]]$learner$id, "classif.rpart")
  expect_false(learners[[1L]]$tune)
})

test_that("optional common learners are available but remain non-default", {
  skip_if_not_installed("e1071")
  keys <- c("log_reg", "svm", "lda", "qda")
  data <- make_registry_fixture()
  spec <- uniml_spec(task_type = "binary", outcome = "y", positive = "yes")
  spec <- set_learners(spec, keys, tune = TRUE)
  task <- make_task(data, spec)

  learners <- resolve_learners(spec, task = task)

  expect_identical(names(learners), keys)
  expect_true(all(vapply(
    learners,
    function(x) identical(x$source, "optional"),
    logical(1)
  )))
  expect_true(all(vapply(
    learners,
    function(x) identical(x$learner$predict_type, "prob"),
    logical(1)
  )))
  expect_false(any(vapply(learners, `[[`, logical(1), "tune")))
  expect_false(any(keys %in% get_task_type("binary")$default_learners))
})

test_that("retired learner keys point at the mlr3 learner to use instead", {
  data <- make_registry_fixture()
  spec <- uniml_spec(task_type = "binary", outcome = "y", positive = "yes")
  spec <- set_learners(spec, "lgb", tune = FALSE)
  task <- make_task(data, spec)

  expect_error(
    resolve_learners(spec, task = task),
    "classif\\.lightgbm"
  )
})

test_that("register_learner() registers custom learners and search spaces", {
  key <- "uniml_test_custom_cart"
  search_space <- paradox::ps(
    cp = paradox::p_dbl(lower = 0.001, upper = 0.1)
  )
  register_learner(
    key,
    mlr3::lrn("classif.rpart", predict_type = "prob"),
    search_space = search_space
  )

  data <- make_registry_fixture()
  spec <- uniml_spec(task_type = "binary", outcome = "y", positive = "yes")
  spec <- set_learners(spec, key, tune = TRUE)
  task <- make_task(data, spec)
  learners <- resolve_learners(spec, task = task)

  expect_identical(names(learners), key)
  expect_identical(learners[[1L]]$source, "custom")
  expect_true(learners[[1L]]$tune)
  expect_s3_class(learners[[1L]]$search_space, "ParamSet")
  expect_identical(learners[[1L]]$learner$id, "classif.rpart")
})

test_that("resolve_learners() applies global and per-learner parameters", {
  data <- make_registry_fixture()
  task_spec <- uniml_spec(task_type = "binary", outcome = "y", positive = "yes")
  task <- make_task(data, task_spec)

  global <- set_learners(
    task_spec,
    "cart",
    tune = FALSE,
    params = list(cp = 0.02, minsplit = 8L)
  )
  global_learner <- resolve_learners(global, task = task)[[1L]]
  expect_equal(global_learner$params$cp, 0.02)
  expect_identical(global_learner$params$minsplit, 8L)
  expect_equal(global_learner$learner$param_set$values$cp, 0.02)

  per_learner <- set_learners(
    task_spec,
    c("cart", "nb"),
    tune = FALSE,
    params = list(
      cart = list(cp = 0.03),
      nb = list(laplace = 2)
    )
  )
  learners <- resolve_learners(per_learner, task = task)
  expect_equal(learners$cart$learner$param_set$values$cp, 0.03)
  expect_equal(learners$nb$learner$param_set$values$laplace, 2)
})

test_that("built-in search spaces adapt to task-dependent values", {
  data <- make_registry_fixture()
  spec <- uniml_spec(task_type = "binary", outcome = "y", positive = "yes")
  spec <- set_learners(spec, c("rf", "knn"), tune = TRUE)
  task <- make_task(data, spec)

  learners <- resolve_learners(spec, task = task, n_train = 12L)

  expect_equal(learners$rf$search_space$lower[["mtry"]], 2)
  expect_equal(learners$rf$search_space$upper[["mtry"]], 3)
  expect_equal(learners$knn$search_space$lower[["k"]], 12)
  expect_equal(learners$knn$search_space$upper[["k"]], 12)
})

test_that("resolve_learners() rejects duplicate and unknown keys", {
  data <- make_registry_fixture()
  spec <- uniml_spec(task_type = "binary", outcome = "y", positive = "yes")
  task <- make_task(data, spec)

  duplicate <- set_learners(spec, c("cart", "cart"), tune = FALSE)
  expect_error(resolve_learners(duplicate, task = task), "unique")

  unknown <- set_learners(spec, "classif.not_a_real_learner", tune = FALSE)
  expect_error(resolve_learners(unknown, task = task), "Unknown mlr3 learner")
})

test_that("tuning metric is carried from the specification", {
  data <- make_registry_fixture()
  spec <- uniml_spec(task_type = "binary", outcome = "y", positive = "yes")
  spec <- set_tuning(spec, metric = "accuracy")
  spec <- set_learners(spec, "cart", tune = TRUE)
  task <- make_task(data, spec)

  learner_spec <- resolve_learners(spec, task = task)[[1L]]
  expect_identical(learner_spec$metric, "accuracy")
})
