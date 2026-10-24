make_train_fixture <- function(n = 80L, seed = 123L) {
  set.seed(seed)
  y <- factor(rep(c("no", "yes"), length.out = n), levels = c("no", "yes"))
  data.frame(
    y = y,
    x1 = ifelse(y == "yes", 1.1, -1.1) + stats::rnorm(n),
    x2 = stats::rnorm(n),
    x3 = stats::rnorm(n),
    stringsAsFactors = FALSE
  )
}

test_that("train_model() trains a cloned mlr3 learner", {
  data <- make_train_fixture()
  spec <- uniml_spec(task_type = "binary", outcome = "y", positive = "yes")
  task <- make_task(data, spec)
  learner <- mlr3::lrn("classif.rpart", predict_type = "prob", cp = 0.02)
  original_cp <- learner$param_set$values$cp

  trained <- train_model(task, learner, params = list(minsplit = 8L))
  prediction <- trained$predict(task)

  expect_s3_class(trained, "Learner")
  expect_identical(trained$predict_type, "prob")
  expect_equal(trained$param_set$values$cp, original_cp)
  expect_identical(trained$param_set$values$minsplit, 8L)
  expect_false(is.null(trained$model))
  expect_s3_class(prediction, "PredictionClassif")
  expect_identical(dim(prediction$prob), c(task$nrow, 2L))
  expect_setequal(colnames(prediction$prob), c("no", "yes"))
})

test_that("train_model() accepts an uniml learner specification", {
  data <- make_train_fixture()
  spec <- uniml_spec(task_type = "binary", outcome = "y", positive = "yes")
  spec <- set_learners(
    spec,
    "cart",
    tune = FALSE,
    params = list(cp = 0.04)
  )
  task <- make_task(data, spec)
  learner_spec <- resolve_learners(spec, task = task)[[1L]]

  trained <- train_model(task, learner_spec, params = list(minsplit = 10L))

  expect_equal(trained$param_set$values$cp, 0.04)
  expect_identical(trained$param_set$values$minsplit, 10L)
  expect_identical(trained$predict_type, "prob")
})

test_that("train_model() validates its inputs", {
  data <- make_train_fixture()
  spec <- uniml_spec(task_type = "binary", outcome = "y", positive = "yes")
  task <- make_task(data, spec)

  expect_error(
    train_model(data, mlr3::lrn("classif.rpart")),
    "classification task"
  )
  expect_error(train_model(task, list()), "learner")
  expect_error(
    train_model(task, mlr3::lrn("classif.rpart"), params = list(cp = "bad")),
    "cp"
  )
})
