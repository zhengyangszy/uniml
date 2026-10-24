make_shap_learner_fixture <- function(seed = 123L, n = 40L) {
  set.seed(seed)
  data <- data.frame(
    y = factor(rep(c("no", "yes"), length.out = n)),
    x1 = stats::rnorm(n),
    x2 = stats::runif(n)
  )
  task <- mlr3::as_task_classif(data, target = "y", positive = "yes")
  learner <- mlr3::lrn("classif.rpart", predict_type = "prob", model = TRUE)
  learner$train(task)
  learner
}

make_shap_data_fixture <- function(n = 12L) {
  data.frame(
    x1 = seq(-1, 1, length.out = n),
    x2 = seq(0, 1, length.out = n)
  )
}

test_that("explain_shap.Learner returns dual-track matrices and importance", {
  skip_if_not_installed("kernelshap")
  skip_if_not_installed("shapviz")

  learner <- make_shap_learner_fixture()
  data <- make_shap_data_fixture()
  raw_data <- data.frame(
    x1 = data$x1 * 10,
    x2 = data$x2 + 5
  )
  result <- explain_shap(
    learner,
    data = data,
    raw_data = raw_data,
    seed = 1L,
    positive = "yes"
  )

  expect_s3_class(result, "uniml_shap")
  expect_s3_class(result$objects, "shapviz")
  expect_equal(dim(result$values_matrix_std), c(nrow(data), 3L))
  expect_equal(dim(result$values_matrix_raw), c(nrow(data), 3L))
  expect_identical(tail(names(result$values_matrix_std), 1L), "model")
  expect_identical(result$feature_values_raw$x1, raw_data$x1)
  expect_true(all(c("model", "feature", "mean_abs_shap", "rank") %in%
                    names(result$importance)))
  expect_identical(result$models, learner$id)
})

test_that("explain_shap combines similarly shaped models", {
  skip_if_not_installed("kernelshap")
  skip_if_not_installed("shapviz")

  learner <- make_shap_learner_fixture()
  data <- make_shap_data_fixture()
  result <- explain_shap(
    list(cart_a = learner, cart_b = learner),
    data = data,
    seed = 2L,
    positive = "yes"
  )

  expect_s3_class(result$objects, "mshapviz")
  expect_identical(result$models, c("cart_a", "cart_b"))
  expect_true(all(c("cart_a", "cart_b") %in% result$importance$model))
})

test_that("explain_shap stores SHAP results on a uniml result", {
  skip_if_not_installed("kernelshap")
  skip_if_not_installed("shapviz")

  learner <- make_shap_learner_fixture()
  result <- assemble_result(
    data_info = list(n = 40L),
    models = list(cart = learner)
  )
  updated <- explain_shap(
    result,
    data = make_shap_data_fixture(),
    seed = 3L,
    positive = "yes"
  )

  expect_s3_class(updated, "uniml_result")
  expect_s3_class(updated$shap, "uniml_shap")
  expect_identical(result$shap, NULL)
})

test_that("explain_shap rejects objects without trained learners", {
  expect_error(
    explain_shap.Learner(list(foo = 1), data = data.frame(x1 = 1)),
    "trained mlr3 learners"
  )
})
