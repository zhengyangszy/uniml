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

make_encoded_shap_fixture <- function(encode) {
  set.seed(21)
  n <- 60
  train <- data.frame(
    y = factor(rep(c("no", "yes"), length.out = n)),
    x1 = seq(10, 50, length.out = n),
    skewed = exp(stats::rnorm(n, 0, 1.1)),
    z = factor(sample(c("a", "b", "c"), n, replace = TRUE))
  )
  preprocessor <- fit_preprocessor(
    train,
    impute = "none",
    scale = "zscore",
    encode = encode,
    transform = "log1p",
    outcome = "y"
  )
  data <- preprocessor$train
  features <- data[, setdiff(names(data), "y"), drop = FALSE]
  task <- mlr3::as_task_classif(data, target = "y", positive = "yes")
  learner <- mlr3::lrn("classif.rpart", predict_type = "prob", model = TRUE)
  learner$train(task)
  result <- assemble_result(
    data_info = list(n = n),
    preprocess = preprocessor,
    models = list(rpart = learner)
  )
  list(train = train, features = features, result = result)
}

test_that("SHAP raw track displays the original patient values", {
  skip_if_not_installed("kernelshap")
  skip_if_not_installed("shapviz")

  fixture <- make_encoded_shap_fixture("onehot")
  explained <- explain_shap(
    fixture$result,
    data = fixture$features,
    orig_data = fixture$train[, setdiff(names(fixture$train), "y")],
    model = "rpart",
    n_explain = 20,
    bg_n = 20,
    seed = 1
  )
  expect_s3_class(explained$shap$objects_raw, "shapviz")

  set.seed(1)
  rows <- sort(sample.int(60L, 20L))
  raw_values <- shapviz::get_feature_values(explained$shap$objects_raw)
  # Numeric columns come straight from the original frame: imputation,
  # scaling, encoding, and the log transform are not reflected.
  expect_equal(raw_values$x1, fixture$train$x1[rows])
  expect_equal(raw_values$skewed, fixture$train$skewed[rows])
  # Categorical levels decode from the original factor.
  codes <- raw_values$z
  expect_identical(
    as.numeric(codes),
    as.numeric(match(
      as.character(fixture$train$z[rows]),
      levels(fixture$train$z)
    ) - 1L)
  )
  expect_identical(attr(codes, "uniml_levels"), levels(fixture$train$z))
  # The attribute must not be called "levels": data.frame algebra (rbind)
  # re-coerces such columns and corrupts the values.
  expect_null(attr(explained$shap$feature_values_raw$z, "levels"))
})

test_that("impact-encoded categoricals stay plottable in SHAP", {
  skip_if_not_installed("kernelshap")
  skip_if_not_installed("shapviz")

  fixture <- make_encoded_shap_fixture("impact")
  explained <- explain_shap(
    fixture$result,
    data = fixture$features,
    orig_data = fixture$train[, setdiff(names(fixture$train), "y")],
    model = "rpart",
    n_explain = 20,
    bg_n = 20,
    seed = 2
  )
  # Regression guard: impact encoding used to abort the whole aggregation
  # ("attempt to set an attribute on NULL") and drop the variable.
  expect_s3_class(explained$shap$objects, "shapviz")
  expect_true("z" %in% colnames(shapviz::get_shap_values(explained$shap$objects)))
  expect_true("z" %in% names(shapviz::get_feature_values(explained$shap$objects)))

  set.seed(2)
  rows <- sort(sample.int(60L, 20L))
  raw_values <- shapviz::get_feature_values(explained$shap$objects_raw)
  expect_identical(
    as.numeric(raw_values$z),
    as.numeric(match(
      as.character(fixture$train$z[rows]),
      levels(fixture$train$z)
    ) - 1L)
  )
  expect_s3_class(plot_shap_dependence(explained, feature = "z"), "ggplot")
})
