cvd_workflow_example_path <- function(file_name) {
  installed <- system.file("extdata", file_name, package = "uniml")
  if (nzchar(installed)) {
    return(installed)
  }
  testthat::test_path("..", "..", "inst", "extdata", file_name)
}

read_cvd_workflow_example <- function(file_name) {
  utils::read.csv(
    cvd_workflow_example_path(file_name),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

test_that("run_workflow() trains and ranks the cardiovascular example", {
  skip_if_not_installed("rpart")
  # This is the end-to-end run on the largest shipped cohort and the slowest test
  # in the suite by far. Everything it covers - assembly, ranking, selection,
  # prediction - is also covered on small synthetic data in test-workflow.R, which
  # does run on CRAN; this one runs on every commit in CI instead, so the CRAN
  # check stays inside its ten-minute budget.
  skip_on_cran()

  development <- read_cvd_workflow_example("uniml-cvd-mortality.csv")
  new_admissions <- read_cvd_workflow_example(
    "uniml-cvd-mortality-new.csv"
  )

  spec <- uniml_spec(
    task_type = "binary",
    outcome = "died",
    positive = "yes",
    id = "id"
  )
  spec <- set_preprocess(
    spec,
    impute = "median_mode",
    scale = "zscore",
    encode = "onehot"
  )
  spec <- set_feature_selection(spec, methods = "univariate")
  spec <- set_learners(spec, c("lr", "cart"), tune = FALSE)
  spec <- set_tuning(spec, folds = 3, evals = 1)
  spec <- set_evaluation(
    spec,
    ci = "none",
    calibration = "none",
    select_on = "validation"
  )

  split <- make_split(
    as_uniml_data(development, spec),
    prop = c(0.8, 0.2),
    stratify = TRUE,
    seed = 123
  )

  result <- run_workflow(
    spec,
    split$train$data,
    split$validation$data,
    new_admissions
  )

  expect_s3_class(result, "uniml_result")
  expect_setequal(names(result$models), c("lr", "cart"))
  expect_identical(nrow(result$ranking), 2L)
  expect_identical(result$ranking$model[1L], "lr")
  expect_true(all(c("age", "nt_probnp", "creatinine") %in%
    result$selection$selected_features))

  validation_auc <- result$metrics$auc[
    result$metrics$set == "validation"
  ][1L]
  test_auc <- result$metrics$auc[
    result$metrics$set == "test"
  ][1L]

  expect_gte(validation_auc, 0.80)
  expect_gte(test_auc, 0.80)

  predicted <- predict_uniml(
    result,
    newdata = new_admissions,
    model = "best"
  )

  expect_s3_class(predicted, "data.frame")
  expect_identical(nrow(predicted), nrow(new_admissions))
})
