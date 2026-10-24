test_that("predict_uniml() reuses fitted state without refitting", {
  skip_if_not_installed("rpart")
  data <- make_workflow_data()
  split <- make_workflow_split(data)
  spec <- make_workflow_spec("strict_out")
  result <- run_workflow(spec, split$train, split$validation, split$test)

  preprocess_before <- result$preprocess
  selection_before <- result$selection
  threshold_before <- result$models$cart$threshold
  state_before <- result$models$cart$learner$state

  predicted <- predict_uniml(result, split$test)

  expect_identical(result$preprocess, preprocess_before)
  expect_identical(result$selection, selection_before)
  expect_identical(result$models$cart$threshold, threshold_before)
  state_after <- result$models$cart$learner$state
  state_expected <- state_before
  state_after$predict_time <- NULL
  state_expected$predict_time <- NULL
  expect_identical(state_after, state_expected)
  expect_equal(nrow(predicted), nrow(split$test))
  expect_identical(
    names(predicted),
    c(
      "model", "prob", "calibrated_prob", "probability", "predicted",
      "threshold", "truth"
    )
  )
  expect_true(all(predicted$prob >= 0 & predicted$prob <= 1))
  expect_identical(unique(predicted$model), "cart")
})

test_that("predict_uniml() applies stored scaling parameters to drifted data", {
  skip_if_not_installed("rpart")
  data <- make_workflow_data()
  split <- make_workflow_split(data)
  spec <- make_workflow_spec("strict_out")
  result <- run_workflow(spec, split$train, split$validation)

  newdata <- split$test
  newdata$x1 <- newdata$x1 + 100
  stored_scale <- result$preprocess$steps$scale$params$differences$x1
  expected <- (newdata$x1 - stored_scale$center) / stored_scale$scale
  transformed <- apply_preprocessor(result$preprocess, newdata)

  expect_equal(transformed$x1, expected)
  predicted <- predict_uniml(result, newdata)
  expect_equal(nrow(predicted), nrow(newdata))
  expect_identical(
    result$preprocess$steps$scale$params$differences$x1,
    stored_scale
  )
})

test_that("predict_uniml() supports new data without the outcome column", {
  skip_if_not_installed("rpart")
  data <- make_workflow_data()
  split <- make_workflow_split(data)
  spec <- make_workflow_spec("strict_out")
  result <- run_workflow(spec, split$train, split$validation)

  with_outcome <- predict_uniml(result, split$test)
  without_outcome <- predict_uniml(result, split$test[, setdiff(names(split$test), "y")])

  expect_equal(without_outcome$prob, with_outcome$prob, tolerance = 1e-12)
  expect_true(all(is.na(without_outcome$truth)))
  expect_identical(without_outcome$predicted, with_outcome$predicted)
  expect_error(
    predict_uniml(result, split$test[, setdiff(names(split$test), "y")], model = "missing"),
    "Unknown model"
  )
})

test_that("predict() dispatches to predict_uniml()", {
  skip_if_not_installed("rpart")
  data <- make_workflow_data()
  split <- make_workflow_split(data)
  spec <- make_workflow_spec("strict_out")
  result <- run_workflow(spec, split$train, split$validation)

  expect_equal(
    predict(result, split$test),
    predict_uniml(result, split$test),
    tolerance = 1e-12
  )
})

test_that("predict_uniml() averages probabilities across mice imputations", {
  skip_if_not_installed("rpart")
  skip_if_not_installed("mice")
  data <- make_workflow_mice_data()
  split <- make_workflow_split(data)
  spec <- make_workflow_mice_spec()
  result <- run_workflow(spec, split$train, split$validation, split$test)
  entry <- result$models$cart

  expected <- vapply(seq_along(entry$learners), function(index) {
    transformed <- apply_preprocessor(entry$preprocessors[[index]], split$test)
    features <- intersect(
      as.character(entry$selections[[index]]$selected_features),
      names(transformed)
    )
    transformed$y <- factor(
      transformed$y,
      levels = levels(result$preprocess$train$y)
    )
    task <- make_task(
      transformed[, c(features, "y"), drop = FALSE],
      spec
    )
    .workflow_probabilities(
      entry$learners[[index]],
      task,
      spec$positive
    )$prob
  }, numeric(nrow(split$test)))

  predicted <- predict_uniml(result, split$test)

  expect_equal(nrow(predicted), nrow(split$test))
  expect_equal(predicted$prob, rowMeans(expected), tolerance = 1e-12)
  expect_true(all(predicted$prob >= 0 & predicted$prob <= 1))
  expect_identical(
    names(predicted),
    c(
      "model", "prob", "calibrated_prob", "probability", "predicted",
      "threshold", "truth"
    )
  )
})
