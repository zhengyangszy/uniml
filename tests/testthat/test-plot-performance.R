make_prediction_frame <- function() {
  data.frame(
    model = rep(c("lr", "cart"), each = 4L),
    prob = c(0.1, 0.4, 0.6, 0.9, 0.2, 0.3, 0.7, 0.8),
    calibrated_prob = c(0.12, 0.38, 0.62, 0.88, 0.22, 0.32, 0.68, 0.78),
    truth = rep(factor(c("no", "no", "yes", "yes"), levels = c("no", "yes")), 2L),
    positive = "yes",
    set = "validation",
    stringsAsFactors = FALSE
  )
}

test_that("normalise_predictions() keeps every model of a prediction frame", {
  normalised <- .normalise_predictions(make_prediction_frame())

  expect_setequal(unique(normalised$model), c("lr", "cart"))
  expect_identical(nrow(normalised), 8L)
  expect_true(all(c("model", "prob", "truth", "calibrated_prob", "positive", "set")
                  %in% names(normalised)))
})

test_that("plot_roc() draws one curve per model of a result", {
  result <- assemble_result(
    metrics = data.frame(
      model = c("lr", "cart"),
      set = "validation",
      probability = "raw",
      auc = c(0.80, 0.75)
    ),
    meta = list(predictions = make_prediction_frame()),
    models = list(
      lr = mlr3::lrn("classif.rpart", predict_type = "prob"),
      cart = mlr3::lrn("classif.rpart", predict_type = "prob")
    ),
    ranking = data.frame(
      model = c("lr", "cart"),
      probability = "raw",
      score = c(0.8, 0.7),
      rank = 1:2,
      metric = "composite",
      set = "validation",
      stringsAsFactors = FALSE
    )
  )

  plot <- plot_roc(result, set = "validation")
  expect_s3_class(plot, "ggplot")
  expect_setequal(unique(plot$data$model), c("lr", "cart"))

  best <- plot_roc(result, models = "best", set = "validation")
  expect_identical(unique(best$data$model), "lr")
})

test_that("plot_confusion() accepts the best-model shortcut", {
  result <- assemble_result(
    meta = list(predictions = make_prediction_frame()),
    models = list(
      lr = mlr3::lrn("classif.rpart", predict_type = "prob"),
      cart = mlr3::lrn("classif.rpart", predict_type = "prob")
    ),
    ranking = data.frame(
      model = c("lr", "cart"),
      probability = "raw",
      score = c(0.8, 0.7),
      rank = 1:2,
      metric = "composite",
      set = "validation",
      stringsAsFactors = FALSE
    )
  )

  plot <- plot_confusion(result, models = "best", set = "validation")
  expect_s3_class(plot, "ggplot")
})
