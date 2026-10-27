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

test_that("plot_pr_curve() draws the same area the table reports", {
  frame <- make_prediction_frame()
  plot <- plot_pr_curve(frame)
  expect_s3_class(plot, "ggplot")
  expect_identical(plot$labels$x, "Recall (sensitivity)")
  expect_identical(plot$labels$y, "Precision (PPV)")

  # The label carries `calc_metrics()`'s PR AUC, so the figure and the table
  # cannot disagree.
  subset <- frame[frame$model == "lr", , drop = FALSE]
  reported <- calc_metrics(subset$prob, subset$truth, positive = "yes")$pr_auc
  expect_true(any(grepl(
    sprintf("PR AUC = %.3f", reported),
    as.character(plot$data$label),
    fixed = TRUE
  )))
  # The curve spans the whole recall range and sits above zero precision.
  expect_true(all(plot$data$recall >= 0 & plot$data$recall <= 1))
  expect_true(all(plot$data$precision >= 0 & plot$data$precision <= 1))

  best <- plot_pr_curve(frame, models = "cart")
  expect_identical(unique(best$data$model), "cart")

  # A single-class outcome has no precision-recall curve to draw.
  one_class <- data.frame(
    model = "lr",
    prob = c(0.2, 0.4, 0.6, 0.8),
    truth = factor(rep("yes", 4L), levels = c("no", "yes")),
    positive = "yes"
  )
  expect_error(plot_pr_curve(one_class), "two outcome classes")
  expect_error(plot_pr_curve(structure(list(), class = "nope")), "no method")
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
