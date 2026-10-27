test_that("plot_roc() adds the micro-average curve for multiclass predictions", {
  set.seed(41)
  n <- 150L
  classes <- c("low", "mid", "high")
  truth <- factor(sample(classes, n, TRUE, prob = c(0.6, 0.3, 0.1)),
                  levels = classes)
  prob <- matrix(stats::runif(n * 3L), nrow = n,
                 dimnames = list(NULL, classes))
  prob <- prob / rowSums(prob)
  prediction <- as_uniml_multiclass(prob, truth = truth)

  plot <- plot_roc(prediction)
  expect_s3_class(plot, "ggplot")
  expect_true("micro-average" %in% levels(plot$data$class))
  expect_setequal(
    as.character(unique(plot$data$class)),
    c(classes, "micro-average")
  )
  # The subtitle names all three estimands.
  expect_match(plot$labels$subtitle, "Hand-Till")
  expect_match(plot$labels$subtitle, "macro one-vs-rest")
  expect_match(plot$labels$subtitle, "micro")

  plain <- plot_roc(prediction, micro = FALSE)
  expect_false("micro-average" %in% levels(plain$data$class))
  expect_match(plain$labels$subtitle, "Hand-Till")
})
