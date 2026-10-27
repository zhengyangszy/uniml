test_that("methods = \"none\" keeps every candidate", {
  frame <- data.frame(
    y = factor(rep(c("no", "yes"), 6), levels = c("no", "yes")),
    x1 = stats::rnorm(12),
    x2 = stats::rnorm(12),
    g = factor(rep(c("a", "b"), 6))
  )
  spec <- uniml_spec("binary", "y", "yes")
  spec <- set_feature_selection(spec, methods = "none")
  result <- select_features(frame, spec)

  expect_identical(result$method, "none")
  expect_setequal(result$selected_features, c("x1", "x2", "g_a", "g_b"))
  expect_true(all(result$details$selected))

  # An empty method vector means the same thing.
  empty <- spec
  empty$selection$methods <- character()
  expect_setequal(select_features(frame, empty)$selected_features,
                  result$selected_features)

  expect_error(
    set_feature_selection(spec, methods = "nope"),
    "unsupported"
  )
})
