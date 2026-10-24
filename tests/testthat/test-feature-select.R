make_selection_data <- function(n = 60L, seed = 42L) {
  set.seed(seed)
  y <- factor(rep(c("no", "yes"), length.out = n), levels = c("no", "yes"))
  data.frame(
    y = y,
    x1 = ifelse(y == "yes", 2, 0) + stats::rnorm(n),
    x2 = stats::rnorm(n),
    x3 = stats::rnorm(n),
    stringsAsFactors = FALSE
  )
}

test_that("select_univariate() retains significant encoded features", {
  data <- make_selection_data()
  result <- select_univariate(data, "y", p_threshold = 0.05)

  expect_s3_class(result, "uniml_selection_result")
  expect_true("x1" %in% result$selected_features)
  expect_identical(
    names(result$details),
    c("feature", "selected", "p_value")
  )
  expect_equal(result$params$p_threshold, 0.05)
  expect_true(all(result$details$selected %in% c(TRUE, FALSE)))

  missing_outcome <- data
  missing_outcome$y[1L] <- NA
  expect_error(select_univariate(missing_outcome, "y"), "non-missing outcome")

  missing_feature <- data
  missing_feature$x1[1L] <- NA
  expect_error(select_univariate(missing_feature, "y"), "complete feature data")

  non_binary <- data
  non_binary$y <- factor(rep(c("a", "b", "c"), 20))
  expect_error(select_univariate(non_binary, "y"), "two-level")
  expect_error(select_univariate(data, "missing"), "not present")
  expect_error(select_univariate(data, "y", p_threshold = 2), "p_threshold")
})

test_that("select_lasso() supports both lambda rules", {
  data <- make_selection_data()
  min_result <- select_lasso(data, "y", lambda = "min", nfolds = 3, seed = 123)
  one_se_result <- select_lasso(data, "y", lambda = "1se", nfolds = 3, seed = 123)

  expect_s3_class(min_result, "uniml_selection_result")
  expect_true("x1" %in% min_result$selected_features)
  expect_true(all(c("feature", "selected", "coefficient") %in% names(min_result$details)))
  expect_identical(one_se_result$params$lambda, "1se")
  expect_identical(one_se_result$params$seed, 123)
  expect_error(
    select_lasso(data, "y", nfolds = 2),
    "at least 3"
  )
})

test_that("select_boruta() returns deterministic filter scores", {
  data <- make_selection_data()
  result <- select_boruta(
    data, "y", maxRuns = 11, num_threads = 1, seed = 123
  )

  expect_s3_class(result, "uniml_selection_result")
  expect_true("x1" %in% result$selected_features)
  expect_identical(result$params$maxRuns, 11L)
  expect_identical(result$params$seed, 123L)
  expect_error(
    select_boruta(data, "y", maxRuns = 10),
    "maxRuns"
  )
})

test_that("select_rf_importance() selects the requested top features", {
  data <- make_selection_data()
  result <- select_rf_importance(
    data, "y", top_k = 2, num_trees = 20, seed = 123
  )

  expect_length(result$selected_features, 2L)
  expect_identical(result$details$rank, seq_len(nrow(result$details)))
  expect_true("importance" %in% names(result$details))
  expect_error(
    select_rf_importance(data, "y", top_k = 0),
    "top_k"
  )
})

test_that("select_rfe() returns the target number of features", {
  data <- make_selection_data()
  result <- select_rfe(data, "y", n_features = 2, seed = 123)

  expect_s3_class(result, "uniml_selection_result")
  expect_length(result$selected_features, 2L)
  expect_identical(result$params$n_features, 2L)
  expect_error(
    select_rfe(data, "y", n_features = 0),
    "n_features"
  )
})

test_that("select_multivariate() supports stepwise and p-value selection", {
  data <- make_selection_data()
  stepwise <- select_multivariate(data, "y", method = "stepwise")
  pvalue <- select_multivariate(
    data, "y", method = "pvalue", p_threshold = 0.2
  )

  expect_true("x1" %in% stepwise$selected_features)
  expect_true("x1" %in% pvalue$selected_features)
  expect_identical(pvalue$params$method, "pvalue")
  expect_true(all(c("p_value", "coefficient") %in% names(pvalue$details)))
})

test_that("combine_selections() intersects or unions feature sets", {
  selections <- list(
    c("x1", "x2"),
    c("x2", "x3")
  )

  expect_identical(combine_selections(selections, "intersect"), "x2")
  expect_identical(
    combine_selections(selections, "union"),
    c("x1", "x2", "x3")
  )
  expect_error(combine_selections(list(), "union"), "non-empty")
  expect_error(combine_selections(list(1), "union"), "selection")
})

test_that("selection representation maps dummy columns to original variables", {
  mapping <- list(
    levels = list(cat = c("a", "b", "c")),
    columns = "cat",
    dropped = list()
  )
  full <- .apply_representation("cat_b", mapping, "full_levels")
  selected <- .apply_representation("cat_b", mapping, "selected_levels")

  expect_identical(full$original_features, "cat")
  expect_identical(full$selected_features, c("cat_a", "cat_b", "cat_c"))
  expect_identical(selected$original_features, "cat")
  expect_identical(selected$selected_features, "cat_b")

  data <- make_selection_data()
  cat_values <- ifelse(data$y == "yes", "a", "b")
  flipped <- c(
    seq(1L, nrow(data), by = 10L),
    seq(6L, nrow(data), by = 10L)
  )
  cat_values[flipped] <- ifelse(cat_values[flipped] == "a", "b", "a")
  data$cat <- factor(cat_values)
  spec <- uniml_spec(task_type = "binary", outcome = "y", positive = "yes")
  spec <- set_feature_selection(
    spec,
    methods = "univariate",
    p_threshold = 0.05,
    representation = "full_levels"
  )
  result <- select_features(data, spec)

  expect_true(all(c("cat_a", "cat_b") %in% result$selected_features))
  expect_true("cat" %in% result$original_features)
})

test_that("select_features() combines methods and rejects empty selections", {
  data <- make_selection_data()
  spec <- uniml_spec(task_type = "binary", outcome = "y", positive = "yes")
  spec <- set_feature_selection(
    spec,
    methods = c("univariate", "rf"),
    p_threshold = 0.05,
    k = 2,
    combine = "union",
    representation = "selected_levels"
  )
  result <- select_features(data, spec)

  expect_true("x1" %in% result$selected_features)
  expect_identical(result$combine, "union")
  expect_true(is.data.frame(result$details))
  expect_true(all(c("method", "method_selected", "combined_selected") %in% names(result$details)))

  empty_spec <- set_feature_selection(
    uniml_spec(task_type = "binary", outcome = "y", positive = "yes"),
    methods = "univariate",
    p_threshold = 0
  )
  expect_error(select_features(data, empty_spec), "no features")
})

test_that("remove_collinear() removes correlated and high-VIF features", {
  data <- data.frame(
    x1 = seq_len(20),
    x2 = seq_len(20) + rep(c(-0.01, 0.01), 10),
    x3 = sin(seq_len(20))
  )
  correlated <- remove_collinear(data, cor_threshold = 0.99)

  expect_identical(correlated$removed_features, "x1")
  expect_identical(correlated$selected_features, c("x2", "x3"))
  expect_identical(correlated$history$method, "correlation")
  expect_true(is.matrix(correlated$correlation))

  set.seed(1)
  vif_data <- data.frame(
    x1 = stats::rnorm(40),
    x2 = stats::rnorm(40),
    x3 = stats::rnorm(40)
  )
  vif_data$x1 <- vif_data$x2 + vif_data$x3 + stats::rnorm(40, sd = 0.05)
  vif_result <- remove_collinear(vif_data, vif_threshold = 5)

  expect_true(length(vif_result$removed_features) >= 1L)
  expect_false("x2" %in% vif_result$removed_features &&
                 "x3" %in% vif_result$removed_features)
  expect_false(is.null(vif_result$vif))
  expect_error(remove_collinear(data, cor_threshold = 2), "cor_threshold")
  expect_error(remove_collinear(data, vif_threshold = 0.5), "vif_threshold")
  expect_error(remove_collinear(data.frame(x = "a")), "numeric")
  expect_error(remove_collinear(data.frame(x = c(1, NA))), "finite")
  expect_error(remove_collinear(data.frame(x = c(1, 1))), "zero-variance")
})

test_that("schema validates M5 selection thresholds", {
  base <- uniml_spec(task_type = "binary", outcome = "y", positive = "yes")

  expect_error(
    set_feature_selection(base, cor_threshold = 1.1),
    "selection\\$cor_threshold"
  )
  expect_error(
    set_feature_selection(base, vif_threshold = 0.9),
    "selection\\$vif_threshold"
  )
  expect_error(
    set_feature_selection(base, maxRuns = 10),
    "selection\\$maxRuns"
  )
  expect_error(
    set_feature_selection(base, methods = "unknown"),
    "selection\\$methods"
  )
})
