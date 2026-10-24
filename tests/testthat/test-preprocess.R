make_preprocess_fixture <- function() {
  set.seed(42)
  data.frame(
    x = c(1.1, 2.4, NA, 4.8, 5.2, 6.6, 7.1, 8.9, 9.3, 10.4, 11.2, 12.8),
    z = c(10, 20, 30, NA, 50, 60, 70, 80, 90, 100, 110, 120),
    cat = factor(c("a", "b", "a", "c", NA, "b", "a", "c", "b", "a", "c", "b")),
    flag = factor(c("no", "yes", "no", "yes", "no", "yes", "no", "yes", NA, "no", "yes", "no")),
    ordinal = ordered(c("low", "mid", "high", "low", "mid", "high", "low", "mid",
                        "high", "low", "mid", "high"), levels = c("low", "mid", "high")),
    y = factor(c("no", "yes", "no", "yes", "no", "yes", "no", "yes", "no", "yes",
                 "no", "yes"), levels = c("no", "yes")),
    stringsAsFactors = FALSE
  )
}

test_that("detect_var_types() classifies feature columns", {
  data <- make_preprocess_fixture()
  types <- detect_var_types(data, outcome = "y")
  expect_identical(names(types), c("column", "type", "n_levels"))
  expect_identical(types$type[types$column == "x"], "continuous")
  expect_identical(types$type[types$column == "z"], "continuous")
  expect_identical(types$type[types$column == "cat"], "categorical")
  expect_identical(types$type[types$column == "flag"], "categorical")
  expect_identical(types$type[types$column == "ordinal"], "ordered")
  expect_false("y" %in% types$column)

  numeric_ordered <- data
  numeric_ordered$ordinal <- as.numeric(numeric_ordered$ordinal)
  types2 <- detect_var_types(numeric_ordered, outcome = "y", ordered_as = "ordinal")
  expect_identical(types2$type[types2$column == "ordinal"], "continuous")
  expect_error(detect_var_types("nope"), "data frame")
  expect_error(detect_var_types(data, outcome = 1), "outcome")
})

test_that("diagnose_missing() reports missing counts", {
  data <- make_preprocess_fixture()
  diagnostics <- diagnose_missing(data)
  expect_identical(diagnostics$per_column$column, names(data))
  expect_identical(
    diagnostics$per_column$n_missing[diagnostics$per_column$column == "x"],
    1L
  )
  expect_true(diagnostics$overall > 0)
  expect_true(diagnostics$n_incomplete >= 1L)
  expect_error(diagnose_missing(1), "data frame")
})

test_that("fit_preprocessor() imputes, scales, encodes, and balances in order", {
  data <- make_preprocess_fixture()
  pre <- fit_preprocessor(
    data, outcome = "y", impute = "median_mode", scale = "zscore",
    encode = "onehot", balance = "none"
  )
  expect_s3_class(pre, "uniml_preprocessor")
  expect_true(validate_uniml_preprocessor(pre))
  expect_identical(pre$order, c("impute", "scale", "encode", "balance"))
  expect_identical(pre$steps$impute$method, "median_mode")
  expect_identical(pre$steps$scale$method, "zscore")
  expect_identical(pre$steps$encode$method, "onehot")
  expect_identical(pre$steps$balance$method, "none")
  expect_false(anyNA(pre$train))
  expect_identical(rownames(pre$train), as.character(seq_len(nrow(data))))

  expect_identical(
    names(pre$train),
    c("x", "z", "y", "cat_a", "cat_b", "cat_c", "flag_no", "flag_yes",
      "ordinal_low", "ordinal_mid", "ordinal_high")
  )
  expect_equal(mean(pre$train$x), 0, tolerance = 1e-8)
  expect_equal(stats::sd(pre$train$x), 1, tolerance = 1e-8)
  expect_true(all(pre$train$cat_a %in% c(0L, 1L)))
  expect_identical(unclass(pre$train$y), unclass(data$y))

  printed <- capture.output(print(pre))
  expect_true(any(grepl("impute: median_mode", printed)))
})

test_that("fit_preprocessor() honours ordered_as and method none", {
  data <- make_preprocess_fixture()
  pre_none <- fit_preprocessor(data, outcome = "y", impute = "none", scale = "none",
                               encode = "none", balance = "none")
  expect_identical(names(pre_none$train), names(data))
  expect_equal(pre_none$train$x, data$x)
  expect_identical(pre_none$train$cat, data$cat)

  pre_numeric_ordered <- fit_preprocessor(
    data, outcome = "y", impute = "median_mode", scale = "zscore",
    encode = "onehot", balance = "none", ordered_as = "numeric"
  )
  expect_true(any(grepl("^ordinal$", names(pre_numeric_ordered$train))))
  expect_false(any(grepl("^ordinal_", names(pre_numeric_ordered$train))))
})

test_that("fit_preprocessor() drops all-NA columns with a warning", {
  data <- make_preprocess_fixture()
  data$all_na <- NA_real_
  expect_warning(
    pre <- fit_preprocessor(data, outcome = "y", impute = "median_mode",
                            scale = "none", encode = "none", balance = "none"),
    "all-NA"
  )
  expect_false("all_na" %in% names(pre$train))
  expect_identical(pre$steps$impute$params$dropped, "all_na")
  expect_error(
    fit_preprocessor(data.frame(x = 1:3), outcome = "missing"),
    "Outcome column"
  )
})

test_that("fit_imputer() supports mice, missForest, and missRanger", {
  data <- make_preprocess_fixture()
  skip_if_not_installed("mice")
  mice_step <- suppressWarnings(
    fit_imputer(data, method = "mice", outcome = "y", m = 2, max_iter = 2)
  )
  mice_data <- apply_imputer(mice_step, data)
  expect_identical(nrow(mice_data), nrow(data))
  expect_false(anyNA(mice_data))
  expect_s3_class(mice_step$params$mice, "mids")

  skip_if_not_installed("missForest")
  forest_step <- fit_imputer(data, method = "missForest", outcome = "y", max_iter = 2)
  forest_data <- apply_imputer(forest_step, data)
  expect_false(anyNA(forest_data))
  expect_s3_class(forest_step$params$missForest, "missForest")

  skip_if_not_installed("missRanger")
  ranger_step <- fit_imputer(data, method = "missRanger", outcome = "y", max_iter = 2)
  ranger_data <- apply_imputer(ranger_step, data)
  expect_false(anyNA(ranger_data))
  expect_error(fit_imputer(data, method = "unknown", outcome = "y"), "method")
})

test_that("impute_data() keeps the input class", {
  data <- make_preprocess_fixture()
  spec <- uniml_spec(task_type = "binary", outcome = "y", positive = "yes")
  udata <- as_uniml_data(data, spec)
  imputed <- impute_data(udata, method = "median_mode", outcome = "y")
  expect_s3_class(imputed, "uniml_data")
  expect_false(anyNA(imputed$data))
  expect_identical(imputed$outcome_name, "y")
  expect_identical(imputed$meta$n_dropped, 0L)
})

test_that("apply_imputer() projects onto new rows without touching observed values", {
  train <- data.frame(
    x = c(1, 2, NA, 4, 5, 6),
    flag = c(TRUE, FALSE, TRUE, NA, FALSE, TRUE),
    count = c(10L, 20L, 30L, NA, 50L, 60L),
    group = factor(c("a", "b", "a", NA, "b", "a"), levels = c("a", "b")),
    y = factor(c("no", "yes", "no", "yes", "no", "yes"), levels = c("no", "yes"))
  )
  # Fewer rows than the training data and missing values in every column.
  newdata <- data.frame(
    x = c(7, NA, 9),
    flag = c(FALSE, NA, TRUE),
    count = c(70L, NA, 90L),
    group = factor(c("b", NA, "a"), levels = c("a", "b")),
    y = factor(c("no", "yes", "no"), levels = c("no", "yes"))
  )

  for (method in c("median_mode", "mice")) {
    if (identical(method, "mice")) {
      skip_if_not_installed("mice")
    }
    step <- suppressWarnings(
      fit_imputer(train, method = method, outcome = "y", m = 2, max_iter = 2)
    )
    imputed <- apply_imputer(step, newdata)
    expect_false(anyNA(imputed))
    expect_identical(nrow(imputed), nrow(newdata))
    # Observed values are preserved exactly; only missing cells are filled.
    expect_identical(imputed$x[c(1, 3)], newdata$x[c(1, 3)])
    expect_identical(imputed$flag[c(1, 3)], newdata$flag[c(1, 3)])
    expect_identical(imputed$count[c(1, 3)], newdata$count[c(1, 3)])
    expect_identical(as.character(imputed$group[c(1, 3)]),
                     as.character(newdata$group[c(1, 3)]))
    # Column types are restored after imputation.
    expect_true(is.logical(imputed$flag))
    expect_true(is.integer(imputed$count))
    expect_true(is.factor(imputed$group))
    expect_identical(levels(imputed$group), levels(newdata$group))
    # Applied to the original training data, the observed cells also hold.
    original <- apply_imputer(step, train)
    observed <- !is.na(train$x)
    expect_identical(original$x[observed], train$x[observed])
  }
})

test_that(".apply_imputer_all() returns one completed set per imputation", {
  skip_if_not_installed("mice")
  data <- make_preprocess_fixture()
  step <- suppressWarnings(
    fit_imputer(data, method = "mice", outcome = "y", m = 3, max_iter = 2)
  )
  completed <- uniml:::.apply_imputer_all(step, data)
  expect_length(completed, 3L)
  for (frame in completed) {
    expect_identical(nrow(frame), nrow(data))
    expect_false(anyNA(frame))
  }

  single <- fit_imputer(data, method = "median_mode", outcome = "y")
  expect_length(uniml:::.apply_imputer_all(single, data), 1L)
})

test_that("apply_imputer() honours a stored imputation index", {
  skip_if_not_installed("mice")
  data <- make_preprocess_fixture()
  step <- suppressWarnings(
    fit_imputer(data, method = "mice", outcome = "y", m = 2, max_iter = 2)
  )
  first <- step
  first$params$imputation_index <- 1L
  second <- step
  second$params$imputation_index <- 2L

  imputed_first <- apply_imputer(first, data)
  imputed_second <- apply_imputer(second, data)

  expect_false(identical(imputed_first, imputed_second))
  completed_first <- mice::complete(step$params$mice, action = 1)
  completed_second <- mice::complete(step$params$mice, action = 2)
  expect_equal(imputed_first$x, completed_first$x)
  expect_equal(imputed_second$x, completed_second$x)
})

test_that("scaling parameters come from training data only", {
  data <- make_preprocess_fixture()
  pre <- fit_preprocessor(data, outcome = "y", impute = "median_mode",
                          scale = "zscore", encode = "none", balance = "none")
  train_scaled <- pre$train
  expect_equal(mean(train_scaled$x), 0, tolerance = 1e-8)

  newdata <- data[1:4, , drop = FALSE]
  newdata$x <- c(100, 200, 300, 400)
  transformed <- apply_preprocessor(pre, newdata)
  difference <- pre$steps$scale$params$differences$x
  expect_equal(transformed$x, (newdata$x - difference$center) / difference$scale)
  expect_true(all(transformed$x > 10))

  imputed <- impute_data(data, method = "median_mode", outcome = "y")
  constant <- imputed
  constant$z <- 5
  expect_warning(
    preconstant <- fit_scale(constant, method = "zscore", outcome = "y"),
    "zero-variance"
  )
  expect_identical(preconstant$params$skipped, "z")
  expect_false("z" %in% names(preconstant$params$differences))
  expect_error(fit_scale(data, method = "zscore", outcome = "y"), "missing values")
  expect_error(fit_scale(data, method = "zscore", vars = "nope"), "not found")
  expect_error(scale_numeric(data, method = "bad", outcome = "y"), "method")
})

test_that("scale_numeric() and encode_categorical() return transformed data", {
  data <- make_preprocess_fixture()
  imputed <- impute_data(data, method = "median_mode", outcome = "y")
  scaled <- scale_numeric(imputed, method = "minmax", outcome = "y")
  expect_true(all(scaled$x >= 0 & scaled$x <= 1))
  expect_true(all(scaled$z >= 0 & scaled$z <= 1))
  expect_identical(scaled$y, imputed$y)

  encoded <- encode_categorical(imputed, method = "onehot_drop", outcome = "y")
  expect_true("cat_b" %in% names(encoded))
  expect_false("cat_a" %in% names(encoded))
  expect_true(all(encoded$cat_b %in% c(0L, 1L)))

  dropped_all <- encode_categorical(imputed, method = "onehot_drop", outcome = "y")
  expect_identical(sort(names(dropped_all)), sort(c("x", "z", "flag_yes",
                                                    "ordinal_mid", "ordinal_high",
                                                    "cat_b", "cat_c", "y")))
})

test_that("encoding handles unseen levels with a warning", {
  train <- data.frame(
    x = factor(c("a", "b", "a", "c")),
    y = factor(c("no", "yes", "no", "yes"))
  )
  pre <- fit_preprocessor(train, outcome = "y", impute = "none", scale = "none",
                          encode = "onehot", balance = "none")
  newdata <- data.frame(
    x = factor(c("a", "z"), levels = c("a", "b", "c", "z")),
    y = factor(c("no", "yes"), levels = c("no", "yes"))
  )
  expect_warning(transformed <- apply_preprocessor(pre, newdata), "Unseen")
  expect_identical(unname(unlist(transformed[1, c("x_a", "x_b", "x_c")])),
                   c(1L, 0L, 0L))
  expect_identical(unname(unlist(transformed[2, c("x_a", "x_b", "x_c")])),
                   c(0L, 0L, 0L))
})

test_that("high-cardinality encoding warns", {
  train <- data.frame(
    x = factor(sprintf("l%02d", 1:12)),
    y = rep(factor(c("no", "yes")), 6)
  )
  expect_warning(
    fit_encode(train, outcome = "y", max_levels = 5),
    "High-cardinality"
  )
})

test_that("balance_classes() only affects the training data", {
  data <- data.frame(
    x = c(1, 2, 3, 4, 5, 6, 7, 8),
    z = c(1, 1, 2, 2, 3, 3, 4, 4),
    y = factor(c("no", "no", "no", "no", "no", "no", "yes", "yes"))
  )
  none <- balance_classes(data, method = "none", outcome = "y")
  expect_identical(none, data)

  step <- fit_balance(data, method = "none", outcome = "y")
  expect_identical(apply_balance(step, data), data)
  expect_identical(step$params$before[["no"]], 6L)

  skip_if_not_installed("smotefamily")
  smote_input <- data.frame(
    x = c(1, 2, 3, 4, 5, 6, 7, 8),
    z = c(1, 1, 2, 2, 3, 3, 4, 4),
    y = factor(c("no", "no", "no", "no", "yes", "yes", "yes", "yes"))
  )
  smote <- balance_classes(
    smote_input, method = "smote", outcome = "y", n_neighbors = 2, dup_size = 1
  )
  expect_true(nrow(smote) > nrow(smote_input))
  expect_true("yes" %in% levels(smote$y))

  one_class <- data
  one_class$y <- factor(rep("no", nrow(data)))
  expect_warning(
    skipped <- balance_classes(one_class, method = "rose", outcome = "y"),
    "skipped"
  )
  expect_identical(nrow(skipped), nrow(one_class))
  expect_error(fit_balance(data, method = "bad"), "method")
})

test_that("apply_step() dispatches on fitted steps", {
  data <- make_preprocess_fixture()
  imputer <- fit_imputer(data, method = "median_mode", outcome = "y")
  expect_false(anyNA(apply_step(imputer, data)))
  expect_error(apply_step(structure(list(), class = "uniml_step_unknown"), data),
               "no method")
})

test_that("uniml_spec integrates with preprocessing", {
  data <- make_preprocess_fixture()
  spec <- uniml_spec(task_type = "binary", outcome = "y", positive = "yes")
  spec <- set_preprocess(spec, impute = "mice", m = 3, scale = "minmax",
                         encode = "onehot_drop", balance = "none")
  expect_identical(spec$preprocess$impute, "mice")
  expect_identical(spec$preprocess$m, 3)
  pre <- suppressWarnings(fit_preprocessor(data, spec = spec))
  expect_identical(pre$steps$impute$method, "mice")
  expect_identical(pre$steps$scale$method, "minmax")
  expect_identical(pre$steps$encode$method, "onehot_drop")
  expect_false(anyNA(pre$train))

  expect_error(set_preprocess(spec, impute = "bad"), "preprocess\\$impute")
})
