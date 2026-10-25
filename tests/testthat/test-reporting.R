test_that("tripod_ai_check() audits an empty result", {
  result <- assemble_result()
  check <- tripod_ai_check(result)

  expect_s3_class(check, "data.frame")
  expect_identical(
    names(check),
    c("section", "domain", "requirement", "status", "evidence", "note")
  )
  expect_true(all(check$status %in% c(
    "reported", "partial", "not_reported", "not_supported"
  )))
  expect_true(any(check$status == "not_supported"))
  expect_true(any(check$status == "not_reported"))
  expect_false(any(check$status == "reported"))
})

test_that("tripod_ai_check() reports the stages that are present", {
  result <- assemble_result(
    data_info = list(
      outcome = "y",
      positive = "yes",
      n_train = 100L,
      n_validation = 30L,
      n_test = 0L,
      n_external = 40L
    ),
    preprocess = list(steps = list(
      impute = list(method = "mice"),
      scale = list(method = "zscore"),
      encode = list(method = "onehot")
    )),
    selection = list(selected_features = c("x1")),
    tuning = data.frame(model = "cart"),
    cv = list(summary = data.frame(model = "cart", auc = 0.8)),
    metrics = data.frame(
      model = c("cart", "cart"),
      set = c("validation", "external"),
      probability = c("raw", "raw"),
      auc = c(0.80, 0.79)
    ),
    ci = data.frame(metric = "auc", mean = 0.8),
    dca = data.frame(threshold = 0.2, net_benefit = 0.1),
    shap = list(objects = "shapviz", models = "cart"),
    models = list(cart = mlr3::lrn("classif.rpart", predict_type = "prob"))
  )

  status <- stats::setNames(
    as.character(tripod_ai_check(result)$status),
    tripod_ai_check(result)$domain
  )

  expect_identical(status[["Outcome"]], "reported")
  expect_identical(status[["Sample size"]], "reported")
  expect_identical(status[["Missing data"]], "reported")
  expect_identical(status[["External validation"]], "reported")
  expect_identical(status[["Discrimination"]], "reported")
  expect_identical(status[["Calibration"]], "not_reported")
  expect_identical(status[["Clinical utility"]], "reported")
  expect_identical(status[["Explainability"]], "reported")
  expect_identical(status[["Fairness and subgroups"]], "not_supported")
})

test_that("tripod_ai_check() writes the checklist to CSV", {
  result <- assemble_result()
  path <- tempfile(fileext = ".csv")
  check <- tripod_ai_check(result, file = path)

  expect_true(file.exists(path))
  csv <- utils::read.csv(path, fileEncoding = "UTF-8")
  expect_identical(nrow(csv), nrow(check))
  expect_true(all(c("status", "domain") %in% names(csv)))
})

test_that("tripod_ai_check() rejects objects that are not uniml results", {
  expect_error(tripod_ai_check(list(a = 1)), "uniml_result")
})
