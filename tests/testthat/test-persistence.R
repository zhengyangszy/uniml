test_that("a result round-trips through save_result() and read_result()", {
  split <- make_workflow_split(make_workflow_data())
  spec <- make_workflow_spec()
  result <- run_workflow(spec, split$train, split$validation)

  path <- save_result(result, file.path(tempdir(), "uniml-roundtrip"))
  expect_true(file.exists(path))
  expect_true(grepl("\\.rds$", path))
  restored <- read_result(path)
  expect_s3_class(restored, "uniml_result")
  expect_identical(names(restored), names(result))
  expect_identical(restored$metrics, result$metrics)
})

test_that("a saved result still scores new data", {
  split <- make_workflow_split(make_workflow_data())
  spec <- make_workflow_spec()
  result <- run_workflow(spec, split$train, split$validation)
  path <- save_result(result, tempfile(fileext = ".rds"))
  restored <- read_result(path)

  scored <- predict_uniml(restored, split$validation)
  expect_s3_class(scored, "data.frame")
  expect_true(all(c("model", "prob", "truth") %in% names(scored)))
  expect_identical(nrow(scored), nrow(split$validation))
})

test_that("read_result() refuses what is not a result", {
  expect_error(read_result(tempfile(fileext = ".rds")), "No such file")
  path <- tempfile(fileext = ".rds")
  saveRDS(list(a = 1), path)
  expect_error(read_result(path), "does not contain")
})

test_that("save_result() validates its input", {
  expect_error(save_result(list(a = 1), tempfile()), "uniml_result")
  expect_error(
    save_result(assemble_result(metrics = data.frame(model = "a", auc = 0.8)), 1),
    "path"
  )
})

test_that("calc_metrics() works on the prediction table it produced", {
  split <- make_workflow_split(make_workflow_data())
  spec <- make_workflow_spec()
  result <- run_workflow(spec, split$train, split$validation)
  scored <- predict_uniml(result, split$validation)

  metrics <- calc_metrics(scored, positive = "yes")
  expect_identical(metrics$n_samples, nrow(scored))
  expect_true(is.finite(metrics$auc))
  expect_true(all(c("tp", "tn", "fp", "fn") %in% names(metrics)))

  # Subgroup analysis: the package deliberately does not define subgroups, so the
  # exported predictions are scored the same way for any subset.
  subgroup <- scored[seq_len(5), , drop = FALSE]
  sub_metrics <- calc_metrics(subgroup, positive = "yes")
  expect_identical(sub_metrics$n_samples, 5L)

  expect_error(calc_metrics(scored[, "model", drop = FALSE]), "prob")
  without_calibration <- scored[, setdiff(names(scored), "calibrated_prob"),
                                drop = FALSE]
  expect_error(
    calc_metrics(without_calibration, probability = "calibrated"),
    "calibrated_prob"
  )
})

test_that("the calibration numbers survive the export as metrics", {
  split <- make_workflow_split(make_workflow_data())
  spec <- make_workflow_spec()
  result <- run_workflow(spec, split$train, split$validation)
  metrics <- result$metrics[result$metrics$set == "validation", , drop = FALSE]
  expect_true(all(c("calib_slope", "calib_intercept", "calib_large") %in%
                    names(metrics)))
})
