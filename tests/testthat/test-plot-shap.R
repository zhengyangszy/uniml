make_shapviz_fixture <- function() {
  shapviz::shapviz(
    matrix(
      c(
        0.40, -0.20, 0.10,
        0.30, -0.50, 0.20,
        0.60, 0.10, -0.30,
        0.20, 0.50, -0.10,
        0.35, 0.15, 0.05,
        0.10, 0.25, 0.40
      ),
      nrow = 6L,
      dimnames = list(NULL, c("x1", "x2", "x3"))
    ),
    data.frame(
      x1 = c(1, 2, 3, 4, 5, 6),
      x2 = c(0, 1, 0, 1, 0, 1),
      x3 = c(2, 4, 6, 8, 10, 12)
    )
  )
}

make_shap_result <- function() {
  result <- assemble_result(
    data_info = list(n = 6L),
    metrics = data.frame(
      model = "lr", set = "validation", auc = 0.80,
      stringsAsFactors = FALSE
    )
  )
  result$shap <- list(objects = make_shapviz_fixture())
  result
}

test_that("SHAP plot helpers dispatch on shapviz objects", {
  skip_if_not_installed("shapviz")
  sv <- make_shapviz_fixture()

  importance <- plot_shap_importance(sv)
  expect_true(inherits(importance, "ggplot"))
  expect_true(inherits(plot_shap_beeswarm(sv), "ggplot"))
  expect_true(inherits(plot_shap_dependence(sv), "ggplot"))
  expect_true(inherits(plot_shap_dependence(sv, feature = "x3"), "ggplot"))
})

test_that("SHAP plot helpers use stored objects from uniml results", {
  skip_if_not_installed("shapviz")
  result <- make_shap_result()

  expect_true(inherits(plot_shap_importance(result), "ggplot"))
  expect_true(inherits(plot_shap_beeswarm(result), "ggplot"))
  expect_true(inherits(plot_shap_force(result), "ggplot"))
  dependence <- plot_shap_dependence(result)
  expect_true(inherits(dependence, "ggplot"))
  expect_identical(ggplot2::get_labs(dependence)$x, "x2")
  importance_plot <- plot(result, type = "shap_importance")
  expect_true(inherits(importance_plot, "ggplot"))
  expect_true(inherits(plot(result, type = "shap_force"), "ggplot"))
})

test_that("SHAP plotting handles mshapviz objects and model selection", {
  skip_if_not_installed("shapviz")
  sv <- make_shapviz_fixture()
  ms <- shapviz::mshapviz(list(lr = sv, rf = sv))
  result <- make_shap_result()
  result$shap$objects <- ms

  expect_true(inherits(plot_shap_importance(result, model = "lr"), "ggplot"))
  expect_true(inherits(plot_shap_importance(result, model = "all"), "patchwork"))
  expect_error(plot_shap_importance(result, model = "xgb"), "Unknown SHAP model")
})

test_that("SHAP plotting reports missing or unsupported objects", {
  result <- assemble_result(data_info = list(n = 1L))
  expect_error(plot_shap_importance(result), "No SHAP objects")
  expect_error(
    plot_shap_importance(structure(list(), class = c("uniml_binary", "uniml_prediction"))),
    "SHAP"
  )
  expect_error(plot_shap_importance(structure(list(), class = "uniml_other")), "no method")
  expect_error(plot_shap_importance(NULL), "no method")
})

test_that("plot.uniml_result() validates the SHAP plot type", {
  result <- assemble_result(data_info = list(n = 1L))
  expect_error(plot(result, type = "shap_missing"), "type")
})
