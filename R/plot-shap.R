# L7 SHAP plots --------------------------------------------------------------
#
# SHAP figures are thin, reproducible wrappers around the `shapviz` plotting
# functions. A `uniml_result` stores the fitted `shapviz` (or `mshapviz`)
# objects in `result$shap$objects`; the atomic plotting functions also accept a
# bare `shapviz`/`mshapviz` object directly.

.shap_available <- function() {
  requireNamespace("shapviz", quietly = TRUE)
}

.is_shap_object <- function(x) {
  inherits(x, "shapviz") || inherits(x, "mshapviz")
}

.shap_view_source <- function(x) {
  if (inherits(x, "shapviz") || inherits(x, "mshapviz")) {
    return(list(objects = x, class = class(x)))
  }
  if (!inherits(x, "uniml_result")) {
    .abort("SHAP plots require a `shapviz`/`mshapviz` object or a `uniml_result`.")
  }
  validate_uniml_result(x)
  objects <- x$shap$objects
  if (is.null(objects)) {
    source_class <- x$meta$source_class
    .abort(sprintf(
      "No SHAP objects are stored in this result%s.",
      if (is.null(source_class)) "" else
        sprintf(" (source class \"%s\")", source_class)
    ))
  }
  list(objects = objects, class = class(objects))
}

.shap_is_mshapviz <- function(objects) {
  inherits(objects, "mshapviz")
}

.shap_is_shapviz <- function(objects) {
  inherits(objects, "shapviz")
}

.shap_values_matrix <- function(object) {
  values <- tryCatch(shapviz::get_shap_values(object), error = function(e) NULL)
  if (is.null(values) || length(dim(values)) != 2L ||
      nrow(values) == 0L || ncol(values) == 0L) {
    .abort("The stored SHAP object has no usable SHAP value matrix.")
  }
  values
}

.shap_models <- function(objects) {
  if (.shap_is_mshapviz(objects)) {
    return(names(objects))
  }
  NULL
}

.shap_pick <- function(objects, model = NULL) {
  if (.shap_is_shapviz(objects)) {
    return(objects)
  }
  if (!.shap_is_mshapviz(objects)) {
    .abort(sprintf(
      "Unsupported SHAP object of class \"%s\"; expected `shapviz` or `mshapviz`.",
      paste(class(objects), collapse = "/")
    ))
  }
  model_names <- names(objects)
  if (length(model_names) == 0L) {
    .abort("The stored `mshapviz` object has no named models.")
  }
  if (is.null(model) || identical(model, "best")) {
    return(objects[[1L]])
  }
  if (identical(model, "all")) {
    return(NULL)
  }
  .assert_scalar_character(model, "model")
  if (!model %in% model_names) {
    .abort(sprintf(
      "Unknown SHAP model \"%s\"; available: %s.",
      model,
      paste(model_names, collapse = ", ")
    ))
  }
  objects[[model]]
}

.shap_mshapviz_plot <- function(objects, fun, ..., title = TRUE) {
  model_names <- names(objects)
  plots <- lapply(model_names, function(name) {
    object <- objects[[name]]
    if (!.shap_is_shapviz(object)) {
      return(NULL)
    }
    plot <- fun(object, ...)
    if (isTRUE(title)) {
      plot <- plot + ggplot2::ggtitle(name)
    }
    plot
  })
  plots <- plots[!vapply(plots, is.null, logical(1))]
  if (length(plots) == 0L) {
    .abort("The stored `mshapviz` object contains no usable `shapviz` entries.")
  }
  if (requireNamespace("patchwork", quietly = TRUE)) {
    return(patchwork::wrap_plots(plots, ncol = 1L))
  }
  plots[[1L]]
}

.shap_feature_names <- function(object) {
  names <- tryCatch(colnames(object), error = function(e) NULL)
  if (is.null(names) || length(names) == 0L) {
    names <- tryCatch(
      colnames(object$S),
      error = function(e) NULL
    )
  }
  if (is.null(names) || length(names) == 0L) {
    .abort("Could not determine the SHAP feature names.")
  }
  as.character(names)
}

.shap_importance_scores <- function(object) {
  values <- .shap_values_matrix(object)
  scores <- colMeans(abs(as.matrix(values)), na.rm = TRUE)
  scores[is.na(scores)] <- 0
  scores
}

.default_shap_feature <- function(object) {
  scores <- .shap_importance_scores(object)
  names(scores)[which.max(scores)]
}

.require_shapviz <- function() {
  if (!.shap_available()) {
    .abort("Package \"shapviz\" is required for SHAP plots.")
  }
  invisible(TRUE)
}

#' Plot SHAP importance
#'
#' Draws a SHAP importance plot through `shapviz::sv_importance()`. A stored
#' `mshapviz` object is drawn one panel per model unless a single `model` is
#' requested.
#'
#' @param x A `uniml_result`, `shapviz`, or `mshapviz` object.
#' @param model For `mshapviz` results, `"best"`, `"all"`, or a model name.
#' @param kind Importance style passed to `shapviz::sv_importance()`.
#' @param max_display Maximum number of features to display.
#' @param palette Discrete palette name used by the shared plot theme.
#' @param theme A `ggplot2` theme applied to the figure.
#' @param title,subtitle,caption Optional figure labels.
#' @param ... Additional arguments passed to `shapviz::sv_importance()`.
#'
#' @return A `ggplot` or `patchwork` object.
#' @export
#' @rdname plot_shap_importance
#' @examples
#' \dontrun{
#' plot_shap_importance(result)
#' }
plot_shap_importance.uniml_result <- function(x, model = "best", kind = "bar",
                                              max_display = 15L,
                                              palette = "bmj",
                                              theme = theme_uniml(),
                                              title = NULL, subtitle = NULL,
                                              caption = NULL, ...) {
  .require_shapviz()
  .assert_scalar_character(model, "model")
  .assert_scalar_character(kind, "kind")
  .assert_scalar_number(max_display, "max_display", integer = TRUE, min = 1)
  source <- .shap_view_source(x)
  objects <- source$objects
  picked <- if (.shap_is_shapviz(objects)) NULL else .shap_pick(objects, model = model)
  plot <- if (.shap_is_shapviz(objects)) {
    shapviz::sv_importance(
      objects, kind = kind, max_display = max_display, ...
    )
  } else if (!is.null(picked)) {
    shapviz::sv_importance(
      picked, kind = kind, max_display = max_display, ...
    )
  } else {
    .shap_mshapviz_plot(
      objects,
      shapviz::sv_importance,
      kind = kind,
      max_display = max_display,
      ...
    )
  }
  .uniml_style_plot(plot, theme, title, subtitle, caption)
}

#' @rdname plot_shap_importance
#' @export
plot_shap_importance.uniml_binary <- function(x, model = "best", kind = "bar",
                                              max_display = 15L,
                                              palette = "bmj", ...) {
  if (inherits(x, "shapviz") || inherits(x, "mshapviz")) {
    return(plot_shap_importance.uniml_result(
      x, model = model, kind = kind, max_display = max_display,
      palette = palette, ...
    ))
  }
  plot_shap_importance.uniml_result(
    .as_uniml_result(x, source_class = "uniml_binary"),
    model = model,
    kind = kind,
    max_display = max_display,
    palette = palette,
    ...
  )
}

#' @rdname plot_shap_importance
#' @export
plot_shap_importance.shapviz <- function(x, model = "best", kind = "bar",
                                         max_display = 15L,
                                         palette = "bmj", ...) {
  plot_shap_importance.uniml_result(
    x, model = model, kind = kind, max_display = max_display,
    palette = palette, ...
  )
}

#' @rdname plot_shap_importance
#' @export
plot_shap_importance.mshapviz <- function(x, model = "best", kind = "bar",
                                          max_display = 15L,
                                          palette = "bmj", ...) {
  plot_shap_importance.uniml_result(
    x, model = model, kind = kind, max_display = max_display,
    palette = palette, ...
  )
}

#' Plot a SHAP beeswarm
#'
#' Draws a SHAP beeswarm through `shapviz::sv_importance(kind = "beeswarm")`.
#'
#' @param x A `uniml_result`, `shapviz`, or `mshapviz` object.
#' @param model For `mshapviz` results, `"best"`, `"all"`, or a model name.
#' @param max_display Maximum number of features to display.
#' @param palette Discrete palette name used by the shared plot theme.
#' @param theme A `ggplot2` theme applied to the figure.
#' @param title,subtitle,caption Optional figure labels.
#' @param ... Additional arguments passed to `shapviz::sv_importance()`.
#'
#' @return A `ggplot` or `patchwork` object.
#' @export
#' @rdname plot_shap_beeswarm
#' @examples
#' \dontrun{
#' plot_shap_beeswarm(result)
#' }
plot_shap_beeswarm.uniml_result <- function(x, model = "best", max_display = 15L,
                                            palette = "bmj",
                                            theme = theme_uniml(),
                                            title = NULL, subtitle = NULL,
                                            caption = NULL, ...) {
  .require_shapviz()
  .assert_scalar_character(model, "model")
  .assert_scalar_number(max_display, "max_display", integer = TRUE, min = 1)
  source <- .shap_view_source(x)
  objects <- source$objects
  picked <- if (.shap_is_shapviz(objects)) NULL else .shap_pick(objects, model = model)
  plot <- if (.shap_is_shapviz(objects)) {
    shapviz::sv_importance(
      objects, kind = "beeswarm", max_display = max_display, ...
    )
  } else if (!is.null(picked)) {
    shapviz::sv_importance(
      picked, kind = "beeswarm", max_display = max_display, ...
    )
  } else {
    .shap_mshapviz_plot(
      objects,
      shapviz::sv_importance,
      kind = "beeswarm",
      max_display = max_display,
      ...
    )
  }
  .uniml_style_plot(plot, theme, title, subtitle, caption)
}

#' @rdname plot_shap_beeswarm
#' @export
plot_shap_beeswarm.uniml_binary <- function(x, model = "best",
                                            max_display = 15L,
                                            palette = "bmj", ...) {
  if (inherits(x, "shapviz") || inherits(x, "mshapviz")) {
    return(plot_shap_beeswarm.uniml_result(
      x, model = model, max_display = max_display, palette = palette, ...
    ))
  }
  plot_shap_beeswarm.uniml_result(
    .as_uniml_result(x, source_class = "uniml_binary"),
    model = model,
    max_display = max_display,
    palette = palette,
    ...
  )
}

#' @rdname plot_shap_beeswarm
#' @export
plot_shap_beeswarm.shapviz <- function(x, model = "best",
                                       max_display = 15L,
                                       palette = "bmj", ...) {
  plot_shap_beeswarm.uniml_result(
    x, model = model, max_display = max_display, palette = palette, ...
  )
}

#' @rdname plot_shap_beeswarm
#' @export
plot_shap_beeswarm.mshapviz <- function(x, model = "best",
                                        max_display = 15L,
                                        palette = "bmj", ...) {
  plot_shap_beeswarm.uniml_result(
    x, model = model, max_display = max_display, palette = palette, ...
  )
}

#' Plot a SHAP dependence curve
#'
#' When `feature` is `NULL`, the feature with the largest mean absolute SHAP
#' value is used.
#'
#' @param x A `uniml_result`, `shapviz`, or `mshapviz` object.
#' @param feature Feature to draw on the x-axis; `NULL` picks the most
#'   important feature.
#' @param model For `mshapviz` results, `"best"`, `"all"`, or a model name.
#' @param color_by Optional feature used to colour the points.
#' @param palette Discrete palette name used by the shared plot theme.
#' @param theme A `ggplot2` theme applied to the figure.
#' @param title,subtitle,caption Optional figure labels.
#' @param ... Additional arguments passed to `shapviz::sv_dependence()`.
#'
#' @return A `ggplot` or `patchwork` object.
#' @export
#' @rdname plot_shap_dependence
#' @examples
#' \dontrun{
#' plot_shap_dependence(result)
#' }
plot_shap_dependence.uniml_result <- function(x, feature = NULL, model = "best",
                                              color_by = NULL,
                                              palette = "bmj",
                                              theme = theme_uniml(),
                                              title = NULL, subtitle = NULL,
                                              caption = NULL, ...) {
  .require_shapviz()
  .assert_scalar_character(model, "model")
  source <- .shap_view_source(x)
  objects <- source$objects
  if (.shap_is_shapviz(objects)) {
    if (is.null(feature)) {
      feature <- .default_shap_feature(objects)
    }
    .assert_scalar_character(feature, "feature")
    plot <- shapviz::sv_dependence(
      objects, v = feature, color = color_by, ...
    )
    return(.uniml_style_plot(plot, theme, title, subtitle, caption))
  }
  picked <- .shap_pick(objects, model = model)
  if (!is.null(picked)) {
    if (is.null(feature)) {
      feature <- .default_shap_feature(picked)
    }
    .assert_scalar_character(feature, "feature")
    plot <- shapviz::sv_dependence(
      picked, v = feature, color = color_by, ...
    )
    return(.uniml_style_plot(plot, theme, title, subtitle, caption))
  }
  features <- .shap_feature_names(objects[[1L]])
  if (is.null(feature)) {
    feature <- features[[which.max(
      .shap_importance_scores(objects[[1L]])
    )]]
  }
  .assert_scalar_character(feature, "feature")
  plot <- .shap_mshapviz_plot(
    objects,
    shapviz::sv_dependence,
    v = feature,
    color = color_by,
    ...
  )
  .uniml_style_plot(plot, theme, title, subtitle, caption)
}

#' @rdname plot_shap_dependence
#' @export
plot_shap_dependence.uniml_binary <- function(x, feature = NULL, model = "best",
                                              color_by = NULL,
                                              palette = "bmj", ...) {
  if (inherits(x, "shapviz") || inherits(x, "mshapviz")) {
    return(plot_shap_dependence.uniml_result(
      x, feature = feature, model = model, color_by = color_by,
      palette = palette, ...
    ))
  }
  plot_shap_dependence.uniml_result(
    .as_uniml_result(x, source_class = "uniml_binary"),
    feature = feature,
    model = model,
    color_by = color_by,
    palette = palette,
    ...
  )
}

#' @rdname plot_shap_dependence
#' @export
plot_shap_dependence.shapviz <- function(x, feature = NULL, model = "best",
                                         color_by = NULL,
                                         palette = "bmj", ...) {
  plot_shap_dependence.uniml_result(
    x, feature = feature, model = model, color_by = color_by,
    palette = palette, ...
  )
}

#' @rdname plot_shap_dependence
#' @export
plot_shap_dependence.mshapviz <- function(x, feature = NULL, model = "best",
                                          color_by = NULL,
                                          palette = "bmj", ...) {
  plot_shap_dependence.uniml_result(
    x, feature = feature, model = model, color_by = color_by,
    palette = palette, ...
  )
}

#' Plot a single-observation SHAP waterfall
#'
#' Draws the standard SHAP waterfall for one observation through
#' `shapviz::sv_waterfall()`.
#'
#' @param x A `uniml_result`, `shapviz`, or `mshapviz` object.
#' @param model For `mshapviz` results, `"best"`, `"all"`, or a model name.
#' @param row_id Observation to explain.
#' @param max_display Maximum number of features to display.
#' @param palette Discrete palette name used by the shared plot theme.
#' @param theme A `ggplot2` theme applied to the figure.
#' @param title,subtitle,caption Optional figure labels.
#' @param ... Additional arguments passed to `shapviz::sv_waterfall()`.
#'
#' @return A `ggplot` or `patchwork` object.
#' @export
#' @rdname plot_shap_waterfall
#' @examples
#' \dontrun{
#' plot_shap_waterfall(result, row_id = 1)
#' }
plot_shap_waterfall.uniml_result <- function(x, model = "best", row_id = 1L,
                                             max_display = 10L,
                                             palette = "bmj",
                                             theme = theme_uniml(),
                                             title = NULL, subtitle = NULL,
                                             caption = NULL, ...) {
  .require_shapviz()
  .assert_scalar_character(model, "model")
  .assert_scalar_number(row_id, "row_id", integer = TRUE, min = 1)
  .assert_scalar_number(max_display, "max_display", integer = TRUE, min = 1)
  source <- .shap_view_source(x)
  objects <- source$objects
  picked <- if (.shap_is_shapviz(objects)) NULL else .shap_pick(objects, model = model)
  plot <- if (.shap_is_shapviz(objects)) {
    shapviz::sv_waterfall(objects, row_id = row_id, max_display = max_display, ...)
  } else if (!is.null(picked)) {
    shapviz::sv_waterfall(picked, row_id = row_id, max_display = max_display, ...)
  } else {
    .shap_mshapviz_plot(
      objects,
      shapviz::sv_waterfall,
      row_id = row_id,
      max_display = max_display,
      ...
    )
  }
  .uniml_style_plot(plot, theme, title, subtitle, caption)
}

#' @rdname plot_shap_waterfall
#' @export
plot_shap_waterfall.uniml_binary <- function(x, model = "best", row_id = 1L,
                                             max_display = 10L,
                                             palette = "bmj", ...) {
  if (inherits(x, "shapviz") || inherits(x, "mshapviz")) {
    return(plot_shap_waterfall.uniml_result(
      x, model = model, row_id = row_id, max_display = max_display,
      palette = palette, ...
    ))
  }
  plot_shap_waterfall.uniml_result(
    .as_uniml_result(x, source_class = "uniml_binary"),
    model = model,
    row_id = row_id,
    max_display = max_display,
    palette = palette,
    ...
  )
}

#' @rdname plot_shap_waterfall
#' @export
plot_shap_waterfall.shapviz <- function(x, model = "best", row_id = 1L,
                                        max_display = 10L,
                                        palette = "bmj", ...) {
  plot_shap_waterfall.uniml_result(
    x, model = model, row_id = row_id, max_display = max_display,
    palette = palette, ...
  )
}

#' @rdname plot_shap_waterfall
#' @export
plot_shap_waterfall.mshapviz <- function(x, model = "best", row_id = 1L,
                                         max_display = 10L,
                                         palette = "bmj", ...) {
  plot_shap_waterfall.uniml_result(
    x, model = model, row_id = row_id, max_display = max_display,
    palette = palette, ...
  )
}

#' Plot a single-observation SHAP force plot
#'
#' Draws a force plot for one row through `shapviz::sv_force()`. A stored
#' `mshapviz` object is drawn one panel per model unless a single `model` is
#' requested.
#'
#' @param x A `uniml_result`, `shapviz`, or `mshapviz` object.
#' @param model For `mshapviz` results, `"best"`, `"all"`, or a model name.
#' @param row_id Observation (row) to display.
#' @param max_display Maximum number of features to display.
#' @param palette Discrete palette name used by the shared plot theme.
#' @param theme A `ggplot2` theme applied to the figure.
#' @param title,subtitle,caption Optional figure labels.
#' @param ... Additional arguments passed to `shapviz::sv_force()`.
#'
#' @return A `ggplot` or `patchwork` object.
#' @export
#' @rdname plot_shap_force
#' @examples
#' \dontrun{
#' plot_shap_force(result)
#' }
plot_shap_force.uniml_result <- function(x, model = "best", row_id = 1L,
                                         max_display = 6L,
                                         palette = "bmj",
                                         theme = theme_uniml(),
                                         title = NULL, subtitle = NULL,
                                         caption = NULL, ...) {
  .require_shapviz()
  .assert_scalar_character(model, "model")
  .assert_scalar_number(row_id, "row_id", integer = TRUE, min = 1)
  .assert_scalar_number(max_display, "max_display", integer = TRUE, min = 1)
  source <- .shap_view_source(x)
  objects <- source$objects
  picked <- if (.shap_is_shapviz(objects)) NULL else .shap_pick(objects, model = model)
  plot <- if (.shap_is_shapviz(objects)) {
    shapviz::sv_force(objects, row_id = row_id, max_display = max_display, ...)
  } else if (!is.null(picked)) {
    shapviz::sv_force(picked, row_id = row_id, max_display = max_display, ...)
  } else {
    .shap_mshapviz_plot(
      objects,
      shapviz::sv_force,
      row_id = row_id,
      max_display = max_display,
      ...
    )
  }
  .uniml_style_plot(plot, theme, title, subtitle, caption)
}

#' @rdname plot_shap_force
#' @export
plot_shap_force.uniml_binary <- function(x, model = "best", row_id = 1L,
                                         max_display = 6L,
                                         palette = "bmj", ...) {
  if (inherits(x, "shapviz") || inherits(x, "mshapviz")) {
    return(plot_shap_force.uniml_result(
      x, model = model, row_id = row_id, max_display = max_display,
      palette = palette, ...
    ))
  }
  plot_shap_force.uniml_result(
    .as_uniml_result(x, source_class = "uniml_binary"),
    model = model,
    row_id = row_id,
    max_display = max_display,
    palette = palette,
    ...
  )
}

#' @rdname plot_shap_force
#' @export
plot_shap_force.shapviz <- function(x, model = "best", row_id = 1L,
                                    max_display = 6L,
                                    palette = "bmj", ...) {
  plot_shap_force.uniml_result(
    x, model = model, row_id = row_id, max_display = max_display,
    palette = palette, ...
  )
}

#' @rdname plot_shap_force
#' @export
plot_shap_force.mshapviz <- function(x, model = "best", row_id = 1L,
                                     max_display = 6L,
                                     palette = "bmj", ...) {
  plot_shap_force.uniml_result(
    x, model = model, row_id = row_id, max_display = max_display,
    palette = palette, ...
  )
}
