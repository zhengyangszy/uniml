# L8 Partial dependence and ICE -----------------------------------------------
#
# SHAP dependence plots attribute credit; partial dependence (PD) and
# individual conditional expectation (ICE) curves answer a different, sharper
# question: what does the model predict when this feature is *forced* to a
# value, for the average patient and for every patient separately? The
# tutorial literature warns that PD alone can hide heterogeneous effects, so
# the two are drawn together.
#
# The implementation sits on `predict_uniml()`, so a patient pushed to a grid
# value goes through the same preprocessing, calibration, and deployment
# threshold a new patient would. Binary results explain the positive-class
# probability; multiclass results are supported with one PD/ICE pair per
# class.

.pd_grid <- function(values, grid) {
  unique_values <- sort(unique(values[!is.na(values)]))
  if (length(unique_values) <= grid) {
    return(unique_values)
  }
  as.numeric(stats::quantile(
    values[!is.na(values)],
    probs = seq(0, 1, length.out = grid),
    names = FALSE
  ))
}

.pd_predict <- function(result, newdata, model, grid_value, feature) {
  pushed <- newdata
  pushed[[feature]] <- grid_value
  prediction <- predict_uniml(result, newdata = pushed, model = model)
  prediction
}

#' Partial dependence and individual conditional expectation curves
#'
#' Forces one feature through a grid of values and records what the model
#' predicts - for the average patient (`pd`, the partial dependence curve) and
#' for every patient individually (`ice`). Both go through the model's full
#' deployment pipeline, so what you see is what a new patient would get.
#'
#' Draw them together: a flat PD curve can hide patients that swing up and
#' down (the ICE lines reveal them), and a PD curve with a kink may be driven
#' by a handful of patients.
#'
#' @param result A `uniml_result` from [run_workflow()].
#' @param newdata Data frame whose other columns define "the patients"; rows
#'   are used as they are.
#' @param feature The feature to sweep. Categorical features are swept over
#'   their levels.
#' @param model A model key, or `"best"`.
#' @param grid Number of grid points for a numeric feature (the observed
#'   quantiles); categorical features use every level.
#'
#' @return A list with `ice` (data frame: `value`, `patient`, `prob`, and for
#'   multiclass one `prob_<class>` column), `pd` (data frame: `value`, `prob`),
#'   `feature`, and `class` (NULL for binary results).
#' @references
#' Goldstein A, Kapelner A, Bleich J, Pitkin E (2015). Peeking inside the black
#' box: visualizing statistical learning with plots of individual conditional
#' expectation. *Journal of Computational and Graphical Statistics*, 24(1),
#' 44-65. doi:10.1080/10618600.2014.907095
#'
#' Friedman JH (2001). Greedy function approximation: a gradient boosting
#' machine. *Annals of Statistics*, 29(5), 1189-1232.
#' doi:10.1214/aos/1013203451
#' @export
#' @examples
#' \dontrun{
#' result <- run_workflow(spec, train, validation)
#' curves <- pd_ice(result, validation, feature = "age", grid = 15)
#' plot_pd_ice(curves)
#' }
pd_ice <- function(result, newdata, feature, model = "best", grid = 20L) {
  if (!is.data.frame(newdata)) {
    .abort("`newdata` must be a data frame.")
  }
  if (!feature %in% names(newdata)) {
    .abort(sprintf("`newdata` does not contain the column \"%s\".", feature))
  }
  .assert_scalar_number(grid, "grid", integer = TRUE, min = 2)
  key <- if (identical(model, "best")) {
    result$ranking$model[[which.min(result$ranking$rank)]]
  } else {
    model
  }
  if (is.null(result$models[[key]])) {
    .abort(sprintf("Unknown model \"%s\".", key))
  }
  task_type <- result$meta$spec$task_type
  positive <- result$meta$spec$positive
  values <- newdata[[feature]]
  grid_values <- if (is.numeric(values) && !is.factor(values)) {
    .pd_grid(values, grid)
  } else {
    sort(unique(as.character(stats::na.omit(values))))
  }
  if (length(grid_values) == 0L) {
    .abort("The feature has no observed values to sweep.")
  }

  ice_rows <- vector("list", length(grid_values))
  pd_rows <- vector("list", length(grid_values))
  for (index in seq_along(grid_values)) {
    value <- grid_values[[index]]
    prediction <- .pd_predict(result, newdata, key, value, feature)
    prob <- prediction$prob
    if (identical(task_type, "binary")) {
      # `predict_uniml()` returns the positive-class probability as a numeric
      # vector for binary results.
      ice_rows[[index]] <- data.frame(
        value = value,
        patient = seq_len(nrow(newdata)),
        prob = as.numeric(prob),
        stringsAsFactors = FALSE
      )
      pd_rows[[index]] <- data.frame(
        value = value,
        prob = mean(as.numeric(prob)),
        stringsAsFactors = FALSE
      )
    } else {
      classes <- colnames(prob)
      ice_frame <- data.frame(
        value = value,
        patient = seq_len(nrow(newdata)),
        stringsAsFactors = FALSE
      )
      for (class_name in classes) {
        ice_frame[[paste0("prob_", class_name)]] <-
          as.numeric(prob[[class_name]])
      }
      ice_rows[[index]] <- ice_frame
      pd_rows[[index]] <- data.frame(
        value = value,
        stringsAsFactors = FALSE
      )
      for (class_name in classes) {
        pd_rows[[index]][[paste0("prob_", class_name)]] <-
          mean(as.numeric(prob[[class_name]]))
      }
    }
  }
  ice <- do.call(rbind, ice_rows)
  pd <- do.call(rbind, pd_rows)
  rownames(ice) <- NULL
  rownames(pd) <- NULL
  list(
    ice = ice,
    pd = pd,
    feature = feature,
    class = if (identical(task_type, "binary")) positive else NULL
  )
}

#' Plot partial dependence with ICE curves
#'
#' Thin lines for every patient (ICE), one bold line for the average (PD).
#' Where they disagree, the average is hiding someone.
#'
#' @param curves The list returned by [pd_ice()].
#' @param prob_columns For multiclass results, which `prob_<class>` columns to
#'   draw; NULL draws every class. Ignored for binary results.
#' @param palette Discrete palette: a `ggsci` name, a colour, or a vector.
#' @param theme A `ggplot2` theme.
#' @param title,subtitle,caption Optional figure labels.
#'
#' @return A `ggplot` object.
#' @references
#' Goldstein A, Kapelner A, Bleich J, Pitkin E (2015). Peeking inside the black
#' box: visualizing statistical learning with plots of individual conditional
#' expectation. *Journal of Computational and Graphical Statistics*, 24(1),
#' 44-65. doi:10.1080/10618600.2014.907095
#' @export
#' @examples
#' \dontrun{
#' curves <- pd_ice(result, validation, feature = "age")
#' plot_pd_ice(curves)
#' }
plot_pd_ice <- function(curves, prob_columns = NULL, palette = "bmj",
                        theme = theme_uniml(), title = NULL, subtitle = NULL,
                        caption = NULL) {
  if (!is.list(curves) || is.null(curves$ice) || is.null(curves$pd)) {
    .abort("`curves` must be the list returned by `pd_ice()`.")
  }
  ice <- curves$ice
  pd <- curves$pd
  prob_columns <- if (is.null(prob_columns)) {
    setdiff(names(ice), c("value", "patient"))
  } else {
    prob_columns
  }
  colours <- .uniml_plot_palette(length(prob_columns), palette = palette)
  names(colours) <- vapply(prob_columns, function(column) {
    if (!is.null(curves$class)) {
      curves$class
    } else {
      sub("^prob_", "", column)
    }
  }, character(1))
  graph <- ggplot2::ggplot()
  for (index in seq_along(prob_columns)) {
    column <- prob_columns[[index]]
    class_name <- sub("^prob_", "", column)
    graph <- graph +
      ggplot2::geom_line(
        data = ice,
        ggplot2::aes(
          x = .data$value, y = .data[[column]],
          group = .data$patient
        ),
        colour = "grey65",
        alpha = 0.35,
        linewidth = 0.3
      ) +
      ggplot2::geom_line(
        data = pd,
        ggplot2::aes(x = .data$value, y = .data[[column]],
                     colour = class_name),
        linewidth = 1.1
      )
  }
  graph +
    ggplot2::scale_colour_manual(values = colours) +
    ggplot2::labs(
      x = curves$feature,
      y = if (is.null(curves$class)) {
        "Predicted probability"
      } else {
        sprintf("Predicted P(%s)", curves$class)
      },
      colour = NULL,
      title = if (is.null(title)) {
        sprintf("Partial dependence and ICE - %s", curves$feature)
      } else {
        title
      }
    ) +
    theme
  .uniml_style_plot(graph, theme, title, subtitle, caption)
}
