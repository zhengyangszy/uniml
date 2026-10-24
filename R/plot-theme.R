# L7 shared plotting theme ---------------------------------------------------

.uniml_plot_palette <- function(n, palette = "bmj") {
  .assert_scalar_number(n, "n", integer = TRUE, min = 1)
  # A colour vector (or a single colour such as "#2A6EBB") is used as supplied.
  if (length(palette) > 1L) {
    values <- rep(as.character(palette), length.out = n)
    return(values[seq_len(n)])
  }
  .assert_scalar_character(palette, "palette")
  if (requireNamespace("ggsci", quietly = TRUE)) {
    ggsci_names <- sub(
      "^pal_", "",
      grep("^pal_", getNamespaceExports("ggsci"), value = TRUE)
    )
    if (palette %in% ggsci_names) {
      generator <- get(paste0("pal_", palette), envir = asNamespace("ggsci"))
      values <- tryCatch(generator("default")(max(n, 1L)), error = function(e) NULL)
      if (!is.null(values)) {
        return(values[seq_len(n)])
      }
    }
  }
  is_colour <- tryCatch(
    {
      grDevices::col2rgb(palette)
      TRUE
    },
    error = function(e) FALSE
  )
  if (is_colour) {
    return(rep(palette, length.out = n))
  }
  fallback <- c(
    "#2A6EBB", "#F0AB00", "#C50084", "#7D5CC6",
    "#E37222", "#2E8B57", "#B22222", "#008B8B"
  )
  if (n <= length(fallback)) {
    return(fallback[seq_len(n)])
  }
  grDevices::colorRampPalette(fallback)(n)
}

# Apply a user theme and optional labels to a finished figure. Patchwork
# objects use `&` so the theme reaches every panel.
.uniml_style_plot <- function(plot, theme = NULL, title = NULL, subtitle = NULL,
                              caption = NULL) {
  if (!is.null(theme) && !inherits(theme, "theme")) {
    .abort("`theme` must be a ggplot2 theme object.")
  }
  has_labels <- !is.null(title) || !is.null(subtitle) || !is.null(caption)
  if (isTRUE(has_labels)) {
    if (inherits(plot, "patchwork") && requireNamespace("patchwork", quietly = TRUE)) {
      plot <- plot + patchwork::plot_annotation(
        title = title,
        subtitle = subtitle,
        caption = caption
      )
    } else {
      labels <- list()
      if (!is.null(title)) labels$title <- title
      if (!is.null(subtitle)) labels$subtitle <- subtitle
      if (!is.null(caption)) labels$caption <- caption
      plot <- plot + do.call(ggplot2::labs, labels)
    }
  }
  if (!is.null(theme)) {
    plot <- if (inherits(plot, "patchwork")) plot & theme else plot + theme
  }
  plot
}


.uniml_scale_color <- function(palette = "bmj", values = NULL, name = NULL) {
  if (is.null(values)) {
    values <- .uniml_plot_palette(1L, palette = palette)
  }
  ggplot2::scale_color_manual(values = values, name = name)
}

.uniml_scale_fill <- function(palette = "bmj", values = NULL, name = NULL) {
  if (is.null(values)) {
    values <- .uniml_plot_palette(1L, palette = palette)
  }
  ggplot2::scale_fill_manual(values = values, name = name)
}

#' Unified plotting theme
#'
#' Creates the restrained plotting theme used by all `uniml` figures. When
#' `ggsci` is installed, the default `"bmj"` palette is used for discrete
#' colour and fill scales.
#'
#' @param base_size Base font size.
#' @param base_family Base font family.
#' @param legend_position Legend position.
#' @param grid Whether to draw major grid lines.
#'
#' @return A `ggplot2` theme object.
#' @export
#' @examples
#' theme_uniml()
theme_uniml <- function(base_size = 11, base_family = "",
                        legend_position = "right", grid = TRUE) {
  .assert_scalar_number(base_size, "base_size", min = 1)
  .assert_scalar_character(base_family, "base_family", allow_empty = TRUE)
  .assert_choice(legend_position, "legend_position", c("right", "left", "top", "bottom", "none"))
  .assert_scalar_logical(grid, "grid")

  theme <- ggplot2::theme_bw(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = if (isTRUE(grid)) {
        ggplot2::element_line(colour = "grey90", linewidth = 0.25)
      } else {
        ggplot2::element_blank()
      },
      panel.border = ggplot2::element_rect(colour = "grey35", linewidth = 0.35),
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_line(colour = "grey35", linewidth = 0.3),
      axis.text = ggplot2::element_text(colour = "grey15"),
      axis.title = ggplot2::element_text(colour = "grey5"),
      plot.title = ggplot2::element_text(face = "bold", hjust = 0),
      plot.subtitle = ggplot2::element_text(colour = "grey35"),
      legend.position = legend_position,
      legend.title = ggplot2::element_text(face = "bold"),
      strip.background = ggplot2::element_rect(fill = "grey95", colour = "grey80"),
      strip.text = ggplot2::element_text(face = "bold")
    )
  theme
}
