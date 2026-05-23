#' Base theme used internally by panel builders
#'
#' Not exported. Applied to each panel during construction via \code{%+replace%}
#' so it fully overrides \code{theme_classic()}. User-facing themes are purely
#' additive patches and should not use \code{%+replace%}.
#' @noRd
.upset_base_theme <- function(base_size = 9, base_family = "") {
  ggplot2::theme_classic(base_size = base_size, base_family = base_family) %+replace%
    ggplot2::theme(
      axis.text       = ggplot2::element_text(color = "black", size = base_size),
      axis.title      = ggplot2::element_text(color = "black", size = base_size),
      axis.line       = ggplot2::element_line(color = "black", linewidth = 0.4),
      axis.ticks      = ggplot2::element_line(color = "black", linewidth = 0.3),
      strip.text      = ggplot2::element_text(size = base_size, face = "bold"),
      strip.background = ggplot2::element_blank(),
      panel.grid      = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA)
    )
}


#' Publication-ready theme patch for upset plots
#'
#' Applies clean, journal-appropriate styling to an assembled upset plot.
#' Use with \code{&} to broadcast across all panels:
#'
#' \preformatted{
#'   p <- ggupset(data, sets)
#'   p & theme_upset_clean()
#' }
#'
#' This theme is purely additive — it patches individual panel themes rather
#' than replacing them, so panel-level layout controls (hidden x-axis ticks,
#' etc.) are preserved.
#'
#' @param base_size Base font size in pts. Default \code{9}.
#' @param base_family Base font family. Default \code{""}.
#'
#' @return A \code{theme} object.
#' @export
theme_upset_clean <- function(base_size = 9, base_family = "") {
  ggplot2::theme(
    text            = ggplot2::element_text(size = base_size,
                                             family = base_family, color = "black"),
    axis.text       = ggplot2::element_text(size = base_size,
                                             family = base_family, color = "black"),
    axis.title      = ggplot2::element_text(size = base_size,
                                             family = base_family, color = "black"),
    axis.line       = ggplot2::element_line(color = "black", linewidth = 0.4),
    axis.ticks      = ggplot2::element_line(color = "black", linewidth = 0.3),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor   = ggplot2::element_blank(),
    strip.background   = ggplot2::element_blank(),
    legend.text     = ggplot2::element_text(size = base_size),
    legend.title    = ggplot2::element_text(size = base_size),
    plot.background = ggplot2::element_rect(fill = "white", color = NA),
    panel.background = ggplot2::element_rect(fill = "white", color = NA)
  )
}


#' Presentation theme patch for upset plots
#'
#' Larger fonts and slightly bolder lines for slides and posters.
#' Use with \code{&} to broadcast across all panels.
#'
#' @param base_size Base font size in pts. Default \code{12}.
#' @param base_family Base font family. Default \code{""}.
#'
#' @return A \code{theme} object.
#' @export
theme_upset_presentation <- function(base_size = 12, base_family = "") {
  ggplot2::theme(
    text       = ggplot2::element_text(size = base_size,
                                        family = base_family, color = "black"),
    axis.text  = ggplot2::element_text(size = base_size,
                                        family = base_family, color = "black"),
    axis.title = ggplot2::element_text(size = base_size, family = base_family,
                                        color = "black", face = "bold"),
    axis.line  = ggplot2::element_line(color = "black", linewidth = 0.6),
    axis.ticks = ggplot2::element_line(color = "black", linewidth = 0.5),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor   = ggplot2::element_blank(),
    plot.background    = ggplot2::element_rect(fill = "white", color = NA),
    panel.background   = ggplot2::element_rect(fill = "white", color = NA)
  )
}


#' Save an upset plot with journal-standard dimensions
#'
#' A thin wrapper around \code{ggplot2::ggsave()} with sensible defaults for
#' common publication formats. Width presets follow standard single-column
#' (3.46 in / 88 mm) and double-column (7.09 in / 180 mm) journal figures.
#'
#' @param plot A \code{ggupset_plot} or any ggplot object.
#' @param filename Output file path. Extension determines format
#'   (\code{.pdf}, \code{.png}, \code{.svg}, \code{.tiff}, etc.).
#' @param width Figure width in inches. Preset shortcuts: \code{"single"}
#'   (3.46 in, one journal column), \code{"double"} (7.09 in, two columns),
#'   \code{"full"} (6.85 in, common full-width). Default \code{"double"}.
#' @param height Figure height in inches. Default \code{3.5}.
#' @param dpi Resolution for raster formats. Default \code{300}.
#' @param ... Additional arguments passed to \code{ggplot2::ggsave()}.
#'
#' @return Invisibly returns \code{filename}.
#' @export
save_upset <- function(plot,
                        filename,
                        width  = "double",
                        height = 3.5,
                        dpi    = 300,
                        ...) {

  width_in <- switch(as.character(width),
    single = 3.46,
    double = 7.09,
    full   = 6.85,
    as.numeric(width)
  )

  ggplot2::ggsave(filename, plot,
    width  = width_in,
    height = height,
    units  = "in",
    dpi    = dpi,
    ...
  )

  invisible(filename)
}

