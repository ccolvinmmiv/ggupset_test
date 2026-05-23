#' Build the (optional) set-size bar chart
#'
#' Returns a horizontal bar chart showing the total size of each set.
#' The y-axis mirrors the row order of the dot matrix so that the two panels
#' align when assembled.
#'
#' @param upset_data Output of \code{prepare_upset_data()}.
#' @param bar_fill Fill colour for bars. Default \code{"grey10"}.
#' @param bar_color Outline colour. Default \code{NA}.
#' @param bar_width Width of bars (0–1). Default \code{0.6}.
#' @param show_counts Logical. Add count labels? Default \code{FALSE}.
#' @param count_size Text size for labels. Default \code{3}.
#' @param count_color Colour for labels. Default \code{"grey10"}.
#' @param x_label Label for the x-axis. Default \code{"Set size"}.
#' @param base_size Base font size. Default \code{9}.
#' @param base_family Base font family. Default \code{""}.
#'
#' @return A \code{ggplot} object.
#' @export
build_set_sizes <- function(upset_data,
                              bar_fill    = "grey10",
                              bar_color   = NA,
                              bar_width   = 0.6,
                              show_counts = FALSE,
                              count_size  = 3,
                              count_color = "grey10",
                              x_label     = "Set size",
                              base_size   = 9,
                              base_family = "") {

  stopifnot(inherits(upset_data, "upset_data"))

  sets <- upset_data$sets
  n    <- length(sets)

  df        <- upset_data$set_sizes
  # Numeric y: sets[1] at top (position n), sets[n] at bottom (position 1)
  set_y     <- stats::setNames(seq(n, 1L, -1L), sets)
  df$y      <- set_y[as.character(df$set)]

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$size, y = .data$y)) +
    ggplot2::geom_col(
      fill      = bar_fill,
      color     = bar_color,
      width     = bar_width,
      orientation = "y"
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq_len(n),
      labels = rev(sets),
      limits = c(0.5, n + 0.5),
      expand = ggplot2::expansion(0)
    ) +
    ggplot2::scale_x_reverse(
      expand = ggplot2::expansion(mult = c(if (show_counts) 0.2 else 0.08, 0))
    ) +
    ggplot2::labs(x = x_label, y = NULL) +
    .upset_base_theme(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      axis.text.x   = ggplot2::element_text(color = "black", size = base_size),
      axis.title.x  = ggplot2::element_text(color = "black", size = base_size),
      axis.text.y   = ggplot2::element_blank(),
      axis.ticks.y  = ggplot2::element_blank(),
      axis.line.y   = ggplot2::element_blank(),
      axis.line.x   = ggplot2::element_line(color = "black"),
      panel.grid.major.x = ggplot2::element_line(color = "grey90", linewidth = 0.3),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank()
    )

  if (show_counts) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = .data$size, x = 0),
      hjust  = 1.2,
      size   = count_size,
      color  = count_color,
      family = base_family
    )
  }

  p
}
