#' Build the intersection size bar chart
#'
#' Returns a plain \code{ggplot2} object showing intersection sizes as a bar
#' chart. Because it is a standard ggplot, any ggplot2 function—
#' \code{theme()}, \code{scale_fill_manual()}, \code{labs()}, etc.—can be
#' applied directly.
#'
#' @param upset_data Output of \code{prepare_upset_data()}.
#' @param bar_fill Fill colour for bars when \code{fill_by = NULL}.
#'   Default \code{"grey10"}.
#' @param bar_color Outline colour for bars. Default \code{NA} (none).
#' @param bar_width Width of bars (0–1). Default \code{0.6}.
#' @param fill_by How to colour bars. \code{NULL} (default) uses a single
#'   \code{bar_fill} colour. \code{"degree"} colours bars by the number of
#'   sets in the intersection (uses a sequential grey palette by default;
#'   override with \code{+ scale_fill_manual(...)}).
#' @param degree_palette Character vector of colours for degree levels when
#'   \code{fill_by = "degree"}. Recycled if too short. Defaults to a
#'   sequential palette from light grey to near-black.
#' @param show_counts Logical. Add count labels above bars? Default \code{FALSE}.
#' @param count_size Text size for count labels. Default \code{3}.
#' @param count_color Colour for count labels. Default \code{"grey10"}.
#' @param y_label Label for the y-axis. Default \code{"Intersection size"}.
#' @param base_size Base font size passed to \code{theme_classic()}. Default \code{9}.
#' @param base_family Base font family. Default \code{""}.
#'
#' @return A \code{ggplot} object.
#' @export
build_intersection_bars <- function(upset_data,
                                     bar_fill       = "grey10",
                                     bar_color      = NA,
                                     bar_width      = 0.6,
                                     fill_by        = NULL,
                                     degree_palette = NULL,
                                     show_counts    = FALSE,
                                     count_size     = 3,
                                     count_color    = "grey10",
                                     y_label        = "Intersection size",
                                     base_size      = 9,
                                     base_family    = "") {

  stopifnot(inherits(upset_data, "upset_data"))

  df <- upset_data$intersections

  # ── Fill mapping ─────────────────────────────────────────────────────────────
  use_fill_mapping <- !is.null(fill_by) && fill_by == "degree"

  if (use_fill_mapping) {
    df$fill_var <- factor(df$degree)
    aes_spec    <- ggplot2::aes(x = .data$int_id, y = .data$size,
                                 fill = .data$fill_var)
  } else {
    aes_spec <- ggplot2::aes(x = .data$int_id, y = .data$size)
  }

  p <- ggplot2::ggplot(df, aes_spec) +
    { if (use_fill_mapping)
        ggplot2::geom_col(color = bar_color, width = bar_width)
      else
        ggplot2::geom_col(fill = bar_fill, color = bar_color, width = bar_width)
    } +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, if (show_counts) 0.2 else 0.08))
    ) +
    ggplot2::labs(x = NULL, y = y_label) +
    .upset_base_theme(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      axis.text.x        = ggplot2::element_blank(),
      axis.ticks.x       = ggplot2::element_blank(),
      axis.line.x        = ggplot2::element_blank(),
      axis.text.y        = ggplot2::element_text(color = "black", size = base_size),
      axis.title.y       = ggplot2::element_text(color = "black", size = base_size,
                                                  angle = 90, vjust = 0.5),
      axis.title.x       = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "grey90", linewidth = 0.3),
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      legend.position    = "none"   # degree legend rarely useful on a bar chart
    )

  # Apply degree palette if needed
  if (use_fill_mapping) {
    n_degrees <- length(unique(df$degree))
    if (is.null(degree_palette)) {
      # Sequential light-to-dark, works for 1-6 levels
      greys <- c("grey78", "grey58", "grey38", "grey22", "grey12", "grey6")
      degree_palette <- greys[seq_len(min(n_degrees, length(greys)))]
    }
    p <- p + ggplot2::scale_fill_manual(
      values = rep_len(degree_palette, n_degrees),
      name   = "Degree"
    )
  }

  if (show_counts) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = .data$size),
      vjust  = -0.4,
      size   = count_size,
      color  = count_color,
      family = base_family
    )
  }

  p
}
