#' Build the set-membership dot matrix
#'
#' Returns a plain \code{ggplot2} object showing which sets belong to each
#' intersection. Filled dots are connected by a vertical line segment within
#' each intersection column. The x-axis uses the same factor levels as the
#' bar panel so that \code{patchwork} can align them perfectly.
#'
#' @param upset_data Output of \code{prepare_upset_data()}.
#' @param dot_size Size of dots. Default \code{3}.
#' @param dot_color Colour of filled (active) dots. Default \code{"grey10"}.
#' @param empty_color Colour of empty (inactive) dots. Default \code{"grey88"}.
#' @param connector_color Colour of vertical segment lines. Default \code{"grey10"}.
#' @param connector_width \code{linewidth} of connector segments. Default \code{0.7}.
#' @param show_empty_dots Logical. Draw empty dots for sets not in an
#'   intersection? Strongly recommended; set \code{FALSE} to hide them.
#'   Default \code{TRUE}.
#' @param set_face Font face for set name labels on the y-axis.
#'   Default \code{"italic"} (appropriate for species/gene names).
#' @param show_degree_lines Logical. Draw faint vertical lines between degree
#'   groups when the data was sorted by degree? Default \code{FALSE}.
#' @param degree_line_color Colour of degree separator lines. Default \code{"grey80"}.
#' @param base_size Base font size. Default \code{9}.
#' @param base_family Base font family. Default \code{""}.
#'
#' @return A \code{ggplot} object.
#' @export
build_dot_matrix <- function(upset_data,
                               dot_size          = 3,
                               dot_color         = "grey10",
                               empty_color       = "grey88",
                               connector_color   = "grey10",
                               connector_width   = 0.7,
                               show_empty_dots   = TRUE,
                               set_face          = "italic",
                               show_degree_lines = FALSE,
                               degree_line_color = "grey80",
                               base_size         = 9,
                               base_family       = "") {

  stopifnot(inherits(upset_data, "upset_data"))

  sets  <- upset_data$sets
  n     <- length(sets)
  df    <- upset_data$intersections

  # ── Numeric y positions (top of plot = sets[1]) ───────────────────────────
  set_y <- stats::setNames(seq(n, 1L, -1L), sets)

  # ── Long format dots ──────────────────────────────────────────────────────
  dots_long <- do.call(rbind, lapply(sets, function(s) {
    data.frame(
      int_id = df$int_id,
      set    = s,
      y      = set_y[[s]],
      member = df[[s]],
      stringsAsFactors = FALSE
    )
  }))
  dots_long$int_id <- factor(dots_long$int_id, levels = levels(df$int_id))

  # ── Connector segments ────────────────────────────────────────────────────
  active <- dots_long[dots_long$member, ]

  conn_df <- do.call(rbind, lapply(
    split(active, droplevels(active$int_id)),
    function(g) {
      if (nrow(g) < 2L) return(NULL)
      data.frame(
        int_id = g$int_id[1],
        y_low  = min(g$y),
        y_high = max(g$y),
        stringsAsFactors = FALSE
      )
    }
  ))
  if (!is.null(conn_df))
    conn_df$int_id <- factor(conn_df$int_id, levels = levels(df$int_id))

  # ── Degree separator x-positions ─────────────────────────────────────────
  # A separator falls between the last intersection of degree d and the first
  # of degree d+1 (only when consecutive degrees differ)
  sep_x <- NULL
  if (show_degree_lines && nrow(df) > 1L) {
    deg_rle   <- rle(df$degree)
    change_at <- cumsum(deg_rle$lengths)
    # x positions are between integer positions; separator at i + 0.5
    sep_x <- change_at[-length(change_at)] + 0.5
  }

  # ── Plot ─────────────────────────────────────────────────────────────────
  p <- ggplot2::ggplot() +
    { if (show_empty_dots)
        ggplot2::geom_point(
          data = dots_long,
          ggplot2::aes(x = .data$int_id, y = .data$y),
          color = empty_color,
          size  = dot_size
        )
    } +
    { if (!is.null(conn_df) && nrow(conn_df) > 0L)
        ggplot2::geom_segment(
          data = conn_df,
          ggplot2::aes(
            x    = .data$int_id, xend = .data$int_id,
            y    = .data$y_low,  yend = .data$y_high
          ),
          color     = connector_color,
          linewidth = connector_width
        )
    } +
    ggplot2::geom_point(
      data = dots_long[dots_long$member, ],
      ggplot2::aes(x = .data$int_id, y = .data$y),
      color = dot_color,
      size  = dot_size
    ) +
    # Optional degree-group separator lines
    { if (!is.null(sep_x) && length(sep_x) > 0L)
        ggplot2::geom_vline(
          xintercept = sep_x,
          color      = degree_line_color,
          linewidth  = 0.4,
          linetype   = "dashed"
        )
    } +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::scale_y_continuous(
      breaks = seq_len(n),
      labels = rev(sets),
      limits = c(0.5, n + 0.5),
      expand = ggplot2::expansion(0)
    ) +
    ggplot2::labs(x = NULL, y = NULL) +
    .upset_base_theme(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      axis.text.x   = ggplot2::element_blank(),
      axis.ticks.x  = ggplot2::element_blank(),
      axis.line.x   = ggplot2::element_blank(),
      axis.line.y   = ggplot2::element_line(color = "black"),
      axis.ticks.y  = ggplot2::element_blank(),
      axis.text.y   = ggplot2::element_text(
        color  = "black",
        size   = base_size,
        face   = set_face
      ),
      panel.grid    = ggplot2::element_blank()
    )

  p
}
