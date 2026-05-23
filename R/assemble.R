#' Assemble upset plot panels into a single figure
#'
#' Combines the bar, matrix, and (optionally) set-size panels using
#' \code{patchwork}. Returns an object of class \code{"ggupset_plot"} that
#' prints as a combined figure. Applying \code{+} to the result forwards a
#' \code{theme()} call to all panels simultaneously (equivalent to patchwork's
#' \code{&}).
#'
#' @param bars A \code{ggplot} from \code{build_intersection_bars()}.
#' @param matrix_plot A \code{ggplot} from \code{build_dot_matrix()}.
#' @param sets_plot Optional \code{ggplot} from \code{build_set_sizes()}, or
#'   \code{NULL} (default).
#' @param bar_height Relative height of the bar panel. Default \code{3}.
#' @param matrix_height Relative height of the matrix panel. Default \code{1}.
#' @param set_width Relative width of the set-size panel (only used when
#'   \code{sets_plot} is not \code{NULL}). Default \code{0.25}.
#'
#' @return An object of class \code{c("ggupset_plot", "patchwork")}.
#' @export
assemble_upset <- function(bars,
                             matrix_plot,
                             sets_plot     = NULL,
                             bar_height    = 3,
                             matrix_height = 1,
                             set_width     = 0.25) {

  heights <- c(bar_height, matrix_height)

  if (is.null(sets_plot)) {
    # Simple vertical stack: bars on top, matrix on bottom
    combined <- (bars / matrix_plot) +
      patchwork::plot_layout(
        heights = heights,
        axes    = "collect_x"
      )
  } else {
    # 2×2 grid:
    #   [spacer    ][bars  ]
    #   [set_sizes ][matrix]
    combined <- patchwork::wrap_plots(
      patchwork::plot_spacer(),
      bars,
      sets_plot,
      matrix_plot,
      ncol    = 2,
      widths  = c(set_width, 1 - set_width),
      heights = heights
    ) +
      patchwork::plot_layout(axes = "collect_x")
  }

  structure(combined, class = c("ggupset_plot", class(combined)))
}


# ── S3 helpers ───────────────────────────────────────────────────────────────

#' Apply a ggplot2 modifier to all panels of a ggupset_plot
#'
#' Because a \code{ggupset_plot} is a \code{patchwork} object, use the
#' \code{&} operator (not \code{+}) to broadcast a \code{theme()} call or
#' other modifier to every panel simultaneously:
#'
#' \preformatted{
#'   p <- ggupset(data, sets)
#'   p & theme(axis.text = element_text(size = 12))
#' }
#'
#' Using \code{+} targets only the last-added patchwork element, which is
#' standard patchwork behaviour.
#'
#' @param x A \code{ggupset_plot}.
#' @param ... Passed to \code{\link[patchwork]{plot_annotation}}.
#' @name ggupset_plot-helpers
NULL
