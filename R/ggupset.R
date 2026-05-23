#' Create an upset plot
#'
#' The main entry point for \code{ggupset}. Processes data, builds each panel,
#' and assembles them. All arguments can also be set by calling the underlying
#' builder functions directly if you want maximum control over individual panels.
#'
#' @param data A data frame with binary set-membership columns.
#' @param sets Character vector of column names to use as sets.
#' @param min_size Minimum intersection size to display. Default \code{0}.
#' @param sort_by Sort intersections by \code{"size"} (default), \code{"degree"},
#'   or \code{"none"}.
#'
#' @section Bar panel:
#' @param bar_fill Bar fill colour. Default \code{"grey10"}.
#' @param bar_color Bar outline colour. Default \code{NA}.
#' @param bar_width Bar width (0–1). Default \code{0.6}.
#' @param show_counts Show count labels above bars. Default \code{FALSE}.
#' @param count_size Text size for count labels. Default \code{3}.
#' @param y_label Y-axis label for the bar panel. Default \code{"Intersection size"}.
#'
#' @section Matrix panel:
#' @param dot_size Dot size. Default \code{3}.
#' @param dot_color Filled dot colour. Default \code{"grey10"}.
#' @param empty_color Empty dot colour. Default \code{"grey88"}.
#' @param connector_color Connector line colour. Default \code{"grey10"}.
#' @param connector_width Connector line width. Default \code{0.7}.
#'
#' @section Set size panel:
#' @param show_set_sizes Show horizontal set-size bars. Default \code{FALSE}.
#' @param set_size_fill Set-size bar fill colour. Default \code{"grey10"}.
#' @param set_width Relative width of the set-size panel (0–1). Default \code{0.25}.
#'
#' @section Layout:
#' @param bar_height Relative height of bar panel. Default \code{3}.
#' @param matrix_height Relative height of matrix panel. Default \code{1}.
#'
#' @section Typography:
#' @param base_size Base font size (pts). Default \code{9}.
#' @param base_family Base font family. Default \code{""}.
#'
#' @return An object of class \code{c("ggupset_plot", "patchwork")} that prints
#'   as a combined upset figure. Use \code{+} to apply \code{theme()} or other
#'   ggplot2 modifiers to all panels simultaneously. Access individual panels
#'   via \code{attr(p, "panels")}.
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' ggupset(genes, sets = c("Maize", "Sorghum", "Soybean"))
#'
#' # With set sizes and count labels
#' ggupset(genes, sets = c("Maize", "Sorghum", "Soybean"),
#'         show_set_sizes = TRUE, show_counts = TRUE)
#'
#' # Customise all panels at once
#' p <- ggupset(genes, sets = c("Maize", "Sorghum", "Soybean"))
#' p + theme(axis.text = element_text(family = "Helvetica", size = 11))
#'
#' # Customise one panel then reassemble
#' ud     <- prepare_upset_data(genes, c("Maize", "Sorghum", "Soybean"))
#' bars   <- build_intersection_bars(ud, bar_fill = "#2E4057")
#' matrix <- build_dot_matrix(ud, dot_color = "#2E4057", connector_color = "#2E4057")
#' assemble_upset(bars, matrix)
#' }
#'
#' @export
ggupset <- function(data,
                    sets,

                    # Data
                    min_size  = 0,
                    sort_by   = c("size", "degree", "none"),

                    # Bars
                    bar_fill       = "grey10",
                    bar_color      = NA,
                    bar_width      = 0.6,
                    fill_by        = NULL,
                    degree_palette = NULL,
                    show_counts    = FALSE,
                    count_size     = 3,
                    y_label        = "Intersection size",

                    # Matrix
                    dot_size          = 3,
                    dot_color         = "grey10",
                    empty_color       = "grey88",
                    connector_color   = "grey10",
                    connector_width   = 0.7,
                    set_face          = "italic",
                    show_degree_lines = FALSE,
                    degree_line_color = "grey80",

                    # Set sizes
                    show_set_sizes = FALSE,
                    set_size_fill  = "grey10",
                    set_width      = 0.25,

                    # Layout
                    bar_height    = 3,
                    matrix_height = 1,

                    # Typography
                    base_size   = 9,
                    base_family = "") {

  sort_by <- match.arg(sort_by)

  # ── Data prep ────────────────────────────────────────────────────────────────
  ud <- prepare_upset_data(data, sets,
                            min_size = min_size,
                            sort_by  = sort_by)

  # ── Build panels ─────────────────────────────────────────────────────────────
  p_bars <- build_intersection_bars(
    ud,
    bar_fill       = bar_fill,
    bar_color      = bar_color,
    bar_width      = bar_width,
    fill_by        = fill_by,
    degree_palette = degree_palette,
    show_counts    = show_counts,
    count_size     = count_size,
    y_label        = y_label,
    base_size      = base_size,
    base_family    = base_family
  )

  p_matrix <- build_dot_matrix(
    ud,
    dot_size          = dot_size,
    dot_color         = dot_color,
    empty_color       = empty_color,
    connector_color   = connector_color,
    connector_width   = connector_width,
    set_face          = set_face,
    show_degree_lines = show_degree_lines,
    degree_line_color = degree_line_color,
    base_size         = base_size,
    base_family       = base_family
  )

  p_sets <- if (show_set_sizes) {
    build_set_sizes(
      ud,
      bar_fill    = set_size_fill,
      base_size   = base_size,
      base_family = base_family
    )
  } else {
    NULL
  }

  # ── Assemble ─────────────────────────────────────────────────────────────────
  result <- assemble_upset(
    bars          = p_bars,
    matrix_plot   = p_matrix,
    sets_plot     = p_sets,
    bar_height    = bar_height,
    matrix_height = matrix_height,
    set_width     = set_width
  )

  # Stash individual panels as attribute for power users
  attr(result, "panels") <- list(bars = p_bars, matrix = p_matrix, sets = p_sets)
  attr(result, "upset_data") <- ud

  result
}
