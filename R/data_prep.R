#' Prepare data for upset plot
#'
#' Transforms a binary membership data frame into structured intersection
#' data suitable for plotting. This is called internally by \code{ggupset()}
#' but is exported so advanced users can inspect or manipulate the processed
#' data before plotting.
#'
#' @param data A data frame. Set-membership columns should be logical or
#'   integer (0/1). Non-set columns are ignored.
#' @param sets Character vector of column names representing the sets to plot.
#'   Order determines top-to-bottom display order in the matrix panel.
#' @param min_size Integer. Intersections smaller than this are dropped.
#'   Default 0 (keep all).
#' @param sort_by How to order intersections along the x-axis.
#'   \code{"size"} (default) sorts descending by count.
#'   \code{"degree"} sorts by number of sets in intersection (then size).
#'   \code{"none"} preserves the natural order from the data.
#'
#' @return A list of class \code{"upset_data"} with elements:
#'   \describe{
#'     \item{intersections}{Data frame of intersections with columns
#'       \code{int_id} (factor, x-axis order), \code{size} (count),
#'       one logical column per set, and \code{degree}.}
#'     \item{set_sizes}{Data frame with \code{set} and \code{size} columns.}
#'     \item{sets}{Character vector of set names, in display order.}
#'   }
#' @export
prepare_upset_data <- function(data, sets,
                                min_size = 0,
                                sort_by   = c("size", "degree", "none")) {

  sort_by <- match.arg(sort_by)

  # ── Input validation ────────────────────────────────────────────────────────
  if (!is.data.frame(data))
    stop("`data` must be a data frame.")
  if (length(sets) < 2)
    stop("`sets` must contain at least 2 set names.")

  missing_sets <- sets[!sets %in% names(data)]
  if (length(missing_sets) > 0)
    stop("Column(s) not found in `data`: ",
         paste(missing_sets, collapse = ", "))

  # ── Binary membership matrix ─────────────────────────────────────────────────
  mat <- as.matrix(data[, sets, drop = FALSE])
  storage.mode(mat) <- "integer"
  mat[is.na(mat)] <- 0L

  # Clamp anything >1 to 1 (in case of count columns passed by mistake)
  mat[mat > 1L] <- 1L

  # ── Intersection ID per row ──────────────────────────────────────────────────
  # ID = sorted set names joined by "&"; rows in no set get NA (excluded)
  int_ids <- apply(mat, 1, function(row) {
    members <- sets[as.logical(row)]
    if (length(members) == 0L) return(NA_character_)
    paste(sort(members), collapse = "&")
  })

  valid <- !is.na(int_ids)
  if (!any(valid))
    stop("No rows belong to any set. Check that set columns have TRUE/1 values.")

  # ── Counts ───────────────────────────────────────────────────────────────────
  counts <- sort(table(int_ids[valid]), decreasing = TRUE)

  int_df <- data.frame(
    int_id = names(counts),
    size   = as.integer(counts),
    stringsAsFactors = FALSE
  )

  # ── Which sets participate in each intersection ──────────────────────────────
  for (s in sets) {
    int_df[[s]] <- vapply(int_df$int_id, function(id) {
      s %in% strsplit(id, "&", fixed = TRUE)[[1]]
    }, logical(1L))
  }

  int_df$degree <- as.integer(rowSums(int_df[, sets, drop = FALSE]))

  # ── Filter ───────────────────────────────────────────────────────────────────
  int_df <- int_df[int_df$size >= min_size, ]
  if (nrow(int_df) == 0L)
    stop("No intersections remain after min_size filtering (min_size = ",
         min_size, ").")

  # ── Sort ─────────────────────────────────────────────────────────────────────
  int_df <- switch(sort_by,
    size   = int_df[order(-int_df$size), ],
    degree = int_df[order(int_df$degree, -int_df$size), ],
    none   = int_df
  )

  int_df$int_id  <- factor(int_df$int_id, levels = int_df$int_id)
  rownames(int_df) <- NULL

  # ── Set sizes ────────────────────────────────────────────────────────────────
  set_sizes_df <- data.frame(
    set  = factor(sets, levels = rev(sets)),   # rev → first set at top of matrix
    size = as.integer(colSums(mat))
  )

  structure(
    list(
      intersections    = int_df,
      set_sizes        = set_sizes_df,
      sets             = sets,
      n_sets           = length(sets),
      n_intersections  = nrow(int_df)
    ),
    class = "upset_data"
  )
}
