library(testthat)
library(ggplot2)
library(patchwork)

# ── Shared test data ──────────────────────────────────────────────────────────
make_df <- function(n = 100, seed = 1) {
  set.seed(seed)
  df <- data.frame(
    A = as.integer(sample(0:1, n, replace = TRUE)),
    B = as.integer(sample(0:1, n, replace = TRUE)),
    C = as.integer(sample(0:1, n, replace = TRUE)),
    D = as.integer(sample(0:1, n, replace = TRUE))
  )
  df[rowSums(df) > 0, ]
}

# ── prepare_upset_data ────────────────────────────────────────────────────────

test_that("prepare_upset_data returns correct structure", {
  df <- make_df()
  ud <- prepare_upset_data(df, sets = c("A", "B", "C"))

  expect_s3_class(ud, "upset_data")
  expect_named(ud, c("intersections", "set_sizes", "sets",
                      "n_sets", "n_intersections"))
  expect_true(all(c("int_id", "size", "degree", "A", "B", "C") %in%
                    names(ud$intersections)))
  expect_equal(ud$sets, c("A", "B", "C"))
  expect_equal(ud$n_sets, 3L)
})

test_that("prepare_upset_data sorts by size by default", {
  df <- make_df()
  ud <- prepare_upset_data(df, c("A", "B", "C"))
  sizes <- ud$intersections$size
  expect_true(all(diff(sizes) <= 0))
})

test_that("prepare_upset_data sorts by degree correctly", {
  df <- make_df()
  ud <- prepare_upset_data(df, c("A", "B", "C"), sort_by = "degree")
  degs <- ud$intersections$degree
  expect_true(all(diff(degs) >= 0))
})

test_that("min_size filtering works", {
  df <- make_df(200)
  ud <- prepare_upset_data(df, c("A", "B", "C"), min_size = 10)
  expect_true(all(ud$intersections$size >= 10))
})

test_that("set_sizes sums match data", {
  df <- make_df()
  ud <- prepare_upset_data(df, c("A", "B"))
  expect_equal(
    ud$set_sizes$size[ud$set_sizes$set == "A"],
    sum(df$A)
  )
})

test_that("prepare_upset_data errors on missing columns", {
  df <- make_df()
  expect_error(prepare_upset_data(df, c("A", "Z")), "Column\\(s\\) not found")
})

test_that("prepare_upset_data errors on fewer than 2 sets", {
  df <- make_df()
  expect_error(prepare_upset_data(df, "A"), "at least 2")
})

# ── build_intersection_bars ───────────────────────────────────────────────────

test_that("build_intersection_bars returns a ggplot", {
  ud <- prepare_upset_data(make_df(), c("A", "B", "C"))
  p  <- build_intersection_bars(ud)
  expect_s3_class(p, "ggplot")
})

test_that("fill_by='degree' produces a fill aesthetic", {
  ud <- prepare_upset_data(make_df(), c("A", "B", "C"))
  p  <- build_intersection_bars(ud, fill_by = "degree")
  expect_true(!is.null(p$mapping$fill))
})

# ── build_dot_matrix ──────────────────────────────────────────────────────────

test_that("build_dot_matrix returns a ggplot", {
  ud <- prepare_upset_data(make_df(), c("A", "B", "C"))
  p  <- build_dot_matrix(ud)
  expect_s3_class(p, "ggplot")
})

# ── assemble_upset ────────────────────────────────────────────────────────────

test_that("assemble_upset returns ggupset_plot class", {
  ud <- prepare_upset_data(make_df(), c("A", "B", "C"))
  p  <- assemble_upset(build_intersection_bars(ud), build_dot_matrix(ud))
  expect_s3_class(p, "ggupset_plot")
  expect_s3_class(p, "patchwork")
})

test_that("assemble_upset with set sizes panel works", {
  ud  <- prepare_upset_data(make_df(), c("A", "B", "C"))
  p   <- assemble_upset(
    build_intersection_bars(ud),
    build_dot_matrix(ud),
    build_set_sizes(ud)
  )
  expect_s3_class(p, "ggupset_plot")
})

# ── ggupset() full pipeline ───────────────────────────────────────────────────

test_that("ggupset() returns ggupset_plot and renders without error", {
  df <- make_df(200)
  p  <- ggupset(df, sets = c("A", "B", "C"))
  expect_s3_class(p, "ggupset_plot")
  expect_silent(ggplot2::ggplotGrob(patchwork::wrap_plots(p)))
})

test_that("ggupset() show_set_sizes works", {
  df <- make_df()
  expect_no_error(ggupset(df, c("A", "B", "C"), show_set_sizes = TRUE))
})

test_that("ggupset() fill_by='degree' works", {
  df <- make_df()
  expect_no_error(ggupset(df, c("A", "B", "C"), fill_by = "degree"))
})

test_that("ggupset() degree sort + lines work", {
  df <- make_df()
  expect_no_error(
    ggupset(df, c("A", "B", "C"), sort_by = "degree", show_degree_lines = TRUE)
  )
})

test_that("panels attribute is accessible", {
  df <- make_df()
  p  <- ggupset(df, c("A", "B", "C"))
  panels <- attr(p, "panels")
  expect_named(panels, c("bars", "matrix", "sets"))
  expect_s3_class(panels$bars, "ggplot")
  expect_s3_class(panels$matrix, "ggplot")
})

test_that("upset_data attribute is accessible", {
  df <- make_df()
  p  <- ggupset(df, c("A", "B", "C"))
  ud <- attr(p, "upset_data")
  expect_s3_class(ud, "upset_data")
})

# ── Themes ───────────────────────────────────────────────────────────────────

test_that("theme_upset_clean returns a theme", {
  expect_s3_class(theme_upset_clean(), "theme")
})

test_that("theme_upset_presentation returns a theme", {
  expect_s3_class(theme_upset_presentation(), "theme")
})

test_that("& theme broadcast works on ggupset_plot", {
  df <- make_df()
  p  <- ggupset(df, c("A", "B", "C"))
  expect_no_error(p & ggplot2::theme(axis.text = ggplot2::element_text(size = 14)))
})
