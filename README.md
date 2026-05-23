# ggupset

Clean, modifiable upset plots built on ggplot2 and patchwork.

## Why ggupset?

ComplexUpset works, but customising aesthetics means fighting its internal faceting machinery — theme calls conflict, adding annotations breaks layout, and nothing behaves like a normal ggplot2 object.

`ggupset` takes a different approach: each panel (intersection bars, dot matrix, optional set-size bars) is a **plain ggplot2 object**, assembled with patchwork. This means every ggplot2 idiom just works.

## Installation

```r
# install.packages("remotes")
remotes::install_github("yourusername/ggupset")
```

## Quick start

```r
library(ggupset)

# Basic
ggupset(genes, sets = c("Maize", "Sorghum", "Soybean", "Wheat"))

# With set sizes and count labels
ggupset(genes, sets = c("Maize", "Sorghum", "Soybean", "Wheat"),
        show_set_sizes = TRUE, show_counts = TRUE)

# Colour bars by intersection degree
ggupset(genes, sets = c("Maize", "Sorghum", "Soybean", "Wheat"),
        sort_by = "degree", fill_by = "degree",
        show_degree_lines = TRUE)

# Apply a theme to all panels at once
p <- ggupset(genes, sets = c("Maize", "Sorghum", "Soybean", "Wheat"))
p & theme_upset_presentation(base_size = 12)

# Save at journal-standard width
save_upset(p, "figure2.pdf", width = "double", height = 3.5)
```

## Customisation

### One-liner options (via `ggupset()`)

| Argument | Controls |
|---|---|
| `bar_fill`, `dot_color`, `connector_color` | Colour of active elements |
| `empty_color` | Colour of inactive dots |
| `fill_by = "degree"` | Colour bars by intersection degree |
| `degree_palette` | Custom colour vector for degree levels |
| `sort_by` | `"size"` (default), `"degree"`, or `"none"` |
| `min_size` | Drop small intersections |
| `show_counts` | Count labels above bars |
| `show_set_sizes` | Horizontal set-size panel |
| `show_degree_lines` | Dashed separators between degree groups |
| `set_face` | Font face for set labels (`"italic"`, `"plain"`, `"bold"`) |
| `base_size`, `base_family` | Typography |
| `bar_height`, `matrix_height`, `set_width` | Panel proportions |

### Full panel control

```r
ud     <- prepare_upset_data(genes, c("Maize", "Sorghum", "Soybean"))

# Each builder returns a plain ggplot — modify freely
p_bars <- build_intersection_bars(ud, bar_fill = "#2E4057") +
          scale_y_continuous(limits = c(0, 80)) +
          labs(y = "Gene count")

p_mat  <- build_dot_matrix(ud, dot_color = "#2E4057",
                            connector_color = "#2E4057")

p_sets <- build_set_sizes(ud, bar_fill = "#2E4057", show_counts = TRUE)

assemble_upset(p_bars, p_mat, p_sets, bar_height = 3, set_width = 0.2)
```

### Themes

```r
# Broadcast to all panels with &
p & theme_upset_clean(base_size = 9)       # manuscript
p & theme_upset_presentation(base_size = 12) # slides/poster
```

### Accessing panels after assembly

```r
p      <- ggupset(genes, sets)
panels <- attr(p, "panels")   # list: bars, matrix, sets
ud     <- attr(p, "upset_data")  # processed data
```

## Functions

| Function | Description |
|---|---|
| `ggupset()` | Main entry point |
| `prepare_upset_data()` | Data transformation (exported for inspection) |
| `build_intersection_bars()` | Bar chart panel |
| `build_dot_matrix()` | Dot matrix panel |
| `build_set_sizes()` | Set-size bar panel |
| `assemble_upset()` | Patchwork assembly |
| `theme_upset_clean()` | Manuscript theme patch |
| `theme_upset_presentation()` | Presentation theme patch |
| `save_upset()` | `ggsave` wrapper with journal width presets |
