library(ggplot2)
library(patchwork)

# Source all package files for testing without installing
pkg_r_files <- list.files(
  file.path(dirname(dirname(getwd())), "R"),
  pattern = "\\.R$", full.names = TRUE
)
invisible(lapply(pkg_r_files, source))
