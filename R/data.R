#' Simulated crop orthogroup presence/absence data
#'
#' A data frame simulating the output of a comparative-genomics orthogroup
#' analysis across five crop species. Each row is an orthogroup (OG); each
#' species column records whether that orthogroup was detected (1) or absent (0)
#' in that species. The data were generated with a latent conservation score so
#' that broadly-conserved families tend to be present across all five crops,
#' while more derived families appear in only one or two species — a pattern
#' typical of real OrthoFinder or OrthoMCL output.
#'
#' @format A data frame with 461 rows and 6 columns:
#' \describe{
#'   \item{gene_id}{Character. Orthogroup identifier (e.g. \code{"OG00001"}).}
#'   \item{Maize}{Integer (0/1). Presence in \emph{Zea mays}.}
#'   \item{Sorghum}{Integer (0/1). Presence in \emph{Sorghum bicolor}.}
#'   \item{Soybean}{Integer (0/1). Presence in \emph{Glycine max}.}
#'   \item{Wheat}{Integer (0/1). Presence in \emph{Triticum aestivum}.}
#'   \item{Rice}{Integer (0/1). Presence in \emph{Oryza sativa}.}
#' }
#'
#' @source Simulated. See \code{data-raw/crop_genes.R} for the generation
#'   script.
#'
#' @examples
#' data(crop_genes)
#'
#' # Quick look
#' head(crop_genes)
#' colSums(crop_genes[, -1])   # set sizes
#'
#' # Basic upset plot with all five species
#' ggupset(crop_genes,
#'         sets = c("Maize", "Sorghum", "Soybean", "Wheat", "Rice"),
#'         show_counts = TRUE, min_size = 5)
#'
#' # Three-species subset, sorted by degree, coloured by degree
#' ggupset(crop_genes,
#'         sets    = c("Maize", "Sorghum", "Soybean"),
#'         sort_by = "degree",
#'         fill_by = "degree",
#'         show_degree_lines = TRUE,
#'         show_set_sizes    = TRUE)
"crop_genes"
