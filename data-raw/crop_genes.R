## Script to reproduce the crop_genes dataset shipped with ggupset.
## Run this file to regenerate data/crop_genes.rda.
##
## The dataset simulates orthogroup presence/absence across five crop species,
## loosely reflecting the structure of real comparative-genomics output from
## tools like OrthoFinder. A latent conservation score creates realistic
## correlation between species: broadly-conserved gene families tend to be
## present across all five crops, while more derived families are present in
## only one or two.

set.seed(2024)

n_genes <- 500

# Latent conservation score per orthogroup (beta-distributed, right-skewed
# so most genes are moderately conserved, few are universal or unique)
conservation <- rbeta(n_genes, shape1 = 1.5, shape2 = 2.5)

make_presence <- function(base_prob, conservation, n) {
  p <- pmin(pmax(base_prob * (0.4 + 0.9 * conservation), 0.05), 0.97)
  as.integer(rbinom(n, 1, p))
}

crop_genes <- data.frame(
  gene_id = paste0("OG", formatC(seq_len(n_genes), width = 5, flag = "0")),
  Maize   = make_presence(0.72, conservation, n_genes),
  Sorghum = make_presence(0.68, conservation, n_genes),
  Soybean = make_presence(0.61, conservation, n_genes),
  Wheat   = make_presence(0.55, conservation, n_genes),
  Rice    = make_presence(0.64, conservation, n_genes)
)

# Drop orthogroups absent from all five species
crop_genes <- crop_genes[rowSums(crop_genes[, 2:6]) > 0, ]
rownames(crop_genes) <- NULL

save(crop_genes, file = "data/crop_genes.rda", compress = "xz")
