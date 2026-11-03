# ==============================================================================
# 12_table3.R 
#
# Reproduces Table II from paper
# Table III: Alternative RD Specifications
#   -panel (a): Women
#   -panel (b): Men
#
# Saves as raw .csv and .docx in tech6-replication/output/tables
#
# ==============================================================================

# 1. Load Data -----------------------------------------------------------------

df <- readRDS(file.path(path_derived, "analysis.rds"))
