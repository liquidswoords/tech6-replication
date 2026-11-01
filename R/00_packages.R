# ==============================================================================
# 00_packages.R : Install if missing and load every package
#
# ==============================================================================

# 1. Packages ------------------------------------------------------------------
pkgs <- c(
  "here",
  "haven",
  "tidyverse",
  "fixest",
  "rddensity",
  "modelsummary",
  "patchwork"
)

missing <-setdiff(pkgs, rownames(installed.packages()))

if (length(missing) > 0) {
  install.packages(missing, repos = "https://cloud.r-project.org")
}

invisible(lapply(missing, library,character.only=TRUE))

# 2. Paths ---------------------------------------------------------------------

path_raw     <- here("data", "raw")
path_derived <- here("data", "derived")
path_tables  <- here("output", "tables")
path_figures <- here("output", "figures")
path_logs    <- here("output", "logs")

# Create output folders if they do not exist yet
for (p in c(path_derived, path_tables, path_figures, path_logs)) {
  dir.create(p, showWarnings = FALSE, recursive = TRUE)
}

# 3. Global Options ------------------------------------------------------------

set.seed(2026)
options(digits = 4, scipen = 999)      # readable printed output
theme_set(theme_minimal(base_size = 11))


message("Setup complete: ", length(pkgs), " packages loaded.")
