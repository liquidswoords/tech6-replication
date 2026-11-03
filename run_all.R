# ==============================================================================
# run_all.R : Reproduce every table and figure from data/raw/regdata0.dta
#
# Usage: Open tech6-replication.Rproj, then run this file
#
# ==============================================================================

# 0. Anchor Project Root -------------------------------------------------------

here::i_am("run_all.R")
message("Project Root: ", here::here())

# 1. Scripts to Run ------------------------------------------------------------

scripts <- c(
  "00_packages",
  "01_clean",
  "02_functions",
  "03_bandwidths",
  "10_table1", "11_table2"
  # Remains to be added: Figures & Tables + Extensions
)

# 2. Run All -------------------------------------------------------------------

for (s in scripts) {
  message("\n==== Running ", s, " ====")
  source(here::here("R", paste0(s, ".R")))
}


# 3. Log Package Versions ------------------------------------------------------


writeLines(capture.output(sessionInfo()),
           here::here("output", "logs", "sessionInfo.txt"))

message("\nAll done.")



