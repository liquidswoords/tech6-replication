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
  "01_clean"
  # "10_table1", "11_table2", "12_table3", "13_table7",
  # "20_fig1", "21_fig2", "22_fig3", "23_fig4",
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
