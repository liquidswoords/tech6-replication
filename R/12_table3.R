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


# 2. Spec Grid -----------------------------------------------------------------

# 5 bandwidths x 5 polynomial orders x 2 genders
# Fixed bandwidths; controls + province FE included

bws      <- c(1, 0.5, 0.25, 0.1, 0.05)

specs <- expand_grid(
  y = c("hischshr1520f", "hischshr1520m"),
  p = 0:4,
  h = bws
)

# 3. Estimate ------------------------------------------------------------------

t3_raw <- specs %>%
  pmap_dfr(\(y, p, h) rd_est(df, y = y, h = h, p = p, controls = controls)) %>%
  mutate(panel = if_else(outcome == "hischshr1520f", "A: Women", "B: Men"))

# Check if correct sample size
stopifnot(
  all(t3_raw %>% filter(p == 0) %>% pull(n) ==
        rep(c(2628, 2177, 1049, 489, 257), 2))
)

# 4. Format Table --------------------------------------------------------------

poly_lab <- c("None", "Linear", "Quadratic", "Cubic", "Quartic")


panel_rows3 <- function(pan) {
  out <- NULL
  for (k in 0:4) {
    r <- pan %>% filter(p == k) %>% arrange(desc(h))   
    out <- rbind(out,
                 matrix(paste0(f3(r$coef), stars(r$pval)), 1, 
                        dimnames = list(poly_lab[k + 1], NULL)),
                 matrix(paren(r$se),                       1, 
                        dimnames = list("",              NULL)))
  }
  n <- pan %>% filter(p == 0) %>% arrange(desc(h)) %>% pull(n)
  rbind(out, matrix(as.character(n), 1, dimnames = list("Observations", NULL)))
}

title_row3 <- function(txt) matrix("", 1, 5, dimnames = list(txt, NULL))


t3_mat <- rbind(
  matrix(as.character(bws), 1, dimnames = list("Bandwidth", NULL)),
  title_row3("Panel A: Women"), 
  panel_rows3(filter(t3_raw, panel == "A: Women")),
  title_row3("Panel B: Men"),   
  panel_rows3(filter(t3_raw, panel == "B: Men"))
)

colnames(t3_mat) <- paste0("(", 1:5, ")")

t3_tab <- as_tibble(t3_mat) %>%
  mutate(Variable = rownames(t3_mat), .before = 1)

# 5. Save ----------------------------------------------------------------------

# Save CSV
write_csv(t3_raw, file.path(path_tables, "table3_raw.csv"))

# Save Word
save_docx(t3_tab, "table3.docx",
          bold_rows = which(startsWith(t3_tab$Variable, "Panel")))


message("12_table3 done: Table III saved to output/tables.")

