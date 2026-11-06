# ==============================================================================
# 13_table7.R
#
# Reproduces Table VII from paper
# Table VII: RD Treatment Effects of Islamic Rule on Education in 2011
#   -Panel A: Global OLS (unconditional, conditional)
#   -Panel B: Conditional local linear RD (h, h/2, 2h)
#
# Saves as raw .csv and .docx in tech6-replication/output/tables
#
# ==============================================================================

# 1. Load Data -----------------------------------------------------------------

df11 <- readRDS(file.path(path_derived, "analysis_2011.rds"))

stopifnot(nrow(df11) == 2338)

stopifnot(sum(abs(df11$iwm94) >= 1) == 0)

stopifnot(sum(df11$buyuk) == 0)
controls7 <- setdiff(controls, "buyuk")

# 2. Spec Grid -----------------------------------------------------------------

outcomes7 <- c("ahischshr15_19f", "ahischshr15_19m",
               "ahischshr15_29f", "ahischshr15_29m",
               "nevm15_19f",      "nevm15_19m")

# One row per cell: 6 outcomes x 5 models.
# Models 1-2 are Panel A (global, p = 0); 3-5 are Panel B (local linear).
specs7 <- expand_grid(y = outcomes7, model = 1:5) %>%
  mutate(
    panel = if_else(model <= 2, "A", "B"),
    p     = if_else(model <= 2, 0L, 1L),
    cov   = model != 1,
    mult  = case_when(model <= 2 ~ Inf,
                      model == 3 ~ 1,
                      model == 4 ~ 0.5,
                      model == 5 ~ 2),
    h     = mult * h_ik[y]
  )

# 3. Estimate ------------------------------------------------------------------

t7_raw <- specs7 %>%
  pmap_dfr(\(y, model, panel, p, cov, mult, h)
    rd_est(df11, y = y, h = h, p = p,
           controls = if (cov) controls7 else NULL,
           fe = cov) %>%
      mutate(model = model, panel = panel))

# Sample sizes at the IK bandwidth, as reported in the paper
stopifnot(
  t7_raw %>% filter(model == 3) %>% pull(n) ==
    c(879, 766, 950, 840, 1009, 976)
)

# 4. Format Table --------------------------------------------------------------

# 4. Format Table --------------------------------------------------------------

# Helpers for building rows of a 6-column character matrix
row_of    <- function(x, label) matrix(x, 1, dimnames = list(label, NULL))
blank_row <- function(txt)      matrix("", 1, 6, dimnames = list(txt, NULL))

# One model's results, always in outcomes7 column order
get_model <- function(m) {
  t7_raw %>% filter(model == m) %>% arrange(match(outcome, outcomes7))
}

# Panel A reports the global bandwidth as 1.00 rather than Inf
h_disp <- function(h) f3(if_else(is.infinite(h), 1, h))

# Coefficient row with stars, then the standard error row
est_rows <- function(r) {
  rbind(
    row_of(paste0(f3(r$coef), stars(r$pval)), "Islamic mayor in 1994"),
    row_of(paren(r$se), "")
  )
}

m1 <- get_model(1); m2 <- get_model(2); m3 <- get_model(3)
m4 <- get_model(4); m5 <- get_model(5)

t7_mat <- rbind(
  row_of(c(rep("High School", 4), rep("Unmarried", 2)),       "Outcome"),
  row_of(c("15-19","15-19","15-29","15-29","15-19","15-19"),  "Age Cohort"),
  row_of(c("Women","Men","Women","Men","Women","Men"),        "Gender"),

  blank_row("Panel A: Global OLS"),
  row_of(f3(m1$mean_y), "Outcome mean"),
  blank_row("Model 1. Unconditional estimates"),
  est_rows(m1),
  blank_row("Model 2. Conditional estimates"),
  est_rows(m2),
  row_of(h_disp(m2$h),      "Bandwidth"),
  row_of(as.character(m2$n), "Observations"),

  blank_row("Panel B: Conditional local linear RD"),
  row_of(f3(m3$mean_y), "Outcome mean"),
  blank_row("Model 1. Bandwidth = h"),
  est_rows(m3),
  row_of(f3(m3$h),           "Bandwidth"),
  row_of(as.character(m3$n), "Observations"),
  blank_row("Model 2. Bandwidth = h/2"),
  est_rows(m4),
  row_of(f3(m4$h),           "Bandwidth"),
  row_of(as.character(m4$n), "Observations"),
  blank_row("Model 3. Bandwidth = 2h"),
  est_rows(m5),
  row_of(f3(m5$h),           "Bandwidth"),
  row_of(as.character(m5$n), "Observations")
)

colnames(t7_mat) <- paste0("(", 1:6, ")")

t7_tab <- as_tibble(t7_mat) %>%
  mutate(Variable = rownames(t7_mat), .before = 1)

# 5. Save ----------------------------------------------------------------------

write_csv(t7_raw, file.path(path_tables, "table7_raw.csv"))

save_docx(t7_tab, "table7.docx",
          bold_rows = which(startsWith(t7_tab$Variable, "Panel")))

message("13_table7 done: Table VII saved to output/tables.")