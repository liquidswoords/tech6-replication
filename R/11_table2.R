# ==============================================================================
# 11_table2.R 
#
# Reproduces Table II from paper
#
# ==============================================================================

# 1. Load Data -----------------------------------------------------------------

df <- readRDS(file.path(path_derived, "analysis.rds"))

# 2. Spec Table ----------------------------------------------------------------

# One row per column of Table II 
# yf, ym  female and male outcome
# mult    bandwidth as a multiple of gender IK bandwidth (Inf = global)
# p       polynomial order of RD (0=none)
# cov     TRUE adds controls + province FE

specs <- tribble(
  ~col, ~yf,             ~ym,             ~mult, ~p, ~cov,
  1,    "hischshr1520f", "hischshr1520m", Inf,   0,  FALSE,
  2,    "hischshr1520f", "hischshr1520m", Inf,   0,  TRUE,
  3,    "hischshr1520f", "hischshr1520m", 1,     1,  FALSE,
  4,    "hischshr1520f", "hischshr1520m", 1,     1,  TRUE,
  5,    "hischshr1520f", "hischshr1520m", 0.5,   1,  TRUE,
  6,    "hischshr1520f", "hischshr1520m", 2,     1,  TRUE,
  7,    "hischshr1520f", "hischshr1520m", 1,     2,  TRUE,
  8,    "hischshr1520f", "hischshr1520m", 1,     3,  TRUE,
  9,    "studshr1530f",  "studshr1530m",  1,     1,  TRUE
) %>%
  mutate(hf = mult * h_ik[yf],
         hm = mult * h_ik[ym])


# Note on Observations (Panel B, cols 3, 4, 7, 8) (Men regular bandwidth):
# We get 1342 vs 1341 in the paper. One municipality lies within our bandwidth 
# by 2e-6. The papers Stata bandwidth was likely marginally smaller. The other
# smaller bandwidths match, so sample is otherwise identical. Coefficients
# agree with Paper.

# 3. Panels A and B ------------------------------------------------------------
 
# run_col(): One regression per column in Table II 
#   cov = FALSE -> no controls and province FE 

run_col <- function(col, y, h, p, cov) {
  rd_est(df, y=y, h=h, p=p,
         controls = if (cov) controls else NULL,
         fe = cov) %>%
    mutate(col = col)
}

panel_A = specs %>%
  transmute(col, y=yf, h=hf, p, cov) %>%
  pmap_dfr(run_col) %>%
  mutate(panel = "A: Women")

panel_B = specs %>%
  transmute(col, y=ym, h=hm, p, cov) %>%
  pmap_dfr(run_col) %>%
  mutate(panel = "B: Men")

t2_AB <- bind_rows(panel_A,panel_B)

# 4. Panel C (SUR) -------------------------------------------------------------

# Perform SUR test to check relation between male and female coefficients

panel_C <- specs %>%
  mutate(sur = pmap(list(yf, ym, hf, hm, p, cov),
                    \(yf, ym, hf, hm, p, cov)
                    sur_test(df, yf, ym, hf, hm, p,
                             controls = if (cov) controls else NULL,
                             fe = cov))) %>%
  unnest(sur) %>%
  select(col, diff, se_diff, p_sur)


# 5. Format Table --------------------------------------------------------------

# Formatting Helpers
f3    <- function(x) sprintf("%.3f", x)
paren <- function(x) paste0("(", f3(x), ")")
stars <- function(p) case_when(p < 0.01 ~ "***", p < 0.05 ~ "**",
                               p < 0.10 ~ "*",   TRUE     ~ "")
col_names <- paste0("(", specs$col, ")")

# Header Rows
header <- rbind(
  `Control function` = c("None", "Linear", "Quadratic", "Cubic")[specs$p + 1],
  `Bandwidth`        = case_when(is.infinite(specs$mult) ~ "Global",
                                 specs$mult == 1   ~ "h",
                                 specs$mult == 0.5 ~ "h/2",
                                 specs$mult == 2   ~ "2h"),
  `Covariates`       = if_else(specs$cov, "Yes", "No")
)

# Rows for Panel A and B
panel_rows <- function(pan) {
  pan <- arrange(pan, col)
  rbind(
    `Outcome Mean`          = f3(pan$mean_y),
    `Islamic Mayor in 1994` = paste0(f3(pan$coef), stars(pan$pval)),
    `s.e.`                     = paren(pan$se),
    `Bandwidth `            = f3(if_else(is.infinite(pan$h), 1, pan$h)),
    `R2`                    = sprintf("%.2f", pan$r2),
    `Observations`          = as.character(pan$n)
  )
}

# Panel C
rows_C <- rbind(`p-value` = f3(arrange(panel_C, col)$p_sur))

# Assemble Table
title_row <- function(txt) matrix("", 1, 9, dimnames = list(txt, NULL))

t2_mat <- rbind(
  header,
  title_row("Panel A: Women"),  panel_rows(panel_A),
  title_row("Panel B: Men"),    panel_rows(panel_B),
  title_row("Panel C: Test of coefficient equality between women and men"),
  rows_C
)
colnames(t2_mat) <- col_names

t2_tab <- as_tibble(t2_mat) %>%
  mutate(Variable = trimws(rownames(t2_mat)), .before = 1)


# 6. Save ----------------------------------------------------------------------

# Save Raw CSV
t2_raw <- bind_rows(panel_A, panel_B) %>%
  left_join(panel_C, by = "col")
write_csv(t2_raw, file.path(path_tables, "table2_raw.csv"))


# Word Formatting and Save
save_docx(t2_tab, "table2.docx",
          bold_rows = which(startsWith(t2_tab$Variable, "Panel")))


message("11_table2 done: Table II saved to output/tables.")

