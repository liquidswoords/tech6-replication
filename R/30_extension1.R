# ==============================================================================
# 30_extension2.R
#
# Extension 2: Persistence or transmission? A cohort profile of the RD effect
#
# Table VII (2011) pools age bins that mix cohorts schooled during the 1994-99
# term with cohorts schooled after it. This script estimates the RD separately
# for every age bin in the 2000 census and the 2011 registry, and places all
# estimates on one axis: year of birth (equivalently, age in 1994).
#
#   1. Exposure mapping: every bin -> birth years -> age in 1994
#   2. Samples and the day-one check on the pooled 2011 bins
#   3. Bandwidths: IK per outcome, and one common bandwidth
#   4. RD estimates for every cohort (education and never-married)
#   5. Cross-year and cross-cohort tests with sur_test()
#   6. Figure E2 (cohort profile) and Figure E2b (never-married)
#
# Uses rd_est(), sur_test() and ik() exactly as defined in 02 and 03.
# Saves .csv to output/tables and .png/.pdf to output/figures
#
# ==============================================================================

# 1. Exposure Mapping ----------------------------------------------------------

# The census is from 2000 and the registry from 2011, so
#   birth year  = year - age
#   age in 1994 = 1994 - birth year
# Open-ended bins (31-64, 65+, 40+) are capped at 64 / 99 for the table only.

cohorts <- tribble(
  ~year, ~bin,     ~stub2000,      ~stub2011,       ~age_lo, ~age_hi,
  2000,  "15-20",  "hischshr1520", NA,              15, 20,
  2000,  "21-25",  "hischshr2125", NA,              21, 25,
  2000,  "26-30",  "hischshr2630", NA,              26, 30,
  2000,  "31-64",  "hischshr3164", NA,              31, 64,
  2000,  "65+",    "hischshr65",   NA,              65, 99,
  2011,  "15-19",  NA,             "15_19",         15, 19,
  2011,  "20-24",  NA,             "20_24",         20, 24,
  2011,  "25-29",  NA,             "25_29",         25, 29,
  2011,  "30-34",  NA,             "30_34",         30, 34,
  2011,  "35-39",  NA,             "35_39",         35, 39,
  2011,  "40+",    NA,             "40",            40, 99
) %>%
  mutate(
    born_lo  = year - age_hi,
    born_hi  = year - age_lo,
    age94_lo = 1994 - born_hi,
    age94_hi = 1994 - born_lo,
    # High school age is 14-17, as the paper describes the Turkish lise. A birth
    # year is "in term" if the person was 14-17 in at least one year of the
    # 1994-1999 term, i.e. born 1977-1985. Share of the bin's birth years that
    # qualify:
    n_years  = born_hi - born_lo + 1,
    in_term  = pmax(0, pmin(born_hi, 1985) - pmax(born_lo, 1977) + 1) / n_years,
    exposure = case_when(
      born_hi < 1977             ~ "Before term (placebo)",
      born_lo > 1985             ~ "After term",
      in_term >= 0.5             ~ "During term",
      TRUE                       ~ "Mixed"
    )
  )

# Outcome names: education (both years) and never-married (2011 only)
cohorts <- cohorts %>%
  mutate(
    educ_f = if_else(year == 2000, paste0(stub2000, "f"),
                     paste0("ahischshr", stub2011, "f")),
    educ_m = if_else(year == 2000, paste0(stub2000, "m"),
                     paste0("ahischshr", stub2011, "m")),
    nevm_f = if_else(year == 2011, paste0("nevm", stub2011, "f"), NA),
    nevm_m = if_else(year == 2011, paste0("nevm", stub2011, "m"), NA)
  )

educ_vars <- c(cohorts$educ_f, cohorts$educ_m)
nevm_vars <- na.omit(c(cohorts$nevm_f, cohorts$nevm_m))
ext_vars  <- c(educ_vars, nevm_vars)

stopifnot(all(ext_vars %in% names(base)))

# 2. Samples -------------------------------------------------------------------

# 2a. Year-specific samples, as in the replication. Adding the fine bins must
#     not change either sample.
stopifnot(nrow(drop_na(df,   all_of(filter(cohorts, year == 2000)$educ_f),
                             all_of(filter(cohorts, year == 2000)$educ_m))) == 2629)
stopifnot(nrow(drop_na(df11, all_of(c(filter(cohorts, year == 2011)$educ_f,
                                      filter(cohorts, year == 2011)$educ_m,
                                      nevm_vars)))) == 2338)

# 2b. Common sample: municipalities observed in BOTH years. Cross-year
#     comparisons use only this sample, so the two series in the figure are
#     estimated on the same towns.
dfc <- base %>%
  drop_na(i94, iwm94, all_of(controls), all_of(ext_vars))

# No metropolitan municipality has 2011 data, so buyuk is constant (as Table VII)
stopifnot(sum(dfc$buyuk) == 0)
controls_c <- setdiff(controls, "buyuk")

message("Common sample: ", nrow(dfc), " municipalities, ",
        sum(dfc$i94), " with an Islamic mayor in 1994")

# 2c. Day-one check: a pooled bin is a population-weighted average of its
#     components, so it must lie between their minimum and maximum.
in_range <- function(pooled, parts, data, tol = 1e-6) {
  lo <- do.call(pmin, data[parts]); hi <- do.call(pmax, data[parts])
  mean(data[[pooled]] >= lo - tol & data[[pooled]] <= hi + tol)
}

bin_check <- tibble(
  pooled = c("ahischshr15_29f", "ahischshr15_29m",
             "ahischshr15_39f", "ahischshr15_39m"),
  parts  = list(paste0("ahischshr", c("15_19", "20_24", "25_29"), "f"),
                paste0("ahischshr", c("15_19", "20_24", "25_29"), "m"),
                paste0("ahischshr", c("15_19", "20_24", "25_29",
                                      "30_34", "35_39"), "f"),
                paste0("ahischshr", c("15_19", "20_24", "25_29",
                                      "30_34", "35_39"), "m"))
) %>%
  mutate(share_in_range = map2_dbl(pooled, parts, in_range, data = df11))

print(bin_check %>% select(pooled, share_in_range))
stopifnot(all(bin_check$share_in_range > 0.99))

# 3. Bandwidths ----------------------------------------------------------------

# 3a. IK bandwidth per outcome, on that outcome's own replication sample
h_ext <- c(
  sapply(filter(cohorts, year == 2000)$educ_f, ik, data = df),
  sapply(filter(cohorts, year == 2000)$educ_m, ik, data = df),
  sapply(filter(cohorts, year == 2011)$educ_f, ik, data = df11),
  sapply(filter(cohorts, year == 2011)$educ_m, ik, data = df11),
  sapply(nevm_vars, ik, data = df11)
)

# Must agree with 03_bandwidths.R wherever the outcomes overlap
both <- intersect(names(h_ext), names(h_ik))
stopifnot(isTRUE(all.equal(h_ext[both], h_ik[both])))

# 3b. One common bandwidth for every cross-cohort comparison: the IK bandwidth
#     of the headline outcome (women 15-20 in 2000). Half and double as checks.
h_common <- unname(h_ik["hischshr1520f"])
h_grid   <- c(half = 0.5, main = 1, double = 2) * h_common

# 4. RD Estimates --------------------------------------------------------------

# Long format: one row per cohort x gender x outcome family
long <- cohorts %>%
  pivot_longer(c(educ_f, educ_m, nevm_f, nevm_m),
               names_to = c("family", "gender"), names_sep = "_",
               values_to = "y", values_drop_na = TRUE) %>%
  mutate(gender = if_else(gender == "f", "Women", "Men"))

# 4a. IK bandwidth, each outcome on its own year's replication sample.
#     For ahischshr15_19 and nevm15_19 this reproduces Table VII exactly.
est_ik <- long %>%
  mutate(res = pmap(list(y, year), \(y, year)
                    rd_est(if (year == 2000) df else df11, y = y, h = h_ext[[y]],
                           controls = if (year == 2000) controls else controls_c))) %>%
  unnest(res) %>%
  mutate(spec = "IK, own sample")

chk <- est_ik %>% filter(y %in% c("hischshr1520f", "ahischshr15_19f"))
stopifnot(chk$n == c(1020, 879))   # Table II and Table VII sample sizes

# 4b. Common bandwidth on the common sample (main spec, plus h/2 and 2h)
est_common <- expand_grid(long, mult = names(h_grid)) %>%
  mutate(res = map2(y, mult, \(y, mult)
                    rd_est(dfc, y = y, h = h_grid[[mult]],
                           controls = controls_c))) %>%
  unnest(res) %>%
  mutate(spec = paste0("Common h (", mult, "), common sample"))

# Control-side mean inside the window, to express effects relative to a level
ctrl_mean <- function(y, h) mean(dfc[[y]][abs(dfc$iwm94) < h & dfc$i94 == 0])

est_common <- est_common %>%
  mutate(mean_ctrl = map2_dbl(y, h, ctrl_mean),
         rel       = coef / mean_ctrl)

e2_raw <- bind_rows(est_ik, est_common) %>%
  select(spec, family, year, bin, gender, y, born_lo, born_hi, age94_lo,
         age94_hi, in_term, exposure, coef, se, pval, n, h, mean_y,
         mean_ctrl, rel)

write_csv(e2_raw, file.path(path_tables, "ext2_estimates_raw.csv"))

# 5. Tests of Equal Effects ----------------------------------------------------

# sur_test() was written for female vs male. It compares the i94 coefficient
# across any two outcomes, so here the two "equations" are two cohorts.
# Estimated on the common sample at the common bandwidth; diff = second - first.

pairs <- tribble(
  ~panel, ~label,                                      ~first,           ~second,
  # Panel A: the same cohort observed in 2000 and again in 2011
  "A", "15-20 in 2000 to 25-29 in 2011",               "hischshr1520",   "ahischshr25_29",
  "A", "21-25 in 2000 to 30-34 in 2011",               "hischshr2125",   "ahischshr30_34",
  "A", "26-30 in 2000 to 35-39 in 2011",               "hischshr2630",   "ahischshr35_39",
  "A", "31-64 in 2000 to 40+ in 2011 (placebo)",       "hischshr3164",   "ahischshr40",
  # Panel B: within 2011, cohorts schooled after the term, during it, and before
  "B", "20-24 minus 25-29 (after minus during)",       "ahischshr25_29", "ahischshr20_24",
  "B", "15-19 minus 25-29 (after minus during)",       "ahischshr25_29", "ahischshr15_19",
  "B", "25-29 minus 40+ (during minus placebo)",       "ahischshr40",    "ahischshr25_29",
  "B", "20-24 minus 40+ (after minus placebo)",        "ahischshr40",    "ahischshr20_24",
  # Panel C: within 2000, the cohort differencing of Meyersson's Table S.VI
  "C", "15-20 minus 31-64 (during minus placebo)",     "hischshr3164",   "hischshr1520",
  "C", "15-20 minus 26-30 (during minus before)",      "hischshr2630",   "hischshr1520"
)

e2_tests <- expand_grid(pairs, g = c("f", "m")) %>%
  mutate(res = pmap(list(first, second, g), \(first, second, g)
                    sur_test(dfc, yf = paste0(second, g), ym = paste0(first, g),
                             hf = h_common, hm = h_common,
                             controls = controls_c))) %>%
  unnest(res) %>%
  mutate(gender = if_else(g == "f", "Women", "Men")) %>%
  select(panel, label, gender, first, second, diff, se_diff, p_sur)

write_csv(e2_tests, file.path(path_tables, "ext2_tests_raw.csv"))

# 6. Figures -------------------------------------------------------------------

# x-axis: year of birth. Closed bins sit at their midpoint with a horizontal
# bar spanning the bin. Open-ended bins (31-64 and 40+) are placebo cohorts
# and are drawn at a fixed slot left of the axis break. 65+ is in the table
# only.

plot_dat <- function(fam) {
  est_common %>%
    filter(mult == "main", family == fam, bin != "65+") %>%
    mutate(
      open = bin %in% c("31-64", "40+"),
      x    = if_else(open, if_else(year == 2000, 1964.5, 1966.5),
                     (born_lo + born_hi) / 2),
      xmin = if_else(open, NA, born_lo - 0.5),
      xmax = if_else(open, NA, born_hi + 0.5),
      src  = if_else(year == 2000, "2000 census", "2011 registry"),
      gender = factor(gender, levels = c("Women", "Men"))
    )
}

# Style follows figures 1-4: theme_classic with black axis lines, navy points,
# grey confidence bars. Filled circles are the 2000 census, open circles the
# 2011 registry. The grey band with dashed edges marks birth years of
# high-school age during the 1994-99 term (born 1977-85).
term_lo <- 1976.5
term_hi <- 1985.5

cohort_plot <- function(fam, ylab) {
  d <- plot_dat(fam)
  ggplot(d, aes(x, coef)) +
    annotate("rect", xmin = term_lo, xmax = term_hi, ymin = -Inf, ymax = Inf,
             fill = "grey90") +
    geom_vline(xintercept = c(term_lo, term_hi), linetype = "dashed",
               colour = "grey40", linewidth = 0.4) +
    geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.6) +
    geom_vline(xintercept = 1968.5, linetype = "dotted", colour = "grey50") +
    # horizontal line: the birth years the bin covers
    geom_segment(aes(x = xmin, xend = xmax, yend = coef),
                 colour = "navyblue", linewidth = 0.3, alpha = 0.6,
                 na.rm = TRUE) +
    # 95% confidence interval
    geom_errorbar(aes(ymin = coef - 1.96 * se, ymax = coef + 1.96 * se),
                  width = 0.9, linewidth = 0.3, colour = "grey30") +
    geom_point(aes(fill = src), shape = 21, size = 2.2,
               colour = "navyblue", stroke = 0.8) +
    facet_wrap(~ gender) +
    scale_fill_manual(values = c("2000 census" = "navyblue",
                                 "2011 registry" = "white")) +
    scale_x_continuous(
      limits = c(1963, 1997.5),
      breaks = c(1965.5, seq(1972, 1996, 4)),
      labels = c("<1970\n(25+)", paste0(seq(1972, 1996, 4), "\n(",
                                        1994 - seq(1972, 1996, 4), ")"))) +
    labs(x = "Year of birth (age in 1994)", y = ylab, fill = NULL) +
    theme_classic(base_size = 11) +
    theme(axis.line  = element_line(colour = "black", linewidth = 0.4),
          axis.ticks = element_line(colour = "black", linewidth = 0.4),
          strip.background = element_blank(),
          strip.text = element_text(size = 11),
          panel.spacing = unit(1.5, "lines"),
          axis.text.x = element_text(size = 7.5),
          legend.position = "bottom", legend.margin = margin(0, 0, 0, 0))
}

fig_e2  <- cohort_plot("educ", "RD estimate: share with high school")
fig_e2b <- cohort_plot("nevm", "RD estimate: share never married")

for (ext in c("png", "pdf")) {
  ggsave(file.path(path_figures, paste0("figure_ext2.", ext)),
         fig_e2, width = 7.5, height = 3.8, dpi = 300)
  ggsave(file.path(path_figures, paste0("figure_ext2_nevm.", ext)),
         fig_e2b, width = 7.5, height = 3.8, dpi = 300)
}

# 7. Formatted Tables ----------------------------------------------------------

# Same helpers as 11_table2.R, repeated so this script also runs on its own
f3    <- function(x) sprintf("%.3f", x)
paren <- function(x) paste0("(", f3(x), ")")
stars <- function(p) case_when(p < 0.01 ~ "***", p < 0.05 ~ "**",
                               p < 0.10 ~ "*", TRUE ~ "")

# 7a. Table E2: estimates by cohort (common bandwidth, common sample).
#     One coefficient row and one standard error row per cohort, oldest first.
e2_main <- est_common %>%
  filter(mult == "main", family == "educ", bin != "65+") %>%
  mutate(
    born  = if_else(bin == "40+", "1971 or earlier",
                    paste0(born_lo, "-", born_hi)),
    age94 = case_when(bin == "40+"   ~ "23+",
                      age94_lo < 0   ~ paste0("Not born-", age94_hi),
                      TRUE           ~ paste0(age94_lo, "-", age94_hi)),
    source   = paste0(year, ", ", bin),
    schooled = str_extract(exposure, "^\\w+"),
    est      = paste0(f3(coef), stars(pval)),
    se_txt   = paren(se),
    rel_txt  = sprintf("%.1f", 100 * rel)
  )

e2_wide <- e2_main %>%
  select(born_hi, year, born, age94, source, schooled, gender,
         est, se_txt, rel_txt) %>%
  pivot_wider(names_from = gender, values_from = c(est, se_txt, rel_txt)) %>%
  arrange(born_hi, year)

e2_tab <- e2_wide %>%
  pmap_dfr(\(born, age94, source, schooled, est_Women, est_Men,
             se_txt_Women, se_txt_Men, rel_txt_Women, ...)
    tibble(`Born`            = c(born, ""),
           `Age in 1994`     = c(age94, ""),
           `Source, age bin` = c(source, ""),
           `Schooled`        = c(schooled, ""),
           `Women`           = c(est_Women, se_txt_Women),
           `Men`             = c(est_Men, se_txt_Men),
           `Women, % of control mean` = c(rel_txt_Women, ""))) %>%
  bind_rows(
    tibble(`Born` = c("Bandwidth", "Observations"),
           `Women` = c(f3(h_common), as.character(unique(e2_main$n))),
           `Men`   = c(f3(h_common), as.character(unique(e2_main$n))))
  ) %>%
  mutate(across(everything(), \(x) replace_na(x, "")))

save_docx(e2_tab, "table_ext2_estimates.docx")

# 7b. Table E3: tests of equal effects, in three panels
panel_names <- c(A = "Panel A: Same cohort, 2011 minus 2000",
                 B = "Panel B: Between cohorts within 2011",
                 C = "Panel C: Between cohorts within 2000")

e3_wide <- e2_tests %>%
  mutate(panel = panel_names[panel],
         est   = f3(diff), se_txt = paren(se_diff), p = sprintf("%.2f", p_sur)) %>%
  select(panel, label, gender, est, se_txt, p) %>%
  pivot_wider(names_from = gender, values_from = c(est, se_txt, p))

e3_tab <- e3_wide %>%
  group_split(panel) %>%
  map_dfr(\(g) bind_rows(
    tibble(Comparison = g$panel[1]),
    pmap_dfr(g, \(label, est_Women, est_Men, se_txt_Women, se_txt_Men,
                  p_Women, p_Men, ...)
      tibble(Comparison      = c(label, ""),
             `Women`         = c(est_Women, se_txt_Women),
             `p-value`       = c(p_Women, ""),
             `Men`           = c(est_Men, se_txt_Men),
             `p-value `      = c(p_Men, ""))))) %>%
  mutate(across(everything(), \(x) replace_na(x, "")))

save_docx(e3_tab, "table_ext2_tests.docx",
          bold_rows = which(startsWith(e3_tab$Comparison, "Panel")))

message("30_extension2 done: ", nrow(e2_raw), " estimates, ",
        nrow(e2_tests), " tests, 2 figures, 2 tables.")
