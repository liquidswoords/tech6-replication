# ==============================================================================
# e2_03_het_table.R : Main heterogeneity table for Extension 2
#
# Does the effect of an Islamic mayor differ along pre-1994 town traits?
#   Fully interacted local linear RD (h = 0.25, controls + province FE):
#     tau(z) = tau + delta * z,   z standardised within the window
#   - panel (a): barriers to schooling (predicted 1990 female HS share, flipped)
#   - panel (b): poverty (predicted literacy, flipped)
#   - panel (c): piety (Islamic vote share 1994, observed)
#   - panel (d): poverty x piety (Meyersson's "poor and pious")
#   - columns: Women, Men
#
# Saves as raw .csv and .docx in tech6-replication/output/extension2
#
# ==============================================================================


source(here::here("R", "00_packages.R"))
source(here::here("R", "02_functions.R"))
source(here::here("R", "extension2", "e2_01_functions.R"))


# 1. Load Data -----------------------------------------------------------------

cfg <- e2_config()
path_e2 <- here::here(cfg$paths$output_dir)          
dir.create(path_e2, recursive = TRUE, showWarnings = FALSE)
h   <- cfg$h_main                                 

# Flip the two predicted indices so that higher = more disadvantaged.
d <- load_e2(cfg) %>%
  mutate(barrier_idx = -barriers,
         poverty_idx = -poor)


# 2. Spec Grid -----------------------------------------------------------------

models <- tribble(
  ~model, ~title,                                              ~z,
  "a",    "Panel A: Barriers to schooling (pre-1994 index)",   "barrier_idx",
  "b",    "Panel B: Poverty (pre-1994 index)",                 "poverty_idx",
  "c",    "Panel C: Piety (Islamic vote share 1994)",          "pious",
  "d",    "Panel D: Poverty x piety",                          c("poverty_idx", "pious")
)

outcomes <- c(Women = "hischshr1520f", Men = "hischshr1520m")

controls_for <- function(z) {
  if ("pious" %in% z) setdiff(cfg$rd_controls, cfg$pious_var) else cfg$rd_controls
}

key_term <- function(z) paste0("i94:", paste(paste0(z, "_std"), collapse = ":"))


# 3. Estimate ------------------------------------------------------------------

het_raw <- expand_grid(model = models$model, y = outcomes) %>%
  left_join(select(models, model, title, z), by = "model") %>%
  pmap_dfr(\(model, y, title, z) {
    rd_het(d, y = y, z = z, h = h, controls = controls_for(z)) %>%
      mutate(model = model, title = title, key = term == key_term(z))
  })

het_raw <- het_raw %>%
  group_by(outcome) %>%
  mutate(p_holm = replace(rep(NA_real_, n()), key,
                          p.adjust(pval[key], "holm"))) %>%
  ungroup()

stopifnot(sum(het_raw$key) == 8)                   

support <- map_dfr(c("barrier_idx", "poverty_idx", "pious"), \(z)
                   support_table(d, z, h) %>% filter(group %in% c(1, 3))
)
print(support)


# 4. Format Table --------------------------------------------------------------

f3    <- function(x) sprintf("%.3f", x)
paren <- function(x) paste0("(", f3(x), ")")
stars <- function(p) case_when(p < 0.01 ~ "***", p < 0.05 ~ "**",
                               p < 0.1 ~ "*", TRUE ~ "")

term_label <- function(term) {
  case_when(
    term == "i94"                          ~ "Islamic mayor (effect at mean town)",
    term == "i94:barrier_idx_std"          ~ "x Barriers (per 1 SD)",
    term == "i94:poverty_idx_std"          ~ "x Poverty (per 1 SD)",
    term == "i94:pious_std"                ~ "x Piety (per 1 SD)",
    term == "i94:poverty_idx_std:pious_std" ~ "x Poverty x Piety",
    TRUE ~ term
  )
}

panel_rows <- function(m) {
  pan   <- filter(het_raw, model == m)
  terms <- unique(pan$term)
  cell  <- function(t, f) map_chr(outcomes, \(y) f(filter(pan, outcome == y, term == t)))
  keyf  <- function(f) map_chr(outcomes, \(y) f(filter(pan, outcome == y, key)))
  
  coef_rows <- do.call(rbind, map(terms, \(t) rbind(
    matrix(cell(t, \(r) paste0(f3(r$estimate), stars(r$pval))), 1,
           dimnames = list(term_label(t), NULL)),
    matrix(cell(t, \(r) paren(r$se)), 1, dimnames = list(" ", NULL))
  )))
  
  zz  <- models$z[[which(models$model == m)]]
  sup <- if (length(zz) == 1) {
    s <- filter(support, moderator == zz)
    paste0(s$n_islamic[s$group == 1], " / ", s$n_islamic[s$group == 3])
  } else ""
  
  rbind(
    matrix("", 1, 2, dimnames = list(models$title[models$model == m], NULL)),
    coef_rows,
    "p-value, Holm adjusted"        = keyf(\(r) f3(r$p_holm)),
    "Minimum detectable effect"     = keyf(\(r) f3(r$mde)),
    "Observations"                  = keyf(\(r) as.character(r$n)),
    "Islamic-mayor towns"           = keyf(\(r) as.character(r$n_treated)),
    "Islamic towns, low / high tercile" = rep(sup, 2)
  )
}

het_mat <- rbind(
  matrix(names(outcomes), 1,
         dimnames = list("Outcome: High school attainment among 15-20", NULL)),
  do.call(rbind, map(models$model, panel_rows))
)

colnames(het_mat) <- paste0("(", 1:2, ")")

het_tab <- as_tibble(het_mat) %>%
  mutate(Variable = rownames(het_mat), .before = 1)

print(het_tab, n = Inf)


# 5. Save ----------------------------------------------------------------------

# Save CSV
write_csv(het_raw, file.path(path_e2, "e2_het_raw.csv"))
write_csv(support, file.path(path_e2, "e2_het_support.csv"))

# Save Word
save_docx(het_tab, "e2_het_table.docx",
          bold_rows = which(startsWith(het_tab$Variable, "Panel")),
          dir = path_e2)


message("e2_03_het_table done: heterogeneity table saved to output/extension2.")

