# ==============================================================================
# e2_02_table5.R : Reproduces Table V from paper 
#
# Table V: Heterogeneity by median splits 
#   - panel (a): split at median literacy share
#   - panel (b): split at median share of religious buildings (2000)
#   - panel (c): split at median Islamic vote share (1994)
# ==============================================================================

source(here::here("R", "00_packages.R"))
source(here::here("R", "02_functions.R"))
source(here::here("R", "extension2", "e2_01_functions.R"))


# 1. Load Data -----------------------------------------------------------------


df <- read_dta(file.path(path_raw, "regdata0.dta")) %>%
  zap_labels() %>%
  mutate(prov = max.col(as.matrix(select(., starts_with("pd_")))))
cfg <- e2_config()
h   <- cfg$h_main                                   


# 2. Spec Grid -----------------------------------------------------------------


t5_controls <- c("vshr_islam1994", "partycount", "shhs", "lpop1994",
                 "ageshr19", "ageshr60", "sexr",
                 "merkezi", "merkezp", "subbuyuk", "buyuk")

split_vars <- c("litshrt", "relarshr00", "vshr_islam1994")

stopifnot(all(split_vars %in% names(df)))

specs <- expand_grid(
  y = c("hischshr1520f", "hischshr1520m"),
  z = split_vars
)


# 3. Estimation Function -------------------------------------------------------

# t5_cell(): one split of Table V
#   y     outcome name
#   z     split variable
#   h     bandwidth


t5_cell <- function(data, y, z, h) {

  # (1) Window  
  ctl <- setdiff(t5_controls, z)
  d <- data %>%
    filter(abs(iwm94) < h) %>%
    drop_na(all_of(c(y, z, t5_controls))) %>%
    mutate(x = iwm94)                              
  
  # (2) Median split
  m <- as.numeric(sprintf("%.16g", median(d[[z]])))
  d <- d %>%
    mutate(half = case_when(.data[[z]] > m ~ "above",
                            .data[[z]] < m ~ "below")) %>%
    filter(!is.na(half))
  
  rhs <- paste(c("i94 * x", z, ctl), collapse = " + ")
  
  # (3) Separate RD in each half 
  by_half <- map_dfr(c("above", "below"), function(hf) {
    dh  <- filter(d, half == hf)
    fit <- feols(as.formula(paste(y, "~", rhs, "| prov")), data = dh,
                 cluster = ~pcode, fixef.rm = "none",
                 ssc = ssc(fixef.K = "full"))
    tibble(half      = hf,
           coef      = coef(fit)[["i94"]],
           se        = se(fit)[["i94"]],
           pval      = pvalue(fit)[["i94"]],
           n         = nobs(fit),
           n_islamic = sum(dh$i94),
           mean_y    = mean(dh[[y]]))
  })
  
  # (4) Test above = below
  d <- mutate(d, below = as.numeric(half == "below"))
  stacked <- feols(as.formula(paste(y, "~ (", rhs, ") * below | half^prov")),
                   data = d, cluster = ~pcode, fixef.rm = "none",
                   ssc = ssc(adj = FALSE, cluster.adj = TRUE))
  diff    <- coef(stacked)[["i94:below"]]
  diff_se <- se(stacked)[["i94:below"]]
  
  by_half %>%
    mutate(outcome = y, split = z, median = m,
           p_diff  = 2 * pnorm(-abs(diff / diff_se)))
}


# 4. Estimate ------------------------------------------------------------------

t5_raw <- specs %>%
  pmap_dfr(\(y, z) t5_cell(df, y = y, z = z, h = h)) %>%
  group_by(outcome) %>%
  mutate(p_holm = rep(p.adjust(p_diff[half == "above"], "holm"), each = 2)) %>%
  ungroup()


# 5. Format Table --------------------------------------------------------------

f3    <- function(x) sprintf("%.3f", x)
paren <- function(x) paste0("(", f3(x), ")")
stars <- function(p) case_when(p < 0.01 ~ "***", p < 0.05 ~ "**",
                               p < 0.1 ~ "*", TRUE ~ "")

panel_rows5 <- function(pan) {
  r <- pan %>%
    mutate(outcome = factor(outcome, c("hischshr1520f", "hischshr1520m")),
           half    = factor(half, c("above", "below"))) %>%
    arrange(outcome, half)
  pair <- function(x) if_else(r$half == "above", x, "")
  rbind(
    "Outcome mean"           = f3(r$mean_y),
    "Islamic mayor in 1994"  = paste0(f3(r$coef), stars(r$pval)),
    " "                      = paren(r$se),
    "p-value"                = pair(f3(r$p_diff)),
    "Bandwidth"              = f3(rep(h, 4)),
    "Observations"           = as.character(r$n),
    "Islamic-mayor towns"    = as.character(r$n_islamic),
    "p-value, Holm adjusted" = pair(f3(r$p_holm))
  )
}

title_row5 <- function(txt) matrix("", 1, 4, dimnames = list(txt, NULL))

panel_titles <- c(
  litshrt        = "Panel A: Sample split at median literacy share",
  relarshr00     = "Panel B: Sample split at median share of religious buildings",
  vshr_islam1994 = "Panel C: Sample split at median Islamic vote share"
)

t5_mat <- rbind(
  matrix(c("Women", "Women", "Men", "Men"), 1,
         dimnames = list("Outcome: High school attainment among 15-20", NULL)),
  matrix(rep(c("Above median", "Below median"), 2), 1,
         dimnames = list("Sample", NULL)),
  do.call(rbind, map(split_vars, \(z) rbind(
    title_row5(panel_titles[[z]]),
    panel_rows5(filter(t5_raw, split == z))
  )))
)

colnames(t5_mat) <- paste0("(", 1:4, ")")

t5_tab <- as_tibble(t5_mat) %>%
  mutate(Variable = rownames(t5_mat), .before = 1)


# 6. Save ----------------------------------------------------------------------

# Save CSV
write_csv(t5_raw, file.path(path_tables, "table5_raw.csv"))

# Save Word
save_docx(t5_tab, "table5.docx",
          bold_rows = which(startsWith(t5_tab$Variable, "Panel")))


message("e2_02_table5 done: Table V saved to output/tables.")