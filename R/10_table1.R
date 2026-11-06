# ==============================================================================
# 10_table1.R 
#
# Reproduces Table I from paper
# Table I: Summary Statistics
#
# Saves as raw .csv and .docx in tech6-replication/output/tables
#
# ==============================================================================

# 1. Read Data -----------------------------------------------------------------

df <- readRDS(file.path(path_derived, "analysis.rds"))

# 2. Name Columns --------------------------------------------------------------

t1_vars <- c(
  hischshr1520f = "Share women aged 15-20 with high school education",
  hischshr1520m = "Share men aged 15-20 with high school education",
  i94           = "Islamic mayor in 1994",
  vshr_islam1994  = "Islamic vote share in 1994",
  partycount    = "Number of parties receiving votes in 1994",
  lpop1994      = "Log population in 1994",
  ageshr19      = "Population share below 19 in 2000",
  ageshr60      = "Population share above 60 in 2000",
  sexr          = "Gender Ratio in 2000",
  shhs          = "Household size in 2000",
  merkezi       = "District center",
  merkezp       = "Province center",
  subbuyuk      = "Sub-metro center",
  buyuk         = "Metro center"
)

# 3.Statistics Per Variable ----------------------------------------------------

# t1_stats(): Calculate Relevant Statistics for Variables from Table I
#   v         Relevant variable
#   welch    Should we use welch in SE calculation, table in paper is not welch.

t1_stats <- function(data, v, welch=FALSE) {
  x  <- data[[v]]
  x1 <- x[data$i94 == 1]                   # Islamic Mayor
  x0 <- x[data$i94 == 0]                   # Secular Mayor
  n1 <- length(x1)
  n0 <- length(x0)
  
  if (v == "i94") {
    diff <- 1; se <- 0; p <- NA_real_ # Exclude SE calculation for treatment row
  } else {
    tt   <- t.test(x1, x0, var.equal = !welch) # Pooled SE as in Paper
    diff <- unname(tt$estimate[1]-tt$estimate[2])
    se   <- tt$stderr
    p    <- tt$p.value
  }
  
  tibble(var = v,
         mean_all = mean(x),  sd_all = sd(x),
         mean_isl = mean(x1), sd_isl = sd(x1),
         mean_sec = mean(x0), sd_sec = sd(x0),
         diff = diff, se = se, p = p)
}

t1 <- map_dfr(names(t1_vars), t1_stats, data = df)

# 4. Format Table --------------------------------------------------------------

# Formatting Helper Functions
f3 <- function(x) sprintf("%.3f", x)
paren <- function(x) paste0("(", f3(x), ")")
stars <- function(p) case_when(is.na(p) ~ "",
                               p < 0.01 ~ "***",
                               p < 0.05 ~ "**",
                               p < 0.10 ~ "*",
                               TRUE     ~ "")
top <- t1 %>%
  transmute(k = row_number(), j=1,
            Variable   = unname(t1_vars[var]),
            All        = f3(mean_all),
            Islamic    = f3(mean_isl),
            Secular    = f3(mean_sec),
            Difference = paste0(f3(diff), stars(p)))

bot <- t1 %>%
  transmute(k = row_number(), j=2,
              Variable   = "",
              All        = paren(sd_all),
              Islamic    = paren(sd_isl),
              Secular    = paren(sd_sec),
              Difference = paren(se))

obs <- tibble(Variable   = "Observations",
              All        = as.character(nrow(df)),
              Islamic    = as.character(sum(df$i94 == 1)),
              Secular    = as.character(sum(df$i94 == 0)),
              Difference = as.character(nrow(df)))

t1_tab <- bind_rows(top, bot) %>%
  arrange(k, j) %>% 
  select(-k, -j) %>%
  bind_rows(obs)

# 5. Save ----------------------------------------------------------------------

# CSV Saved with Raw Values
write_csv(t1, file.path(path_tables, "table1_raw.csv"))

# Save DOCX pastable to MS Word
save_docx(t1_tab, "table1.docx")

message("10_table1 done: Table I saved to output/tables.")


