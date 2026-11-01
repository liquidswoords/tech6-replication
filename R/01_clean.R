# ==============================================================================
# 01_clean.R : Load and clean data for analysis
#
# ==============================================================================

# 1. Read Data -----------------------------------------------------------------

df <- read_dta(file.path(path_raw,"regdata0.dta")) %>%
  zap_labels()


# 2. Define Control Vector -----------------------------------------------------

# shhs is included as this reproduces table II 
# age over 60 is used, following table notes and data

controls <- c("vshr_islam1994", "partycount", "lpop1994",
              "ageshr19", "ageshr60", "sexr", "shhs",
              "merkezi", "merkezp", "subbuyuk", "buyuk")

# 3. Building Province Variable for Fixed Effects ------------------------------

# We decide to use 81 provinces (pd_*) rather than pcode, which had 67 provinces

df <- df %>%  
  mutate(prov= max.col(as.matrix(select(.,starts_with("pd_")))))


# 4. Analysis Sample -----------------------------------------------------------

# Cut the sample to the 2629 observation size in paper

df <- df %>%  
  drop_na(i94, iwm94,hischshr1520f, hischshr1520m, all_of(controls))

stopifnot(nrow(df)==2629, sum(df$i94)==315)

# 5. Save ----------------------------------------------------------------------

saveRDS(df, file.path(path_derived, "analysis.rds"))
message("01_clean done: ", nrow(df), " municipalities saved.")


