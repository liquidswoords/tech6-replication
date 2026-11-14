# ==============================================================================
# 40_ext2_prep.R : Prepare data for extension 2 (schooling chain)
#
# Question: at which step of schooling did Islamic rule raise girls'
# education, and did the girls it pulled in keep going?
#
# Model: schooling as a sequence of hurdles for the 15-20 cohort in 2000.
#
#  All 2000 shares are attainment BY 2000. Part D uses 2011 bands of the 
#  same birth years.
#
#
# ==============================================================================

# 1. Load ----------------------------------------------------------------------

stopifnot(exists("h_ik"), exists("ik"), exists("controls"))

df   <- readRDS(file.path(path_derived, "analysis.rds"))
df11 <- readRDS(file.path(path_derived, "analysis_2011.rds"))

# 2. Build Chain Variables -----------------------------------------------------

# safe_log(): log that returns NA for zero or missing input
safe_log <- function(x) if_else(is.finite(x) & x > 0, log(x), NA_real_)

# add_chain(): stage shares, continuation rates and logs for one gender
#   g   "f" or "m"
add_chain <- function(data, g) {
  v <- function(stub) paste0(stub, "1520", g)
  data %>%
    mutate(

      # Stage shares 
      "pi1{g}"  := .data[[v("pshr")]],
      "pi2{g}"  := .data[[v("jhischshr")]],
      "pi3{g}"  := .data[[v("hischshr")]],
      "piv2{g}" := .data[[v("jhivshr")]],
      "piv3{g}" := .data[[v("hvshr")]],
      "piall3{g}" := .data[[paste0("pi3", g)]] + .data[[paste0("piv3", g)]],

      # Continuation rates
      "p2{g}"   := .data[[paste0("pi2", g)]] / .data[[paste0("pi1", g)]],
      "p3{g}"   := .data[[paste0("pi3", g)]] / .data[[paste0("pi2", g)]],

      # Logs
      "lp1{g}"  := safe_log(.data[[paste0("pi1", g)]]),
      "lp2{g}"  := safe_log(.data[[paste0("p2", g)]]),
      "lp3{g}"  := safe_log(.data[[paste0("p3", g)]]),
      "lpi3{g}" := safe_log(.data[[paste0("pi3", g)]]),

      # Acceptable towns for the chain model
      "chain_ok{g}" := !is.na(.data[[paste0("lp1", g)]]) &
                       !is.na(.data[[paste0("lp2", g)]]) &
                       !is.na(.data[[paste0("lp3", g)]]) &
                       .data[[paste0("lp2", g)]] <= 1e-9 &
                       .data[[paste0("lp3", g)]] <= 1e-9
    )
}

df_ext2 <- df %>%
  add_chain("f") %>%
  add_chain("m")

# 3. Checks --------------------------------------------------------------------

# Nesting: each stage should be a subset of the one before (NA ignored).
# Known exceptions: 2 towns where men's middle share exceeds primary
# (0.505 vs 0.465 and 0.400 vs 0.372). Treated as data errors and
# excluded from the chain via chain_ok. Any other violation stops the script.
tol <- 1e-9
n_violate <- function(hi, lo) sum(df_ext2[[hi]] > df_ext2[[lo]] + tol, na.rm = TRUE)

nest_report <- map_dfr(c("f", "m"), function(g) tibble(
  gender        = g,
  mid_gt_prim   = n_violate(paste0("pi2",  g), paste0("pi1",  g)),
  high_gt_mid   = n_violate(paste0("pi3",  g), paste0("pi2",  g)),
  vhigh_gt_vmid = n_violate(paste0("piv3", g), paste0("piv2", g))
))

print(nest_report)
stopifnot(
  nest_report$mid_gt_prim   == c(0, 2),
  nest_report$high_gt_mid   == 0,
  nest_report$vhigh_gt_vmid == 0
)

# Identity holds row by row (before any regression)
for (g in c("f", "m")) {
  ok  <- df_ext2[[paste0("chain_ok", g)]]
  lhs <- df_ext2[[paste0("lpi3", g)]][ok]
  rhs <- (df_ext2[[paste0("lp1", g)]] + df_ext2[[paste0("lp2", g)]] +
          df_ext2[[paste0("lp3", g)]])[ok]
  stopifnot(isTRUE(all.equal(lhs, rhs, tolerance = 1e-10)))
}

# 4. Bandwidths ----------------------------------------------------------------

# (a) Chain: common h per gender (Table II, high school)
h_chain <- c(f = unname(h_ik["hischshr1520f"]),
             m = unname(h_ik["hischshr1520m"]))

# (b) Table S.III: IK per stage outcome, used only to replicate S.III.
# Paper values: women .251 .286 .153 .240 .152, men .340 .294 .204 .323 .184
# (primary, middle, voc. middle, high, voc. high).
# All match to 3 decimals except primary (ours: f .254, m .315). Primary is
# the only stage outcome with missing values (4 towns): our IKbandwidth()
# drops them, rdob.ado does not (see 03_bandwidths.R), which shifts h.
s3_outcomes <- c(outer(c("pshr1520", "jhischshr1520", "jhivshr1520",
                         "hischshr1520", "hvshr1520"),
                       c("f", "m"), paste0))
h_s3 <- sapply(s3_outcomes, ik, data = df)

s3_paper <- c(pshr1520f = .251, jhischshr1520f = .286, jhivshr1520f = .153,
              hischshr1520f = .240, hvshr1520f = .152,
              pshr1520m = .340, jhischshr1520m = .294, jhivshr1520m = .204,
              hischshr1520m = .323, hvshr1520m = .184)
s3_match <- abs(round(h_s3, 3) - s3_paper[names(h_s3)]) < 1e-9
stopifnot(all(s3_match[!grepl("^pshr", names(s3_match))]))

# (c) Part D: 2011 high school by birth band, on the Table VII sample
#   25-29 in 2011 = born about 1982-86 (mostly the younger group)
#   30-34 in 2011 = born about 1977-81 (older group plus earlier years)
y2011_outcomes <- c("ahischshr25_29f", "ahischshr25_29m",
                    "ahischshr30_34f", "ahischshr30_34m")
stopifnot(!anyNA(df11[y2011_outcomes]))
h_y2011 <- sapply(y2011_outcomes, ik, data = df11)

h_ext2 <- list(chain = h_chain, s3 = h_s3, y2011 = h_y2011)

# 5. Sample Report -------------------------------------------------------------

# Towns inside each gender's window, and how many the chain loses
# (missing primary share, or a zero share that makes a log undefined)
sample_report <- map_dfr(c("f", "m"), function(g) {
  w <- df_ext2 %>% filter(abs(iwm94) < h_chain[[g]])
  tibble(
    gender       = g,
    h            = h_chain[[g]],
    in_window    = nrow(w),
    islamic      = sum(w$i94),
    chain_ok     = sum(w[[paste0("chain_ok", g)]]),
    dropped      = in_window - chain_ok,
    mean_pi1     = mean(w[[paste0("pi1",  g)]], na.rm = TRUE),
    mean_pi2     = mean(w[[paste0("pi2",  g)]], na.rm = TRUE),
    mean_pi3     = mean(w[[paste0("pi3",  g)]], na.rm = TRUE),
    mean_piv3    = mean(w[[paste0("piv3", g)]], na.rm = TRUE)
  )
})

print(sample_report)

# 6. 2011 outcomes -------------------------------------------------------------



df11 <- readRDS(file.path(path_derived, "analysis_2011.rds"))

# Pick out 2011 outcomes
outcomes_ext2_2011 <- c("ahischshr25_29f", "ahischshr25_29m",
                        "ahischshr30_34f", "ahischshr30_34m")


# Seperate bandwidths for 2011
h_ext3_2011 <- sapply(outcomes_ext2_2011, ik, data = df11)
h_ext3_2011_stored <- c(ahischshr25_29f = 0.2786837, ahischshr25_29m = 0.1998223,
                        ahischshr30_34f = 0.2044676, ahischshr30_34m = 0.3171075)

# 7. Save ----------------------------------------------------------------------


# Save data and bandwidths to tech6-replication/data/derived/...
saveRDS(df_ext3, file.path(path_derived, "ext2.rds"))
saveRDS(df11,    file.path(path_derived, "ext2_2011.rds"))
saveRDS(list(h2000 = h_ext3, h2011 = h_ext3_2011),
        file.path(path_derived, "ext2_h.rds"))


message("40_ext2_prep done: chain variables for ", nrow(df_ext3),
        " towns; h_f = ", round(h_ext3[["f"]], 4),
        ", h_m = ", round(h_ext3[["m"]], 4),
        "; 2011 sample ", nrow(df11), " towns.")
