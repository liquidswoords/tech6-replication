# ==============================================================================

# 42_ext3_fuzzy.R : Extension 3 - (who was pulled in?)

# Fuzzy RD: Islamic rule (i94) instruments middle school completion (pi2),
# outcome is high school completion (pi3), both by 2000.



# ==============================================================================

# 1. Load ----------------------------------------------------------------------

df_ext3 <- readRDS(file.path(path_derived, "ext2.rds"))
h_ext3  <- readRDS(file.path(path_derived, "ext2_h.rds"))$h2000

mults <- c(1, 0.5, 2)

# 2. Helpers -------------------------------------------------------------------

# rd_model(): Returns the fit 
#   y   outcome name; h bandwidth
rd_model <- function(data, y, h, controls) {
  d <- data %>%
    filter(abs(iwm94) < h, !is.na(.data[[y]])) %>%
    mutate(x = iwm94 / h)
  f <- as.formula(paste(y, "~ i94 + I(x^1) + i94:I(x^1) +",
                        paste(controls, collapse = " + "), "| prov"))
  list(model = feols(f, data = d, cluster = ~pcode, fixef.rm = "none"),
       data  = d)
}



# cutoff_limits(): secular-side and Islamic-side limits at x = 0
# Averaged over window towns
cutoff_limits <- function(fit) {
  nd   <- fit$data %>% mutate(x = 0, i94 = 0)
  lim0 <- mean(predict(fit$model, newdata = nd), na.rm = TRUE)
  tau  <- unname(coef(fit$model)["i94"])
  c(lim0 = lim0, lim1 = lim0 + tau)
}

# fuzzy_rd(): 2SLS, instrument i94
fuzzy_rd <- function(data, y, d_endo, h, controls) {
  dd <- data %>%
    filter(abs(iwm94) < h, !is.na(.data[[y]]), !is.na(.data[[d_endo]])) %>%
    mutate(x = iwm94 / h)
  f <- as.formula(paste(y, "~ I(x^1) + i94:I(x^1) +",
                        paste(controls, collapse = " + "),
                        "| prov |", d_endo, "~ i94"))
  feols(f, data = dd, cluster = ~pcode, fixef.rm = "none")
}



# ar_ci(): Anderson-Rubin 95% CI for rho by test inversion.
ar_ci <- function(data, h, g, grid = seq(-1, 3, by = 0.01)) {
  y3 <- paste0("pi3", g); y2 <- paste0("pi2", g)
  acc <- map_lgl(grid, function(r0) {
    d <- data %>% mutate(ar_y = .data[[y3]] - r0 * .data[[y2]])
    e <- rd_est(d, y = "ar_y", h = h, p = 1, controls = controls, fe = TRUE)
    abs(e$coef / e$se) < qnorm(0.975)
  })
  if (!any(acc)) return(c(lo = NA_real_, hi = NA_real_, bounded = NA))
  kept <- grid[acc]
  c(lo = min(kept), hi = max(kept),
    bounded = !(acc[1] || acc[length(acc)]))
}


# 3. Women ---------------------------------------------------------------------

run_women <- function(mult) {
  h <- mult * h_ext3[["f"]]
  
  d <- df_ext3 %>% filter(!is.na(pi2f), !is.na(pi3f))
  
  fs <- rd_model(d, "pi2f", h, controls)    # first stage
  rf <- rd_model(d, "pi3f", h, controls)    # reduced form
  iv <- fuzzy_rd(d, "pi3f", "pi2f", h, controls)
  
  fs_coef <- unname(coef(fs$model)["i94"]); fs_se <- unname(se(fs$model)["i94"])
  rf_coef <- unname(coef(rf$model)["i94"]); rf_se <- unname(se(rf$model)["i94"])
  rho     <- unname(coef(iv)["fit_pi2f"]);  rho_se <- unname(se(iv)["fit_pi2f"])
  
  
  # Checks if 2sls is same as wald (it should be)
  stopifnot(abs(rho - rf_coef / fs_coef) < 1e-8)
  
  l2 <- cutoff_limits(fs)
  l3 <- cutoff_limits(rf)
  
  #AR CI
  ar <- if (mult == 1) ar_ci(d, h, "f") else c(lo = NA, hi = NA, bounded = NA)
  
  tibble(
    g = "f", mult = mult, h = h, n = nobs(iv),
    fs = fs_coef, fs_se = fs_se, fs_F = (fs_coef / fs_se)^2,
    rf = rf_coef, rf_se = rf_se,
    rho = rho, rho_se = rho_se,
    ar_lo = ar[["lo"]], ar_hi = ar[["hi"]], 
                ar_bounded = as.logical(ar[["bounded"]]),
    pi2_0 = l2[["lim0"]], pi2_1 = l2[["lim1"]],
    pi3_0 = l3[["lim0"]], pi3_1 = l3[["lim1"]],
    pbar0 = l3[["lim0"]] / l2[["lim0"]],
    rho_minus_pbar0 = rho - l3[["lim0"]] / l2[["lim0"]])
}

women <- map_dfr(mults, run_women)


# 4. Men First stage only ------------------------------------------------------

run_men <- function(mult) {
  h  <- mult * h_ext3[["m"]]
  d  <- df_ext3 %>% filter(!is.na(pi2m), !is.na(pi3m))
  fs <- rd_model(d, "pi2m", h, controls)
  fs_coef <- unname(coef(fs$model)["i94"]); fs_se <- unname(se(fs$model)["i94"])
  l2 <- cutoff_limits(fs)
  tibble(g = "m", mult = mult, h = h, n = nobs(fs$model),
         fs = fs_coef, fs_se = fs_se, fs_F = (fs_coef / fs_se)^2,
         pi2_0 = l2[["lim0"]], pi2_1 = l2[["lim1"]])
}

men <- map_dfr(mults, run_men)


fuzzy <- bind_rows(women, men) %>% relocate(g, mult)

print(fuzzy %>% filter(mult == 1) %>%
        select(g, n, fs, fs_se, fs_F, rf, rho, rho_se, ar_lo, ar_hi, pbar0))


# 5. Save ----------------------------------------------------------------------


write_csv(fuzzy, file.path(path_extension2, "ext3_fuzzy_raw.csv"))

message("42_ext3_fuzzy done: rho_C (women, h) = ",
        round(women$rho[women$mult == 1], 3),
        ", pbar0 = ", round(women$pbar0[women$mult == 1], 3),
        ", first-stage F = ", round(women$fs_F[women$mult == 1], 1), ".")

