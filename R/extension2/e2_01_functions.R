# ==============================================================================
# e2_01_functions.R : Shared functions for Extension 2 (heterogeneity)
#
# Contains:
#   e2_config()      - read config/extension2.json
#   load_e2()        - analysis sample + Python indices, merged by mun_id
#   het_formula()    - formula of the fully interacted RD
#   fit_het()        - fully interacted RD model (returns model + data)
#   rd_het()         - treatment terms of fit_het() (delta = change in
#                      effect per 1 SD of the moderator)
#   het_curve()      - tau(z) = tau + delta * z with 95% CI, one moderator
#   support_table()  - Islamic-mayor and secular towns per moderator group
#
# Requires: 00_packages.R (tidyverse, fixest, here) and the Python step
#           python/extension2/build_indices.py (writes e2_indices.csv)
# ==============================================================================

# 1. Config --------------------------------------------------------------------

e2_config <- function() {
  jsonlite::read_json(here::here("config", "extension2.json"),
                      simplifyVector = TRUE)
}

# 2. Load Sample + Indices -----------------------------------------------------

# load_e2(): main sample (2629 towns) with the indices from Python.
#   mun_id is created exactly as in e2_00_export.R (row order of analysis.rds),
#   so the merge is one-to-one.
#   pious = 1994 Islamic vote share (observed, not predicted).

load_e2 <- function(cfg = e2_config()) {
  
  df  <- readRDS(file.path(path_derived, "analysis.rds")) %>%
    mutate(mun_id = row_number())
  
  idx <- read.csv(here::here(cfg$paths$indices))
  index_cols <- setdiff(names(idx), cfg$id)
  
  out <- df %>%
    left_join(idx, by = cfg$id) %>%
    mutate(pious = .data[[cfg$pious_var]])
  
  stopifnot(nrow(out) == 2629,
            all(!is.na(out[, index_cols])))
  
  message("load_e2: ", nrow(out), " towns; indices: ",
          paste(index_cols, collapse = ", "))
  out
}

# 3. Fully Interacted RD -------------------------------------------------------

# het_formula(): y ~ i94 * x * (moderators) + controls | prov
#   i94 * x * m expands to every product of treatment, running variable and
#   moderator: separate slopes on each side of the cutoff AND along the
#   moderator, so delta (i94:m) is not confused with slope differences.
#   With two moderators, m = m1 * m2 also gives their interaction (m1:m2).

het_formula <- function(y, mods, controls = NULL, fe = TRUE) {
  m   <- paste0("(", paste(mods, collapse = " * "), ")")
  rhs <- paste(c(paste("i94 * x *", m), controls), collapse = " + ")
  f   <- paste(y, "~", rhs)
  if (fe) f <- paste(f, "| prov")
  as.formula(f)
}

# fit_het(): local linear RD where the effect varies along the moderator(s).
#   y          outcome name
#   z          one or two moderator names (e.g. "barriers", or c("poor", "pious"))
#   h          bandwidth (towns with |iwm94| < h)
#   controls   control names; any moderator is dropped from them (no duplicates)
#   fe         province fixed effects (prov)
#   cluster    variable to cluster SEs on (pcode, as in the replication)
#
# Moderators are standardised WITHIN the window (mean 0, sd 1), so:
#   tau   = effect at the average town in the window
#   delta = change in the effect per 1 SD of the moderator

fit_het <- function(data, y, z, h, controls = NULL, fe = TRUE,
                    cluster = "pcode") {
  
  stopifnot(length(z) %in% 1:2)
  
  # 1. Window and standardised moderators
  d <- data %>%
    filter(abs(iwm94) < h) %>%
    drop_na(all_of(c(y, z))) %>%
    mutate(x = iwm94 / h)
  
  mods <- paste0(z, "_std")
  for (k in seq_along(z)) {
    d[[mods[k]]] <- as.numeric(scale(d[[z[k]]]))
  }
  
  # 2. Estimate
  controls <- setdiff(controls, z)
  m <- feols(het_formula(y, mods, controls, fe), data = d,
             cluster = cluster, fixef.rm = "none")
  
  list(model = m, data = d, z = z, mods = mods, y = y, h = h)
}

# rd_het(): fit_het() and keep the treatment terms (i94 and its interactions
#   with the moderators, not those involving x, which are slope terms)

rd_het <- function(data, y, z, h, controls = NULL, fe = TRUE,
                   cluster = "pcode") {
  
  f  <- fit_het(data, y, z, h, controls, fe, cluster)
  m  <- f$model
  ct <- coeftable(m)
  keep <- grepl("^i94", rownames(ct)) & !grepl("(^|:)x(:|$)", rownames(ct))
  
  tibble(
    outcome   = y,
    moderator = paste(z, collapse = " x "),
    term      = rownames(ct)[keep],
    estimate  = ct[keep, "Estimate"],
    se        = ct[keep, "Std. Error"],
    pval      = pvalue(m)[keep],
    mde       = 2.8 * ct[keep, "Std. Error"],   # 80% power, 5% two-sided
    n         = nobs(m),
    n_treated = sum(f$data$i94),
    h         = h
  )
}

# het_curve(): the effect along ONE moderator, tau(z) = tau + delta * z,
#   with a pointwise 95% CI from the clustered covariance matrix:
#   Var(tau(z)) = Var(tau) + z^2 Var(delta) + 2 z Cov(tau, delta)
#   z_grid is in SD units of the moderator (within the window).

het_curve <- function(fit, z_grid = seq(-2, 2, by = 0.05)) {
  stopifnot(length(fit$mods) == 1)
  b  <- coef(fit$model)
  V  <- vcov(fit$model)
  t1 <- "i94"
  t2 <- paste0("i94:", fit$mods)
  
  est <- b[t1] + b[t2] * z_grid
  se  <- sqrt(V[t1, t1] + z_grid^2 * V[t2, t2] + 2 * z_grid * V[t1, t2])
  
  tibble(outcome = fit$y, moderator = fit$z, z = z_grid,
         estimate = unname(est), se = unname(se),
         lo = estimate - 1.96 * se, hi = estimate + 1.96 * se)
}

# 4. Support -------------------------------------------------------------------

# support_table(): within the window, how many Islamic-mayor (i94 = 1) and
#   secular towns fall in each group of the moderator. Every cell needs a
#   reasonable number of both for the interaction to be identified there.

support_table <- function(data, z, h, groups = 3) {
  data %>%
    filter(abs(iwm94) < h, !is.na(.data[[z]])) %>%
    mutate(group = ntile(.data[[z]], groups)) %>%
    group_by(group) %>%
    summarise(moderator = z,
              z_min = min(.data[[z]]), z_max = max(.data[[z]]),
              n_towns = n(), n_islamic = sum(i94), n_secular = sum(i94 == 0),
              share_islamic = mean(i94), .groups = "drop")
}