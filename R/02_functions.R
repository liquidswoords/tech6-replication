# ==============================================================================
# 02_functions.R 
#
# ==============================================================================

# 1. RD Estimation Function ----------------------------------------------------

# rd_est(): Local polynomial RD at iwm94=0
#   y           outcome name as text (column label)
#   h           bandwidth; keeps towns within bandwidth
#   p           polynomial order per side (p=0 is comparison of means)
#   controls    vector of control names, or NULL (if no controls)
#   fe          TRUE adds province Fixed Effects (prov, 81 provinces)
#   cluster     Variable to cluster SEs on (default "pcode" 67 provinces)

rd_est <- function(data, y, h, p=1, controls=NULL, fe=TRUE, cluster = "pcode") {
  
  # 1. Keep towns inside window
  d <- data %>% filter(abs(iwm94) < h)
  
  # 2. Polynomial Terms
  poly <- if (p == 0) character(0) else paste0("I(iwm94^", 1:p, ")")
  poly <- c(poly, if (p > 0) paste0("i94:", poly))
  
  # 3. Build Formula
  rhs <- paste(c("i94", poly, controls), collapse = " + ")
  f   <- paste(y, "~", rhs)
  if (fe) f <- paste(f, "| prov")
  
  # 4. Estimate (SE clustered depending)
  m <- feols(as.formula(f), data = d, cluster = cluster,fixef.rm = "none")
  
  # 5. Return Row of Results
  tibble(
    outcome = y,
    coef    = unname(coef(m)["i94"]),
    se      = unname(se(m)["i94"]),
    n       = nobs(m),
    mean_y  = mean(d[[y]], na.rm = TRUE),
    r2      = unname(r2(m, "r2")),
    cluster = cluster,
    h       = h,
    p       = p,
    pval    = unname(pvalue(m)["i94"])
  )
}

# 2. SUR Test of Equal Treatment Effects ---------------------------------------


# sur_test(): p-value for H0: female effect = male effect (Table II Panel C)
#   yf, ym      female and male outcome names
#   hf, hm      gender-specific bandwidths (Inf = global)
#   p           polynomial order per side 
#   controls    vector of control names, or NULL
#   fe          TRUE adds province fixed effects 

sur_test <- function(data, yf, ym, hf, hm, p = 1, controls = NULL, fe = TRUE
                     , cluster="pcode") {
  
  data <- data %>% mutate(mun_id = row_number())
  
  
  # Same RHS formula as rd_est()
  poly <- if (p == 0) character(0) else paste0("I(iwm94^", 1:p, ")")
  poly <- c(poly, if (p > 0) paste0("i94:", poly))
  rhs  <- paste(c("i94", poly, controls, if (fe) "factor(prov)"), 
                collapse = " + ")
  
  # RD with lm
  m_f <- lm(as.formula(paste(yf, "~", rhs)), 
            data = filter(data, abs(iwm94) < hf))
  m_m <- lm(as.formula(paste(ym, "~", rhs)), 
            data = filter(data, abs(iwm94) < hm))
  
  # Perform suest, clustered by provinces (67 by default)
  # Finding clustered covariance
  comb <- suest(m_f, m_m, model_names = c("f", "m"),
                observation_id = "mun_id", cluster = cluster)
  b <- coef(comb); V <- vcov(comb)
  
  # Wald Test
  a <- "f::i94"; c <- "m::i94"
  z <- (b[a] - b[c]) / sqrt(V[a, a] + V[c, c] - 2*V[a,c])
  
  # Output
  tibble(diff = unname(b[a]-b[c]),
         se_diff = unname(sqrt(V[a, a] + V[c, c] - 2 * V[a, c])),
         p_sur = unname(2 * pnorm(-abs(z))))
}


# 3. Binned Means for RD Plot --------------------------------------------------

# bin_means(): average outcome in equal-width bins of the win margin
#   y         outcome name as text
#   width     bin width, e.g. 0.04 or 0.08

bin_means <- function(data, y, width) {
  data %>%
    filter(!is.na(.data[[y]]), !is.na(iwm94)) %>%
    mutate(bin=floor(iwm94/width)*width + width/2) %>%
    group_by(bin) %>%
    summarise(y_mean = mean(.data[[y]]), n=n(), .groups = "drop")
}

# 4. RD Plot -------------------------------------------------------------------

# rd_plot(): binned means plus a local linear fit with 95% CI on each side
#   y            outcome name as text
#   width        bin width for dots
#   title, ylab  labels optional


rd_plot <- function(data, y, width, title = NULL, ylab = NULL) {
  
  raw <- data %>%
    filter(!is.na(.data[[y]]), !is.na(iwm94)) %>%
    mutate(side=iwm94 >= 0)
  
  bins <- bin_means(data, y, width)
  
  ggplot(raw, aes(x = iwm94, y = .data[[y]], group = side)) +
    stat_smooth(geom = "ribbon", method = "loess",
                method.args = list(degree = 1), formula = y ~ x, level = 0.95,
                fill = NA, colour = "firebrick", linewidth = 0.3,
                outline.type = "both") +
    stat_smooth(geom = "line", method = "loess", method.args = list(degree = 1),
                formula = y ~ x, se = FALSE,
                colour = "blue", linewidth = 0.7) +
    geom_point(data = bins, aes(x = bin, y = y_mean),
               inherit.aes = FALSE, size = 1.5) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(title = title, x = "Islamic win margin 1994", y = ylab)
}


# 5. Save Table to Word --------------------------------------------------------

save_docx <- function(tab, file, bold_rows = NULL, size = 8) {
  ft <- flextable(tab) |>
    fontsize(size = size, part = "all") |>
    padding(padding = 1, part = "all")
  if (length(bold_rows) > 0) {
    ft <- ft |>
      bold(i = bold_rows, j = 1, part = "body") |>
      merge_h_range(i = bold_rows, j1 = 1, j2 = ncol(tab), part = "body")
  }
  ft <- autofit(ft)
  save_as_docx(ft, path = file.path(path_tables, file))
}

message("02_functions done.
Functions defined:
-rd_est()
-sur_test() 
-bin_means()
-rd_plot()
-save_docx()")
