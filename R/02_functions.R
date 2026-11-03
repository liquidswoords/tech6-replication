# ==============================================================================
# 02_functions.R 
#
# Contains following functions:
#   rd_est()
#   sur_test()
#   bin_means()
#   bin_density()
#   ll_density()
#   mccrary()
#   rd_plot()
#   save_docx()
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

# 3b. Binned Density for Mcrary ------------------------------------------------

# bin_density(): share of observations per bin, scaled to a density
#   x            running variable
#   b            bin width
#   c            cutoff; one bin edge placed at c

bin_density <- function(x, b, c=0) {
  
  x <- x[!is.na(x)]
  
  # (1) Define bins
  lo   <- floor((min(x) - c) / b)
  hi   <- floor((max(x) - c) / b) + 1 
  brks <- c + (lo:hi) * b
  
  # (2) Count per bin
  hh <- hist(x, breaks = brks, right = FALSE, plot = FALSE)

  # (3) Output
  tibble(mid = hh$mids,
         n_k = hh$counts,
         dens = hh$density,
         side = if_else(mid < c, "left", "right"))
}

# 3c. Local linear density at a point ------------------------------------------

# ll_density(): local linear estimate of the density at r0
#   bins        output of bin_density()
#   r0          point to evaluate the density at
#   h           bandwidth
#   s           "left" or "right": which side's bins to use


ll_density <- function(bins, r0, h, s) {
  
  # (1) Select bin and set weights
  d <- bins %>%
    filter(side==s) %>%
    mutate(w=pmax(0,1 - abs(mid - r0) / h))
  
  # (2) Only fits with 3+ bins in window
  if (sum(d$w > 0) < 3) return(c(fhat = NA_real_, se = NA_real_))
  
  # (3) Weighted regression of bin heights on distance to r0
  fit <- lm(dens ~ I(mid - r0), data = d, weights = w)
  
  
  # (4) Density at r0 and standard error for CI
  c(fhat = unname(coef(fit)[1]),
    se   = unname(summary(fit)$coefficients[1, 2]))
}

# 3d. McCrary (2008) Density Test ----------------------------------------------

# mccrary(): test for a discontinuity in the density of x at c
#   x        running variable
#   h        bandwidth
#   c        cutoff (zero by default)
#   b        bin width; NULL = Mcrary Default

mccrary <- function(x, h, c = 0, b = NULL) {

  # (1) Setup
  x <- x[!is.na(x)]
  n <- length(x)
  if (is.null(b)) b <- 2 * sd(x) * n^(-1/2) 
  
  # (2) Bin Data
  bins <- bin_density(x, b, c)
  
  # (3) Density immediately on either side of cutoff
  f_L <- ll_density(bins, c, h, "left")[["fhat"]]
  f_R <- ll_density(bins, c, h, "right")[["fhat"]]
  
  # (4) Test for log jump: 
  theta <- log(f_R) - log(f_L)
  se    <- sqrt((24/5) * (1 / (n * h)) * (1/f_R + 1/f_L)) # McCrary (2008) SE
  z     <- theta / se
  p     <- 2 * pnorm(-abs(z))

  # (5)  Smoothed curve
  fits  <- map2(bins$mid, bins$side, ~ ll_density(bins, .x, h, .y))
  curve <- bins %>%
    mutate(fhat = map_dbl(fits, "fhat"),
           se_f = map_dbl(fits, "se"))
  
  list(theta = theta, se = se, z = z, p = p,
       f_L=f_L, f_R = f_R,
       b = b, h = h, n = n, curve = curve)
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
-bin_density()
-mccrary()
-ll_density()
-rd_plot()
-save_docx()")
