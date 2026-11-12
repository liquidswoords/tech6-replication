# ==============================================================================
# 02_functions.R 
#
# Contains following functions:
#   rd_est() - Main estimation function for local polyonmial RD
#   sur_test() - Seemingly Unrelated Regressions test
#   bin_means() - Binned outcome means for RD plots
#   bin_density() - Binned Density for McCrary
#   ll_density() - Local linear density at a point
#   mccrary() - McCrary (2008) Density Test
#   mccrary_h() - McCrary default bandwidth
#   rd_plot() - Binned means plus a local linear fit with 95% CI on each side
#   save_docx() - Save Table to Word
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
  s <- if (is.finite(h)) h else 1
  d <- data %>% filter(abs(iwm94) < h) %>% mutate(x = iwm94 / s)
  
  # 2. Polynomial Terms
  poly <- if (p == 0) character(0) else paste0("I(x^", 1:p, ")")
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


# Replicates Stata's suest + test from tables_R4_final.do

#  Uses the suest R package (Trenton D. Mize, GitHub tdmize/suest, pinned in 
# 00_packages.R)
# It implements Mize, Doan and Long (2019):
#  "“A General Framework for Comparing Predictions 
#   and Marginal Effects Across Models.”

# Models are fit with lm() + factor(prov) rather than feols(), since the
# package works on lm objects.

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
  
  #  Wald statistic
  a <- "f::i94"; c <- "m::i94"
  z <- (b[a] - b[c]) / sqrt(V[a, a] + V[c, c] - 2*V[a,c])
  
  # Output
  tibble(diff = unname(b[a]-b[c]),
         se_diff = unname(sqrt(V[a, a] + V[c, c] - 2 * V[a, c])),
         p_sur = unname(2 * pnorm(-abs(z))))
}


# 3. Binned Means for RD Plot --------------------------------------------------

# bin_means(): binned outcome means for RD plots
#   y         outcome name as text
#   width     bin width, e.g. 0.04 or 0.08

bin_means <- function(data, y, width) {
  data %>%
    filter(!is.na(.data[[y]]), !is.na(iwm94)) %>%
    mutate(bin=floor(iwm94/width)*width + width/2) %>%
    group_by(bin) %>%
    summarise(y_mean = mean(.data[[y]]), n=n(), .groups = "drop")
}

# 4a. Binned Density for McCrary -----------------------------------------------

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

# 4b. Local linear density at a point ------------------------------------------

# ll_density(): local linear estimate of the density at r0, with its SE
#   bins        output of bin_density() (padded on either side)
#   r0          point to evaluate the density at
#   h           bandwidth
#   s           "left" or "right": which side's bins to use
#   n           Sample Size
#   b           bin 
#   c           cutoff
#   SE is found by the closed form from McCrary's DCdensity package used in 
#   paper.(Var = fhat * C(m) / (nh))

ll_density <- function(bins, r0, s, h, n, b, c=0) {
  
  # (1) Select bins on side within kernel window
  d <- bins %>%
    filter(side == s) %>%
    mutate(w = pmax(0, 1 - abs(mid - r0) / h)) %>%
    filter(w > 0)
  if (nrow(d) < 3) return(c(fhat = NA_real_, se = NA_real_))
  
  # (2) Local linear fit
  X    <- cbind(1, d$mid - r0)
  l    <- solve(crossprod(X, d$w * X), t(d$w * X))[1, ]
  fhat <- sum(l * d$dens)
  
  # (3) Closed Form LLR SE (DCdensity.ado)
  m <- max(-1, min(1, (c - r0) / h))
  if (s == "left") m <- -m # flip left
  C <- 12/5 * (2 - 3*m^11 - 24*m^10 - 83*m^9 - 72*m^8 + 42*m^7 + 18*m^6 -
                 18*m^5 + 18*m^4 - 3*m^3 + 18*m^2 - 15*m) /
    (1 + m^6 + 6*m^5 - 3*m^4 - 4*m^3 + 9*m^2 - 6*m)^2
  

  c(fhat = fhat, se = sqrt(fhat * C / (n * h)))
}

# 4c. McCrary (2008) Density Test ----------------------------------------------

# mccrary(): test for a discontinuity in the density of x at c
#   x        running variable
#   h        bandwidth; NULL = McCrary default via mccrary_h()
#   c        cutoff (zero by default)
#   b        bin width; NULL = Mcrary Default

mccrary <- function(x, h = NULL, c = 0, b = NULL) {

  # (1) Setup
  x <- x[!is.na(x)]
  n <- length(x)
  if (is.null(b)) b <- 2 * sd(x) * n^(-1/2) 
  if (is.null(h)) h <- mccrary_h(x, b, c)[["h"]]
  
  # (2) Bin Data
  bins <- bin_density(x, b, c)
  
  # (3) Density immediately on either side of cutoff
  f_L <- ll_density(bins, r0 = c, h = h, s = "left",  n = n, c = c)[["fhat"]]
  f_R <- ll_density(bins, r0 = c, h = h, s = "right", n = n, c = c)[["fhat"]]
  
  
  # (4) Test for log jump: 
  theta <- log(f_R) - log(f_L)
  se    <- sqrt((24/5) * (1 / (n * h)) * (1/f_R + 1/f_L)) # McCrary (2008) SE
  z     <- theta / se
  p     <- 2 * pnorm(-abs(z))

  # (5)  Smoothed curve: Padding w. empty bins beyond range
  pad <- ceiling(h / b)
  lo_mid <- min(bins$mid); hi_mid <- max(bins$mid)
  bins_p <- bind_rows(
    tibble(mid= lo_mid - (pad:1) * b, n_k = 0, dens = 0),
    bins %>% select(mid, n_k, dens),
    tibble(mid = hi_mid + (1:pad) * b, n_k = 0, dens = 0)
  ) %>% mutate(side= if_else(mid < c, "left", "right"))
  
  
  curve <- bins %>%
    mutate(
      fit  = map2(mid, side, ~ 
                    ll_density(bins_p, r0 = .x, h = h, s = .y, n = n, c = c)),
      fhat = map_dbl(fit, "fhat"),
      se_f = map_dbl(fit, "se")
    ) %>%
    select(-fit)
  
  list(theta = theta, se = se, z = z, p = p,
       f_L = f_L, f_R = f_R,
       b = b, h = h, n = n, curve = curve)
}

# 4d. McCrary default bandwidth ------------------------------------------------

# mcrary_h(): Default bandwidth from DCdensity.ado (McCrary 2008), used for 
#   x running variable
#   b bin width
#   c cutoff


mccrary_h <- function(x, b, c=0) {
  
  x <- x[!is.na(x)]
  n <- length(x)
  
  # (1) Bin grid as in DCdensity: j cells starting at midpoint l
  
  l <- floor((min(x) - c) / b) * b + b/2 + c         # lowest midpoint
  r <- floor((max(x) - c) / b) * b + b / 2 + c   # highest midpoint
  j <- floor((max(x)-min(x)) / b) + 2
  
  mid  <- l + (0:(j - 1)) * b
  k    <- floor((x-c) / b) - floor((min(x) - c) / b) + 1 # Bin Number
  dens <- tabulate(k, nbins = j) / (n * b) 
  
  # (2) Quartic fit on one side
  side_h <- function(keep, range) {
    xs  <- mid[keep]
    ys  <- dens[keep]
    fit <- lm(ys ~ xs + I(xs^2) + I(xs^3) + I(xs^4))
    be  <- coef(fit)
    fpp <- 2 * be[3] + 6 * be[4] * xs + 12 * be[5] * xs^2 # 2nd derivative
    3.348 * (summary(fit)$sigma^2 * range / sum (fpp^2)) ^ (1/5)
  }
  
  h_L <- side_h(mid < c, c - l)
  h_R <- side_h(mid > c, r - c)
  
  
  c(h = unname((h_L + h_R) / 2), h_L = unname(h_L), h_R = unname(h_R))
}


# 5. RD Plot -------------------------------------------------------------------

# rd_plot(): binned means plus a local linear fit with 95% CI on each side
#   y            outcome name as text
#   width        bin width for dots
#   window       half-width of the plotted range (e.g. 0.3); Inf = all data
#   title, ylab  labels optional


rd_plot <- function(data, y, width, window = Inf, title = NULL, ylab = NULL) {
  
  raw <- data %>%
    filter(!is.na(.data[[y]]), !is.na(iwm94), abs(iwm94) < window) %>%
    mutate(side=iwm94 >= 0)
  
  bins <- bin_means(raw, y, width)
  
  ggplot(raw, aes(x = iwm94, y = .data[[y]], group = side)) +
    geom_point(data = bins, aes(x = bin, y = y_mean),
               inherit.aes = FALSE, size = 1.5, colour = "steelblue1") +
    stat_smooth(geom = "ribbon", method = "loess",
                method.args = list(degree = 1), formula = y ~ x, level = 0.95,
                fill = NA, colour = "grey30", linewidth = 0.3,
                outline.type = "both") +
    stat_smooth(geom = "line", method = "loess", method.args = list(degree = 1),
                formula = y ~ x, se = FALSE,
                colour = "navyblue", linewidth = 0.7) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(title = title, x = "Islamic win margin 1994", y = ylab)
}


# 6. Save Table to Word --------------------------------------------------------

save_docx <- function(tab, file, bold_rows = NULL, size = 8) {
  ft <- flextable(tab) |>
    fontsize(size = size, part = "all") |>
    padding(padding = 1, part = "all")
  if (length(bold_rows) > 0) {
    ft <- ft |>
      bold(i = bold_rows, j = 1, part = "body") |>
      merge_h_range(i = bold_rows, j1 = 1, j2 = ncol(tab), part = "body")|>
      merge_h(i = 1:2, part = "body")
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
-ll_density()
-mccrary()
-mccrary_h()
-rd_plot()
-save_docx()")
