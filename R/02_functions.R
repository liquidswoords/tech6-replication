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
  m <- feols(as.formula(f), data = d, cluster = cluster)
  
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
    p       = p
  )
}

# 2. Binned Means for RD Plot --------------------------------------------------

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

# 3. RD Plot -------------------------------------------------------------------

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
    stat_smooth(geom = "ribbon", method = "loess", method.args = list(degree = 1),
                formula = y ~ x, level = 0.95,
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

message("02_functions done: rd_est(), bin_means(), rd_plot() defined.")
