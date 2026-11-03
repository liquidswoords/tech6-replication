# ==============================================================================
# 21_figure2.R 
#
# Reproduces figure 2 from Paper
# Figure 2: Is the density of the Islamic win margin continuous at 
#           the threshold? 
#
#   -panel (a): Histogram of Islamic Win Margin (IWM) 1994 (2% bins)
#   -panel (b): McCrary (2008) test of discontinuity in IWM density 
#
# Saves as .pdf and .png in tech6-replication/output/figures
#
# ==============================================================================

# 1. Load Data -----------------------------------------------------------------


fig2 <- readRDS(file.path(path_derived, "analysis.rds"))

x_all <- fig2$iwm94

# 2. Panel (a): Full Range Histogram -------------------------------------------

p_fig2a <- ggplot(tibble(x = x_all), aes(x = x)) +
  
  # (1) Bars: 2% bins 
  geom_histogram(aes(y = after_stat(count / sum(count) * 100)),
                 breaks = seq(-1, 1, by = 0.02),
                 fill = "steelblue1", colour = "navyblue", linewidth = 0.05) +
  # (2) Cutoff line
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.4,
             colour= "black") +
  
  # (3) Axes ticks and labels
  scale_x_continuous(breaks = seq(-1, 1, 0.5),
                     labels = c("-1", "-0.5", "0", "0.5", "1")) +
  
  # (4) Set window
  coord_cartesian(xlim = c(-1.02, 1.02), ylim = c(0, 5.2), expand = FALSE) +
  
  # (5) Labels and theme
  labs(x = "Islamic win margin", y = "Percent",
       subtitle = "(a) Global histogram") +
  theme_classic() +
  theme(plot.subtitle = element_text(hjust = 0.5))


# 3. McCrary Sample ------------------------------------------------------------

# Panel (b) uses towns with -0.5 < IWM < 0.5. The paper's axis covers only
# this range, and the density levels only match on this subsample.
# 2177 is my own count for given window.


x_res <- x_all[x_all > -0.5 & x_all < 0.5]
stopifnot(length(x_res) == 2177)


# 4. McCrary Test --------------------------------------------------------------

# Bandwidth not reported in paper. We use McCrary (2008) SE formula to solve for
# h at the SE published in paper (0.161). Giving us 0.14.
# McCrary default bandwidth gives us different results from the test.
#
# SE ∈ [0.1605, 0.1615) gives h ∈ [0.1395, 0.1413]; 0.14 is the only round value


h_mc <- 0.14

mc <- mccrary(x_res, h = h_mc)

# Implied bandwidth check: should be close to 0.14
h_implied <- (24/5) * (1/mc$f_R + 1/mc$f_L) / (mc$n * 0.161^2)

# 5. Panel (b): Density Test ---------------------------------------------------

z_band <- 1.96   # 95% CI band

p_fig2b <- ggplot(mc$curve, aes(x = mid)) +
  
  # (1) Bin heights
  geom_point(aes(y = dens), colour = "steelblue1", size = 1.2) +
  
  # (2) Smoothed Density, sides seperate
  geom_line(aes(y = fhat, group = side), linewidth = 0.8, colour="navyblue") +
  
  # (3) CI Bands
  geom_line(aes(y = fhat + z_band * se_f, group = side), linewidth = 0.3,
            colour = "grey50") +
  geom_line(aes(y = fhat - z_band * se_f, group = side), linewidth = 0.3,
            colour = "grey50") +
  
  # (4) Cutoff line
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.3) +
  
  # (5) Axes ticks and labels
  scale_x_continuous(breaks = c(-0.5, 0, 0.5),
                     labels = c("-0.5", "0", "0.5")) +
  scale_y_continuous(breaks = c(0, 1.25, 2.5),
                     labels = c("0", "1.25", "2.5")) +
  
  # (6) Window
  coord_cartesian(xlim = c(-0.51, 0.51), ylim = c(0, 3.3), expand = FALSE) +
  
  # (7) Labels and theme
  labs(x = "Islamic win margin 1994", y = "Density",
       subtitle = "(b) Density test",
       caption = sprintf("Discontinuity est. = %.3f, s.e. = %.3f",
                         mc$theta, mc$se)) +
  theme_classic() +
  theme(plot.subtitle = element_text(hjust = 0.5),
        plot.caption  = element_text(hjust = 0, size = 8))

# 6. Combine and Save ----------------------------------------------------------


p_fig2 <- p_fig2a / p_fig2b

# Save as pdf
ggsave(file.path(path_figures, "figure2.pdf"), p_fig2,
       width = 5.5, height = 9)

# Save as png
ggsave(file.path(path_figures, "figure2.png"), p_fig2,
       width = 5.5, height = 9, dpi = 300)

message(sprintf(paste0(
  "Figure 2: theta = %.3f (SE %.3f, p = %.2f), b = %.4f, h = %.2f, N = %d. ",
  "Paper: -0.049 (0.161). Implied h = %.4f."),
  mc$theta, mc$se, mc$p, mc$b, mc$h, mc$n, h_implied))


