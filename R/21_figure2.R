# ==============================================================================
# 21_figure2.R 
#
# Reproduces figure 2 from Paper
# Figure 2: Is the density of the Islamic win margin continuous at 
#           the threshold? 
#
#   -panel (a): Histogram of Islamic Win Margin (IWM) 1994 (99 bins)
#   -panel (b): McCrary (2008) test of discontinuity in IWM density 
#
# Saves as .pdf and .png in tech6-replication/output/figures
#
# ==============================================================================

# 1. Load Data -----------------------------------------------------------------


fig2 <- read_dta(file.path(path_raw,"regdata0.dta"))


x_all <- as.numeric(fig2$iwm94)
x_all <- x_all[!is.na(x_all)] # Drop rows w. missing running variable
stopifnot(length(x_all) == 2660) # Check length


# 2. Panel (a): Full Range Histogram -------------------------------------------

p_fig2a <- ggplot(tibble(x = x_all), aes(x = x)) +
  
  # (1) Bars: 99 bins over range 
  geom_histogram(aes(y = after_stat(count / sum(count) * 100)),
                 breaks = seq(min(x_all), max(x_all), length.out = 100),
                 closed = "left",
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


# As graphs_R4_final.do (Figure 2B): Towns with non missing outcome and running
# variable. Trimmed to |IWM| < 0.6. Estimation uses the full 0.6 window

x_res <- fig2 %>%
  filter(!is.na(hischshr1520f), !is.na(iwm94)) %>%
  filter(abs(iwm94) < 0.6) %>%
  pull(iwm94) %>%
  as.numeric()

stopifnot(length(x_res) == 2543) # Check

# 4. McCrary Test --------------------------------------------------------------

# Bandwidth for McCrary test was found in same way as DCdensity.ado in authors
# replication files. It is the default bandwidth found by fitting quartic on
# either side. 

# Bin Width selected as McCrary default

mc <- mccrary(x_res)

stopifnot(round(mc$theta, 3) == -0.049, round(mc$se, 3) == 0.161)

# 5. Panel (b): Density Test ---------------------------------------------------

z_band <- 1.96   # 95% CI band

p_fig2b <- ggplot(mc$curve %>% filter(abs(mid) < 0.5), aes(x = mid),) +
  
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

# 6. rddensity check -----------------------------------------------------------

an <-readRDS(file.path(path_derived, "analysis.rds")) # Full analysis sample
dt_global <- rddensity(X = an$iwm94, c = 0) # N=2629


dt_06 <- rddensity(X = x_res, c=0) # 0.6 window, same as McCrary

dt_global_t <- dt_global$test$t_jk
dt_global_p <- dt_global$test$p_jk

dt_06_t <- dt_06$test$t_jk
dt_06_p <- dt_06$test$p_jk


rdd_tab <- tibble(
  sample = c("Full analysis sample", "0.6 window (McCrary)"),
  N      = c(length(an$iwm94), length(x_res)),
  T      = c(dt_global$test$t_jk, dt_06$test$t_jk),
  p      = c(dt_global$test$p_jk, dt_06$test$p_jk)
)


# 7. Combine and Save ----------------------------------------------------------


p_fig2 <- p_fig2a / p_fig2b

# Save as pdf
ggsave(file.path(path_figures, "figure2.pdf"), p_fig2,
       width = 5.5, height = 9)

# Save as png
ggsave(file.path(path_figures, "figure2.png"), p_fig2,
       width = 5.5, height = 9, dpi = 300)

message(
  sprintf(paste0(
    "Figure 2: theta = %.3f (SE %.3f, p = %.2f), b = %.4f, h = %.4f, N = %d. ",
    "Paper: -0.049 (0.161).\n"),
    mc$theta, mc$se, mc$p, mc$b, mc$h, mc$n),
  "rddensity (robust bias-corrected):\n",
  paste(sprintf("  %-22s N = %4d   T = %6.3f   p = %.3f",
                rdd_tab$sample, rdd_tab$N, rdd_tab$T, rdd_tab$p),
        collapse = "\n")
)

# Save side by side version for write-up
p_fig2_wide <- (p_fig2a | p_fig2b) &
  theme(plot.caption = element_text(hjust = 0, size = 10))

ggsave(file.path(path_figures, "figure2_wide.png"), p_fig2_wide,
       width = 6.5, height = 2.8, dpi = 300)
