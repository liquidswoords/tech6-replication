# ==============================================================================
# 20_figure1.R 
#
# Reproduces figure 1 from Paper
# Figure 1: Islamic win margin and Islamic vote share in 1994.
#
# Saves as .pdf and .png in tech6-replication/output/figures
#
# ==============================================================================

# 1. Load Data -----------------------------------------------------------------

fig1 <- readRDS(file.path(path_derived, "analysis.rds")) %>%
  filter(vshr_islam1994 < 0.75) %>%     
  mutate(near_cutoff = abs(iwm94) < 0.02)     

stopifnot(nrow(fig1) == 2627, sum(fig1$near_cutoff) == 100)

# 2. Plot Layers ---------------------------------------------------------------

p_fig1 <- ggplot(fig1, aes(x = vshr_islam1994, y = iwm94)) +
  
  # (1) Cutoff line
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.6) +

  # (2) Towns away from cutoff
  geom_point(data = filter(fig1, !near_cutoff), colour ="steelblue1", size=0.7) +
             
  # (3) Close elections
  geom_point(data=filter(fig1, near_cutoff), colour = "navyblue", size = 1.0) +
  
  # (4) 2-Party benchmark line
  geom_segment(x = 0, y = -1, xend = 0.75, yend = 0.5,
               colour = "black", linewidth = 0.5) +
  
# 3. Axes and theme ------------------------------------------------------------
  
  # (5) Axes ticks and labels
  scale_x_continuous(breaks = seq(0, 0.75, 0.25), 
                     labels = c("0", "0.25", "0.5", "0.75")) +
  scale_y_continuous(breaks = c(-1, -0.5, 0, 0.5),
                     labels = c("-1", "-0.5", "0", "0.5")) +
  
  # (6) Set Window
  coord_cartesian(xlim = c(-0.01, 0.755), ylim = c(-1.02, 0.65),
                  expand = FALSE) +
  
  # (7) labels and theme
  labs(x = "Islamic vote share", y = "Islamic win margin") +
  theme_classic() +
  theme(plot.margin = margin(5.5, 14, 5.5, 5.5))
  
# 4. Save ----------------------------------------------------------------------

# Save as pdf
ggsave(file.path(path_figures, "figure1.pdf"), p_fig1,
       width = 5.5, height = 4.5)

# Save as png
ggsave(file.path(path_figures, "figure1.png"), p_fig1,
       width = 5.5, height = 4.5, dpi = 300)


message("Figure 1: ", nrow(fig1), " towns plotted, ",
        sum(fig1$near_cutoff), " within 2 pp of the cutoff.")
