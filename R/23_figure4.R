# ==============================================================================

# Reproduces figure 4 from Paper
# 23_figure4.R : Figure 4 - High school education 2000 and 1990
#   -panel (a): Women in 2000
#   -panel (b): Men in 200
#   -panel (c): Women in 1990
#   -panel (d): Men in 1990
# All are graphicall illustrations of RD design, showing
# unconditional means in 4-percent bins. The solid black
# line represents the predicted values of a local linear
# smoother on each side of the threshold at zero. The
# outer gray lines denote the 95 percent confidence itnervals

# Saves as .pdf and .png in tech6-replication/output/figures

# ==============================================================================

# 1. Load Data -----------------------------------------------------------------

df <- readRDS(file.path(path_derived, "analysis.rds"))

# Panel samples: the 1990 census matches only part of the 2000 sample
n_2000 <- sum(!is.na(df$hischshr1520f)      & abs(df$iwm94) < 0.5)
n_1990 <- sum(!is.na(df$c90hischshr1520f)   & abs(df$iwm94) < 0.5)

stopifnot(n_2000 == 2177, n_1990 == 1363)

# 2. Panel spec ----------------------------------------------------------------

ylab_hs <- "Share of 15-20-year-olds with High School Degree"

panels <- tribble(
  ~y,                  ~title,                 ~ylab,
  "hischshr1520f",     "(a) Women in 2000",     ylab_hs,
  "hischshr1520m",     "(b) Men in 2000",       "",
  "c90hischshr1520f",  "(c) Women in 1990",     ylab_hs,
  "c90hischshr1520m",  "(d) Men in 1990",       ""
)

# 3. Build Panels --------------------------------------------------------------

# One ggplot per row of panels. 4-percent bins with a +/- 0.5 window

plots <- pmap(panels, function(y, title, ylab)
    rd_plot(df, y = y, width = 0.04, window = 0.5, bw = 0.08, title = title,
            ylab = if (nzchar(ylab)) ylab else NULL))

# 4. Assemble ------------------------------------------------------------------

y_breaks <- seq(-0.05, 0.25, 0.05)

# Right-hand panels mirror the y axis to the right, as in the paper
plots[[1]] <- plots[[1]] + scale_y_continuous(breaks = y_breaks)
plots[[2]] <- plots[[2]] + scale_y_continuous(breaks = y_breaks,
                                              position = "right")
plots[[3]] <- plots[[3]] + scale_y_continuous(breaks = y_breaks)
plots[[4]] <- plots[[4]] + scale_y_continuous(breaks = y_breaks,
                                              position = "right")

p_fig4 <- wrap_plots(plots, ncol = 2) +
  plot_layout(axis_titles = "collect")

p_fig4 <- p_fig4 &
  coord_cartesian(xlim = c(-0.5, 0.5), ylim = c(-0.05, 0.25)) &
  scale_x_continuous(breaks = seq(-0.5, 0.5, 0.25)) &
  theme_classic() &
  theme(plot.title = element_text(hjust = 0.5, size = 11),
        axis.line  = element_line(colour = "black", linewidth = 0.4),
        axis.ticks = element_line(colour = "black", linewidth = 0.4))
        
# 5. Save ----------------------------------------------------------------------

# Save as pdf
ggsave(file.path(path_figures, "figure4.pdf"), p_fig4,                   # <<<
       width = 8, height = 7)

# Save as png
ggsave(file.path(path_figures, "figure4.png"), p_fig4,                   # <<<
       width = 8, height = 7, dpi = 300)

message("Figure 4: ", n_2000, " municipalities in the 2000 panels, ",    # <<<
        n_1990, " in the 1990 panels.")  