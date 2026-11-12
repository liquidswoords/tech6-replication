# ==============================================================================

# Reproduces figure 3 from Paper
# 22_figure3.R : Figure 3 - Balanced covariate checks
#   -panel (a): Islamic vote share
#   -panel (b): Age 19-
#   -panel (c): Gender
#   -panel (d): Number of parties
#   -panel (e): Age 60+
#   -panel (f): Log Population
#   -panel (g): Any center municipality
#   -panel (h): Household size

# Saves as .pdf and .png in tech6-replication/output/figures

# ==============================================================================

# 1. Load ----------------------------------------------------------------------

df <- readRDS(file.path(path_derived, "analysis.rds"))

n_fig3 <- sum(abs(df$iwm94) < 0.3)

stopifnot(n_fig3 == 1248)

# 2. Spec ----------------------------------------------------------------------

panels3 <- tribble(
  ~y,                ~ylab,
  "vshr_islam1994",  "Islamic vote share",
  "ageshr60",        "Age 60+",
  "ageshr19",        "Age 19-",
  "lpop1994",        "Log Population",
  "sexr",            "Gender",
  "anyc",            "Any center municipality",
  "partycount",      "Number of parties",
  "shhs",            "Household size"
)

# 3. Build ---------------------------------------------------------------------

plots3 <- pmap(panels3, function(y, ylab)
  rd_plot(df, y = y, width = 0.08, window = 0.3, ylab = ylab))

# 4. Assemble ------------------------------------------------------------------

# y-axis breaks per panel, matching the paper (same order as panels3)
y_breaks3 <- list(
  seq(0,    0.6,  0.2),    # Islamic vote share
  seq(0.05, 0.11, 0.02),   # Age 60+
  seq(0.35, 0.55, 0.10),   # Age 19-
  seq(6,    10,   2),      # Log Population
  seq(0.9,  1.2,  0.1),    # Gender
  seq(0,    0.8,  0.2),    # Any center municipality
  seq(2,    8,    2),      # Number of parties
  seq(4,    10,   2)       # Household size
)

# Breaks, plus the right-hand mirror for the second column
for (i in seq_along(plots3)) {
  pos <- if (i %% 2 == 0) "right" else "left"
  plots3[[i]] <- plots3[[i]] +
    scale_y_continuous(breaks = y_breaks3[[i]], position = pos) + 
    coord_cartesian(xlim = c(-0.3, 0.3),
        ylim = range(y_breaks3[[i]]))
}

# x title under each column only, not collected into one
for (i in 1:6) plots3[[i]] <- plots3[[i]] + labs(x = NULL)
for (i in 7:8) plots3[[i]] <- plots3[[i]] + labs(x = "Islamic win margin")

p_fig3 <- wrap_plots(plots3, ncol = 2)

p_fig3 <- p_fig3 &
  scale_x_continuous(breaks = c(-0.25, 0, 0.25)) &
  theme_classic() &
  theme(axis.line  = element_line(colour = "black", linewidth = 0.4),
        axis.ticks = element_line(colour = "black", linewidth = 0.4))

# 5. Save ----------------------------------------------------------------------

ggsave(file.path(path_figures, "figure3.pdf"), p_fig3, 
    width = 7, height = 9)

ggsave(file.path(path_figures, "figure3.png"), p_fig3,
    width = 7, height = 9, dpi = 300)

message("Figure 3: ", n_fig3, 
    " municipalities within +/- 0.3 of the cutoff.")
