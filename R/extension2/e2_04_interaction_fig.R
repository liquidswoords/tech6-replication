# ==============================================================================
# e2_04_interaction_fig.R : Interaction curves for Extension 2
#
# How does the effect of an Islamic mayor change along each moderator?
#   - line + band: tau(z) = tau + delta * z with pointwise 95% CI
#                  (fully interacted RD, h = 0.25, controls + province FE)
#   - points:      separate RD estimate within each tercile of z (+/- 1.96 SE),
#                  plotted at the tercile's mean z. Checks the linear line
#                  against a version that assumes no shape. Terciles with
#                  fewer than 20 Islamic-mayor towns are not plotted.
#   - rug:         Islamic-mayor towns. Where the rug is empty, the line is
#                  extrapolated, not estimated.
#   - dashed line: zero; dotted line: pooled RD estimate (no heterogeneity)
#
#   Columns: barriers, poverty, piety (all coded higher = more disadvantaged
#            / more pious, in SD units within the window)
#   Rows:    women (main figure), women + men (appendix figure)
#
# Saves as .pdf and .png in tech6-replication/output/extension2
#
# Requires: e2_01_functions.R (fit_het, het_curve), 02_functions.R (rd_est)
# ==============================================================================

source(here::here("R", "00_packages.R"))
source(here::here("R", "02_functions.R"))
source(here::here("R", "extension2", "e2_01_functions.R"))


# 1. Load Data -----------------------------------------------------------------

cfg <- e2_config()
path_e2 <- here::here(cfg$paths$output_dir)          # output/extension2
dir.create(path_e2, recursive = TRUE, showWarnings = FALSE)
h   <- cfg$h_main

# Same sign convention as e2_03_het_table.R: higher = more disadvantaged
d <- load_e2(cfg) %>%
  mutate(barrier_idx = -barriers,
         poverty_idx = -poor)


# 2. Spec ----------------------------------------------------------------------

mod_labels <- c(barrier_idx = "Barriers to schooling",
                poverty_idx = "Poverty",
                pious       = "Piety (Islamic vote share 1994)")

out_labels <- c(hischshr1520f = "Women", hischshr1520m = "Men")

specs <- expand_grid(y = names(out_labels), z = names(mod_labels))

# vote share is both the piety moderator and an RD control (see e2_03)
controls_for <- function(z) {
  if ("pious" %in% z) setdiff(cfg$rd_controls, cfg$pious_var) else cfg$rd_controls
}


# 3. Estimate ------------------------------------------------------------------

# For each outcome x moderator: the curve, the tercile points and the rug.
# The curve is drawn between the 5th and 95th percentile of z only.

pieces <- pmap(specs, function(y, z) {
  
  fit <- fit_het(d, y = y, z = z, h = h, controls = controls_for(z))
  zs  <- fit$data[[fit$mods]]
  
  curve <- het_curve(fit, z_grid = seq(quantile(zs, 0.05),
                                       quantile(zs, 0.95), length.out = 101))
  
  # Tercile RD estimates (same spec as the main RD, within each tercile)
  bins <- fit$data %>%
    mutate(bin = ntile(.data[[fit$mods]], 3)) %>%
    group_by(bin) %>%
    group_map(\(dd, key) {
      rd_est(dd, y = y, h = h, controls = controls_for(z)) %>%
        mutate(bin = key$bin, z = mean(dd[[fit$mods]]),
               n_islamic = sum(dd$i94))
    }) %>%
    bind_rows() %>%
    # .env$ = the function arguments, not the new column z (tercile mean)
    mutate(outcome = .env$y, moderator = .env$z,
           lo = coef - 1.96 * se, hi = coef + 1.96 * se)
  
  rug <- fit$data %>%
    filter(i94 == 1) %>%
    transmute(outcome = .env$y, moderator = .env$z, z = .data[[fit$mods]])
  
  pooled <- rd_est(d, y = y, h = h, controls = cfg$rd_controls) %>%
    transmute(outcome = .env$y, moderator = .env$z, coef)
  
  list(curve = curve, bins = bins, rug = rug, pooled = pooled)
})

curves <- map_dfr(pieces, "curve")
bins   <- map_dfr(pieces, "bins")
rugs   <- map_dfr(pieces, "rug")
pooled <- map_dfr(pieces, "pooled")

print(select(bins, outcome, moderator, bin, z, coef, se, n, n_islamic))

# Tercile points need Islamic-mayor towns to be identified. The lowest piety
# tercile has ~1, so its "estimate" is noise: shown in the csv, not plotted.
min_islamic <- 20
bins_plot   <- filter(bins, n_islamic >= min_islamic)


# 4. Build ---------------------------------------------------------------------

# Factor labels so facets read in the intended order
lab <- function(df) df %>%
  mutate(moderator = factor(mod_labels[moderator], mod_labels),
         outcome   = factor(out_labels[outcome], out_labels))

plot_curves <- function(outs) {
  keep <- function(df) lab(filter(df, outcome %in% outs))
  
  ggplot(keep(curves), aes(z, estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
    geom_hline(data = keep(pooled), aes(yintercept = coef),
               linetype = "dotted", linewidth = 0.4) +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = "grey80") +
    geom_line(linewidth = 0.7) +
    geom_pointrange(data = keep(bins_plot),
                    aes(x = z, y = coef, ymin = lo, ymax = hi),
                    size = 0.3, shape = 21, fill = "white") +
    geom_rug(data = keep(rugs), aes(x = z), inherit.aes = FALSE,
             sides = "b", alpha = 0.3, length = unit(0.03, "npc")) +
    facet_grid(outcome ~ moderator) +
    coord_cartesian(xlim = c(-2.5, 2.5)) +
    labs(x = "Moderator (SD from window mean; higher = more disadvantaged / pious)",
         y = "Effect of Islamic mayor on high school share") +
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.text = element_text(face = "bold"),
          axis.line  = element_line(colour = "black", linewidth = 0.4),
          axis.ticks = element_line(colour = "black", linewidth = 0.4))
}

p_main <- plot_curves("hischshr1520f")
p_both <- plot_curves(names(out_labels))


# 5. Save ----------------------------------------------------------------------

write_csv(curves, file.path(path_e2, "e2_interaction_curves.csv"))
write_csv(bins,   file.path(path_e2, "e2_interaction_terciles.csv"))

ggsave(file.path(path_e2, "e2_interaction.pdf"), p_main, width = 9, height = 3.4)
ggsave(file.path(path_e2, "e2_interaction.png"), p_main, width = 9, height = 3.4,
       dpi = 300)
ggsave(file.path(path_e2, "e2_interaction_both.pdf"), p_both, width = 9, height = 6)
ggsave(file.path(path_e2, "e2_interaction_both.png"), p_both, width = 9, height = 6,
       dpi = 300)


message("e2_04_interaction_fig done: figures saved to output/extension2.")