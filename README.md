# Replication of Meyersson (2014): Islamic Rule and the Empowerment of the Poor and Pious

TECH6 Data Science & Econometrics, NHH, Fall 2026 · Term paper
**Authors:** Aayush M. Kolhe, Derek Liang

This repository replicates the sharp regression discontinuity (RD) estimates in Meyersson (2014, *Econometrica*), which uses the 1994 Turkish local elections to estimate the effect of Islamic mayors on female education. The running variable is the 1994 Islamic win margin (`iwm94`), the treatment is an Islamic party mayor winning in 1994 (`i94`). Everything is reproduced in R from the original replication data (`regdata0.dta`).

## Quick Start

1. Clone the repository and open `tech6-replication.Rproj` in RStudio
2. Run `run_all.R`

Missing CRAN packages install automatically. `suest` is installed from Github, so the first run needs internet access. Tested on R 4.6.1 (Windows 11).

## Repository structure

```
run_all.R             Runs every script below in order
R/
  00_packages.R       Install and load packages, set paths and options
  01_clean.R          Build analysis samples from raw data
  02_functions.R      Shared functions
  03_bandwidths.R     Imbens-Kalyanaraman bandwidths for every outcome
  1x_table*.R         Tables (10 = Table I, 11 = Table II, ...)
  2x_figure*.R        Figures (20 = Figure 1, 21 = Figure 2, ...)
data/
  raw/                regdata0.dta (original replication data, never modified)
  derived/            Cleaned samples as .rds (2000 and 2011 stored seperatly)
output/
  tables/             Formatted .docx and raw _raw.csv per table
  figures/            .pdf and .png per figure
  logs/               sessionInfo.txt
docs/                 Paper, assignment brief, supplements to paper (found at *Econometrica* webpage)
```

## Replication results

| Output | Script | Content |
|---|---|---|
| Table I | `10_table1.R` | Descriptive statistics by treatment |
| Table II | `11_table2.R` | Main RD estimates, female vs male education (2000) |
| Table III | `12_table3.R` | Robustness to polynomial order and bandwidth |
| Table VII | `13_table7.R` | RD estimates for 2011 outcomes |
| Figure 1 | `20_figure1.R` | RD plots of the main outcomes |
| Figure 2 | `21_figure2.R` | Density of the running variable, McCrary test |
| Figure 3 | `22_figure3.R` | Covariate balance at the cutoff |
| Figure 4 | `23_figure4.R` | Outcomes in 2000 vs 1990 |

## Implementation notes

Specification choices that the paper does not state, recovered by matching published numbers and confirmed against the original Stata do-files (`docs/Recreation data and programmes/`).

1. **Fixed effects and clustering.** Province fixed effects use the 81 current provinces (`pd_*`); standard errors are clustered by the 67 pre-1990s provinces (`pcode`). Only this combination reproduces the published coefficients and SEs.
2. **Bandwidths.** IK bandwidths follow the 2009 working-paper algorithm via `rdd::IKbandwidth()`. `rdd` is archived on CRAN, so `03_bandwidths.R` installs it from the archive and falls back to stored full-precision values if that fails. One known deviation: men's N in Table II is 1342 vs 1341 in the paper, caused by a single town lying $2 \times 10^{-6}$ inside our bandwidth.
3. **Figure 2.** Currently estimated on $|X| < 0.5$ with $h = 0.14$, giving $\hat\theta = -0.047$ (paper: $-0.049$, SE 0.161).
4. **Figures 3 and 4.** Binned means with a loess fit on the main sample. The original uses local linear Epanechnikov smoothing on the full data, so the fitted lines differ slightly.
5. **Panel C of Table II.** Tests of equal effects for women and men use the `suest` R package (Mize), which reproduces Stata's `suest` with province-clustered cross-equation covariance.

## Extensions

*In progress.*

## Reference

Meyersson, E. (2014). Islamic Rule and the Empowerment of the Poor and Pious. *Econometrica*, 82(1), 229-269. https://doi.org/10.3982/ECTA9878
