# Replication of Meyersson (2014): Islamic Rule and the Empowerment of the Poor and Pious

TECH6 Data Science & Econometrics, NHH, Fall 2026 · Term paper
**Authors:** Aayush M. Kolhe, Derek Liang

This repository replicates the sharp regression discontinuity (RD) estimates in Meyersson (2014, *Econometrica*), which uses the 1994 Turkish local elections to estimate the effect of Islamic mayors on female education. The running variable is the 1994 Islamic win margin (`iwm94`), the treatment is an Islamic party mayor winning in 1994 (`i94`). Everything is reproduced in R from the original replication data (`regdata0.dta`).

Link to paper and supplements at `econometricsociety.org`:
https://www.econometricsociety.org/publications/econometrica/2014/01/01/islamic-rule-and-empowerment-poor-and-pious

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
| Table I | `10_table1.R` | Summary Statistics |
| Table II | `11_table2.R` | Main RD estimates, female vs male education (2000) |
| Table III | `12_table3.R` | Robustness to polynomial order and bandwidth |
| Table VII | `13_table7.R` | RD estimates for 2011 outcomes |
| Figure 1 | `20_figure1.R` | RD plots of the main outcomes |
| Figure 2 | `21_figure2.R` | Density of the running variable, McCrary test |
| Figure 3 | `22_figure3.R` | Covariate balance at the cutoff |
| Figure 4 | `23_figure4.R` | Outcomes in 2000 vs 1990 |

## Extensions

*In progress.*

## Reference

Meyersson, E. (2014). Islamic Rule and the Empowerment of the Poor and Pious. *Econometrica*, 82(1), 229-269. https://doi.org/10.3982/ECTA9878
