Course: TECH6 Data Science & Econometrics

Semester: Fall 2026
# Term Paper: Replication of Meyersson (2014), *Econometrica*

## Overview
Replicates the sharp RDD estimates of the effect of Islamic Mayors on female education in Turkey, using the 1994 municipal election win margin.

### Group Members:
- Aayush Kolhe
- Derek Liang

### Reproduce 
1. Clone the repo and open ´tech6-replication.Rproj´
2. Run ´run_all.R´ Missing packages install automatically.

### Reference Paper:
Meyersson, E. (2014), Islamic Rule and the Empowerment of the Poor and Pious. 
*Econometrica*, 82: 229-269. https://doi.org/10.3982/ECTA9878

## Status
- [x] Table I, Table II
- [x] Figure 1, Figure 2
- [ ] Table III, Table VII, Figures 3–4
- [ ] Extensions

## Structure
```
data/raw/        regdata0.dta (original replication data)
data/derived/    cleaned .rds files (generated)
R/               numbered scripts, run in order by run_all.R
output/          tables/, figures/, logs/
```




