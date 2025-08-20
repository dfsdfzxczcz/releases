# Hip OA (GBD 2021) – Analysis Code (Clean)

This repository hosts **cleaned R scripts** used in the paper _Global Burden of Hip Osteoarthritis and High BMI Attribution (1990–2021)_.
All explanatory comments have been removed; only executable code is preserved.

## Structure
```
code/
  inequality/
    inequality_analysis_clean.R   # mapping, age–sex, SDI associations, decomposition figs
  risk/
    paf_analysis_clean.R          # PAF maps, trends, SDI associations, joinpoint
data/
  raw/        # place source CSV/RData from GHDx exports here (not included)
outputs/
  figures/    # plots written by the scripts
  tables/     # CSVs written by the scripts
```

## How to run
1. Open R (>= 4.2). Install packages required by the scripts.
2. Ensure all required input files (e.g., `204counties_1990to2021.csv`, `GBD_maps.RData`, etc.) are placed under `data/raw/`.
3. Set your working directory to the project root. Update any `setwd()` paths if needed.
4. Run:
```r
source("code/inequality/inequality_analysis_clean.R")
source("code/risk/paf_analysis_clean.R")
```

> **Note**: GBD-derived datasets are not redistributed here. Please obtain them from GHDx with appropriate terms of use.
