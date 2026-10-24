# take-home exercise2

Geospatial Analytics for Peace: Myanmar township event patterns, January 2021–September 2025. Prepared by Zhenhua Liu for ISSS626.

- Entry: https://isss626-gaa-zhenhua-liu.netlify.app/take-home%20exercise2/index.html
- Technical report: https://isss626-gaa-zhenhua-liu.netlify.app/take-home%20exercise2/technical-report.html
- Revealjs executive summary: https://isss626-gaa-zhenhua-liu.netlify.app/take-home%20exercise2/executive-summary.html
- GitHub: https://github.com/zhenhuaArhci/isss626-coursework/tree/main/take-home%20exercise2

## Reproduce in RStudio

Open the repository's `ISSS626-GAA.Rproj`, then follow `data/README.md` to supply the two authorized archives. Use R 4.5.3 and Quarto 1.10 or compatible versions. Required R packages: sf, dplyr, tidyr, readr, ggplot2, spdep, Kendall, Matrix, scales, digest, jsonlite, knitr, rmarkdown. Exact installed versions are in `results/session-info.txt`. Install only missing packages from CRAN, if needed.

```r
source("take-home exercise2/R/verify-methods.R")
source("take-home exercise2/R/run.R")
source("take-home exercise2/R/verify.R")
```

Render the whole site in a terminal with `quarto render`, or just the report with `quarto render "take-home exercise2/technical-report.qmd"`. The report invokes the pipeline. Render the summary after the report, since the summary reads its derived results. To recompute everything explicitly:

```r
Sys.setenv(TH2_FORCE="1")
source("take-home exercise2/R/run.R")
Sys.unsetenv("TH2_FORCE")
```

The 999-replicate space-time conditional permutations are the slowest step. Reusing outputs is allowed only when input hashes, script hashes, R version and package versions match and all required outputs exist. This avoids stale-result reuse after code or input changes.

## Files

- `R/prepare.R`: source discovery, checksums, schema checks, spatial containment and complete monthly cube.
- `R/methods.R`: analytical Gi*, temporal graph, Mann–Kendall, history classification, block checks and scan helpers.
- `R/analysis.R`: 999-permutation LMSA, space-time Gi*, EHSA, sensitivity and conditional spatial scan.
- `R/figures.R`: ten static analytical graphics.
- `R/run.R`, `R/cache.R`: end-to-end execution and content-based cache identity.
- `R/verify-methods.R`, `R/verify.R`: formula checks and full-output invariants.
- `results/`: aggregates, significance tables, parameters and validation.
- `data/raw/`, `data/derived/`: local only; never committed or deployed.

The executive summary contains nine substantive slides plus title and contents, within the assignment limit of ten. The complete analysis explains the purpose of each step, exploratory thresholds, FDR correction and the interpretation of zero/grey cells. It does not claim that recorded cold spots are safe or that historical patterns establish current conditions.
