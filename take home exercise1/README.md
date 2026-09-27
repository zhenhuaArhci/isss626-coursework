# Take-home Exercise 1 — Greater Bangkok road accidents, 2022

The complete R and Quarto study covers 3,599 geolocated events across Bangkok and five surrounding provinces. It includes a technical report, a revealjs summary with ten content slides, R scripts, five figures, source metadata and a reproduction guide.

## Reproduce in RStudio

Open `ISSS626-GAA.Rproj` at the repository root. Install missing packages listed by the checker, then run:

```r
source("take home exercise1/R/check-environment.R")
source("take home exercise1/R/download-data.R")
source("take home exercise1/R/render.R")
```

Required packages: sf, dplyr, readr, ggplot2, spatstat.geom, spatstat.explore, spatstat.random, knitr, rmarkdown, jsonlite and digest. Tested with R 4.5.3 and Quarto 1.10.18. The report records package versions.

The full-site build executes this report and reuses frozen results for previous exercises. A report-only refresh is `quarto render "take home exercise1/technical-report.qmd"`. To inspect intermediate objects, source `R/prepare.R` followed by `R/analysis.R` from the project root.

## Contents

- `index.qmd`: landing page and deliverable links.
- `technical-report.qmd`: report with executed labelled R sections.
- `executive-summary.qmd`: revealjs slides reusing the generated figures.
- `learning-guide.qmd`: final reproduction guide; its existing route is preserved.
- `R/`: acquisition, validation, preparation, analysis and rendering scripts.
- `data/metadata/`: original metadata and SHA-256 manifest.
- `data/raw/`: locally downloaded original inputs, ignored by Git.
- `data/derived/`: regenerated tables and summary JSON, ignored by Git.
- `figures/`: five R-generated figures, committed with the report.

All spatial distances are planar metres in EPSG:32647. The study does not claim network density or travel-risk estimates. Methods and limitations are explained in the report. Seeds 6262022–6262025 fix the Monte Carlo runs.

## Publication

Source and generated `_site/` pages are versioned together on `main`. Netlify serves `_site/` on the existing coursework domain. The live deliverables are:

- [Report](https://isss626-gaa-zhenhua-liu.netlify.app/take%20home%20exercise1/technical-report.html)
- [Slides](https://isss626-gaa-zhenhua-liu.netlify.app/take%20home%20exercise1/executive-summary.html)
- [Source folder](https://github.com/zhenhuaArhci/isss626-coursework/tree/main/take%20home%20exercise1)

See `data/README.md` for source licenses and acquisition details. Raw data are acquired by script instead of bundled; boundary attribution is retained in figures and the report.
