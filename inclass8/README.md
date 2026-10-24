# inclass8 — Huff market areas

Implementation checklist (24 October 2026)

- Follow [Chapter 18](https://r4gdsa.netlify.app/chap18): import, joins, point-on-surface distances, interaction matrix, MCI Huff shares, BT probabilities and population-weighted potential.
- Extend the lesson to dominant-store trade areas at village resolution, as in the supplied classroom photos. Preserve all 28 store codes in one categorical legend on the right, village borders, grid, north arrow and scale. Use a 14 × 12 inch figure and show executable R code.
- Keep raw teaching inputs local, pin the public data mirror and verify checksums. Do not copy another student's analysis or prose.
- Verify IDs, 622 input / 617 matched villages, 28 stores, 17,276 pairs, per-origin probability sums, manual formula equivalence, population conservation and all legend categories.
- Render the page and all site navigation locally; check map appearance and published image paths. Preserve existing user edits.
- Commit once and push to GitHub; rely on the existing automatic Netlify deployment. Do not run a second manual production deployment.

Reproduce from the project root:

```r
source("inclass8/R/download-data.R")
source("inclass8/R/analysis.R")
source("inclass8/R/verify.R")
```

Then run `quarto render inclass8/index.qmd`. R packages: sf, dplyr, tidyr, readr, MCI, tmap, cols4all, ggplot2, knitr (and their dependencies, including units and tibble).
