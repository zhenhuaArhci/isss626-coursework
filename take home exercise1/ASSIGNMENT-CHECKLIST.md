# Assignment coverage

| Requirement | Implementation |
|---|---|
| Thailand 2022 option; define Greater Bangkok | Report sections 1–2; six declared ISO-coded polygons |
| Source acquisition and quality audit in R | `R/download-data.R`, `R/prepare.R`; hashes, missingness, duplicates, province-label audit |
| First-order analysis | Calendar-day-adjusted monthly counts; Gaussian planar KDE at 1/2/3 km |
| Second-order analysis | Border-corrected L and 199 conditional CSR simulations; 999 stratified fatal-label permutations |
| Assumptions and sensitivity | Boundary, CRS, source coverage, distance, edge treatment, null models, repeated coordinates and bandwidth checks |
| Integrated interpretation | Report section 5 |
| Each major figure interpretation <=150 words | Five explicitly marked interpretation blocks |
| Public-safety discussion <=500 words | One explicitly marked planning block |
| Executive summary | Quarto revealjs; cover + contents + 10 content slides |
| Reproducibility | Source scripts, provenance, seeds, `sessionInfo()` and reproduction guide |
| Website/GitHub deliverables | Exercise navigation, report/slides links; source and `_site/` published together |

The analysis is exploratory and uses planar distances. No road-network estimate, traffic-exposure-adjusted risk or causal effect is claimed.
