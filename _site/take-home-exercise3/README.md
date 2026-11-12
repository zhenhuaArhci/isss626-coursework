# Reproduce Take-home Exercise 3

Open the repository’s **ISSS626-GAA.Rproj** in RStudio. Use R 4.5 or newer and a recent Quarto installation. In the Console, install packages if they are missing.

```r
required <- c("sf", "terra", "data.table", "ggplot2", "scales",
              "curl", "jsonlite", "knitr", "rmarkdown")
missing <- setdiff(required, rownames(installed.packages()))
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
```

Run the full pipeline from the project root.

```r
source("take-home-exercise3/R/run.R")
```

The first run downloads the inputs over HTTPS. The public station aggregates require no LTA DataMall credentials. Subsequent runs reuse downloaded files under `data/raw/`. The R workflow rebuilds figures and results and stops on data-contract or conservation failures. It takes a few minutes, depending on the machine. See [data provenance](data/README.md) for source vintages and licences.

With the same project open, render the entire website from the RStudio Terminal or a shell in the project root.

```sh
quarto render
```

The technical report runs the analysis in its first chunk. The executive summary reads its generated CSV results. To render only the executive summary after the R workflow, use `quarto render take-home-exercise3/executive-summary.qmd`.

| Location | Contents |
| --- | --- |
| `R/download-data.R` | Input acquisition and MD5 manifest |
| `R/prepare-data.R` | Station matching, tap-ins, projected origins and station attraction data |
| `R/fit-model.R` | Huff model, temporal/geographic validation, four scenarios and accessibility |
| `R/visualise.R` | Six figures for the report and slides |
| `results/` | Origin and destination tables, market-area grid, coefficient estimates and validation |
| `data/derived/interactions-*.csv.gz` | Both full origin–destination interaction tables |
| `data/derived/station_month_tap_ins.csv` | Station-complex tap-in targets by month |

The website publishes figures, R code, station and origin results and samples of the pair tables. Run the script locally for the full pair tables and the archived source files.
