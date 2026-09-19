# ISSS626 Coursework Studio

Zhenhua Liu's coursework website for **ISSS626 Geospatial Analytics and Applications**. The formal local project is stored at `C:\zhenhuaArhci`.

## Published links

- Coursework website: <https://isss626-gaa-zhenhua-liu.netlify.app/>
- GitHub repository: <https://github.com/zhenhuaArhci/isss626-coursework>

## Local workflow

Open `ISSS626-GAA.Rproj` in RStudio, then render from the Build pane or run:

```r
source("hands-on-exercise-1a/R/download-data.R")
source("hands-on-exercise-1b/R/download-data.R")
source("handson2A/R/download-data.R") # shared inputs for Exercises 2A and 2B
source("R/render.R")
```

The download commands are only needed on a new computer. Raw source data
stay in local ignored folders, while the rendered website is written to
`_site/`. That output is committed so Netlify can publish the static site
without installing R or Quarto during deployment.

The project-level `.Renviron` selects a Windows-compatible UTF-8 locale so R can render the site without locale warnings.

## Project structure

- `_quarto.yml` — site navigation and global settings
- `index.qmd` — simple course homepage
- `hands-on-exercise-1a/` — Exercise 1A page, R scripts and data notes
- `hands-on-exercise-1b/` — Exercise 1B page, R scripts and data notes
- `handson2A/` — first-order point patterns, nearest neighbours and KDE
- `handson2B/` — G/F/K/L functions, CSR simulations and interpretation
- `about.qmd` — short course and author page
- `styles.css` — minimal light-blue page styling
- `netlify.toml` — static publishing configuration

## Exercises 2A and 2B

These pages follow course Chapters 4 and 5. They reuse the existing theme and
exercise layout. The shared preparation in `handson2A/R/prepare.R` is executed
independently by each page. Labelled code sections are read directly into Quarto
with `knitr::read_chunk()`, so displayed and executed code remain identical.
Exercise 2A retains its fully executed output for fast whole-site builds.
To refresh it after an external R script or input changes, explicitly run
`quarto render handson2A/index.qmd`. Exercise 2B executes on each build.

Install missing packages before rendering:

```r
needed <- c("sf", "dplyr", "spatstat.geom", "spatstat.explore", "spatstat.random",
            "terra", "tmap", "jsonlite", "knitr", "rmarkdown")
missing <- setdiff(needed, rownames(installed.packages()))
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
```

For a whole-site build, run `quarto render . --execute`. Monte Carlo
simulations and adaptive KDE can take several minutes. The published pages
record package versions with `sessionInfo()` and document data dates, duplicate
coordinates, CRS units, simulation seeds and interpretation limits. See
`handson2A/data/README.md` for input checksums. Do not refresh inputs silently:
compare checksums and feature counts before interpreting a changed result.

Netlify publishes the committed `_site/` directory from the existing `main`
branch without a remote R build. After a full local render, commit the source
and rendered assets together; pushing to `main` triggers the connected site.
