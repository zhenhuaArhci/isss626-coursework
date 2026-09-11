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
source("R/render.R")
```

The two download commands are only needed on a new computer. Raw source data
stay in local ignored folders, while the rendered website is written to
`_site/`. That output is committed so Netlify can publish the static site
without installing R or Quarto during deployment.

The project-level `.Renviron` selects a Windows-compatible UTF-8 locale so R can render the site without locale warnings.

## Project structure

- `_quarto.yml` — site navigation and global settings
- `index.qmd` — simple course homepage
- `hands-on-exercise-1a/` — Exercise 1A page, R scripts and data notes
- `hands-on-exercise-1b/` — Exercise 1B page, R scripts and data notes
- `about.qmd` — short course and author page
- `styles.css` — minimal light-blue page styling
- `netlify.toml` — static publishing configuration
