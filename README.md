# ISSS626 Coursework Studio

Zhenhua Liu's coursework website for **ISSS626 Geospatial Analytics and Applications**.

## Local workflow

Open `ISSS626-GAA.Rproj` in RStudio, then render from the Build pane or run:

```r
source("R/render.R")
```

The rendered website is written to `_site/`. That folder is committed so Netlify can publish the static site without installing R or Quarto during deployment.

The project-level `.Renviron` selects a Windows-compatible UTF-8 locale so R can render the site without locale warnings.

## Project structure

- `_quarto.yml` — site navigation and global settings
- `index.qmd` — studio homepage
- `hands-on.qmd` — guided exercise index
- `take-home.qmd` — independent project index
- `about.qmd` — profile and workflow
- `styles.css` — responsive visual system
- `netlify.toml` — static publishing configuration
