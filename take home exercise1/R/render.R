# Run from the website root in RStudio:
# source("take home exercise1/R/render.R")
# Render the website so every page receives the new navigation.
# Quarto reuses available frozen results for existing exercises.
local({
  if (!file.exists("_quarto.yml") || !dir.exists("take home exercise1")) {
    stop("Open ISSS626-GAA.Rproj and run from the website root.")
  }
  quarto_path <- Sys.which("quarto")
  if (!nzchar(quarto_path)) stop("Quarto was not found on PATH.")
  status <- system2(quarto_path, c("render", ".", "--use-freezer"))
  if (!identical(status, 0L)) stop("Website render failed (exit ", status, ").")
  message("Draft exercise rendered to _site/take home exercise1/. Analysis is still pending.")
})
