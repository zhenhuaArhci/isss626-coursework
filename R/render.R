#!/usr/bin/env Rscript

# Render the complete Quarto coursework website from R.
quarto <- Sys.which("quarto")
if (!nzchar(quarto)) {
  stop("Quarto was not found on PATH. Install Quarto before rendering.")
}

status <- system2(quarto, c("render", "."))
if (!identical(status, 0L)) {
  stop("Quarto render failed with exit status ", status)
}

message("Coursework website rendered to _site/")

