# Run from the website project root in RStudio.
local({
  if(!file.exists('_quarto.yml') || !dir.exists('take home exercise1'))
    stop('Open ISSS626-GAA.Rproj and run from the website root.')
  source('take home exercise1/R/check-environment.R')
  source('take home exercise1/R/download-data.R')
  # This report has freeze:false; previous exercises may reuse their frozen outputs.
  status <- system2(Sys.which('quarto'),c('render','.','--use-freezer'))
  if(!identical(status,0L)) stop('Website render failed (exit ',status,').')
  message('Website rendered to _site/. Verify the HTML before publishing.')
})
