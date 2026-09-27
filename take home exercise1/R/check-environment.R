# Verify every package imported by this exercise before running the analysis.
local({
  required <- c('sf','dplyr','readr','ggplot2','spatstat.geom','spatstat.explore',
                'spatstat.random','knitr','rmarkdown','jsonlite','digest')
  versions <- vapply(required,function(p) {
    if(requireNamespace(p,quietly=TRUE)) as.character(packageVersion(p)) else 'not installed'
  },character(1))
  print(data.frame(package=required,version=unname(versions)),row.names=FALSE)
  cat('\n',R.version.string,'\n',sep='')
  quarto_path <- Sys.which('quarto')
  if(any(versions=='not installed') || !nzchar(quarto_path)) {
    stop('Install the missing packages and Quarto before running this exercise.')
  }
  system2(quarto_path,'--version')
  invisible(versions)
})
