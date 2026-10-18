# Run once from RStudio; the analysis itself never installs packages.
packages <- c("sf", "dplyr", "readr", "tidyr", "tmap", "ggplot2", "accessibility", "knitr", "rmarkdown", "remotes")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
revision <- "b96fb58adcd0e476cbb634aeb6ce27981fbc1dc3"
installed <- if (requireNamespace("catchment", quietly = TRUE)) packageDescription("catchment")$RemoteSha else NULL
if (!identical(installed, revision)) {
  remotes::install_github(paste0("uva-bi-sdad/catchment@", revision), upgrade = "never")
}
