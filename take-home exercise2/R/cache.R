# Cache identity includes both archives, all computation scripts and R/package versions.
pipeline_signature <- function() {
  scripts <- file.path("take-home exercise2/R",c("methods.R","prepare.R","analysis.R","figures.R"))
  inputs <- readr::read_csv("take-home exercise2/data/input-checksums.csv",show_col_types=FALSE)
  pkgs <- c("sf","spdep","Kendall","dplyr","tidyr","ggplot2","Matrix")
  digest::digest(list(input_sha256=inputs$sha256,
    scripts=vapply(scripts,digest::digest,character(1),algo="sha256",file=TRUE),
    r=R.version.string,packages=vapply(pkgs,function(p)as.character(utils::packageVersion(p)),character(1))),algo="sha256")
}
write_cache_manifest <- function() {
  writeLines(pipeline_signature(),"take-home exercise2/data/derived/pipeline-signature.txt")
  jsonlite::write_json(list(analysis_date="2026-10-23",seed=20261023,
    permutation_replicates=999,block_replicates=499,alpha=.05,
    spatial_weights="Queen contiguity, 1 metre snap; symmetric nearest representative-point bridge for each island",
    time_neighbourhood="Current and previous month; first month current only",
    variables=c("armed_events","civilian_events"),fingerprint=pipeline_signature()),
    "take-home exercise2/results/analysis-manifest.json",pretty=TRUE,auto_unbox=TRUE)
  capture.output(sessionInfo(),file="take-home exercise2/results/session-info.txt")
}
