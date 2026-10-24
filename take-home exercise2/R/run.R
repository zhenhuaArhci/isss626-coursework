# Run from the root of ISSS626-GAA.Rproj, e.g. source("take-home exercise2/R/run.R").
run_exercise2 <- function(force=FALSE) {
  options(warn=2)
  source("take-home exercise2/R/cache.R")
  sig <- "take-home exercise2/data/derived/pipeline-signature.txt"
  inputs <- "take-home exercise2/data/input-checksums.csv"
  outputs <- c("data/derived/prepared.rds","data/derived/analysis.rds",
    "results/lmsa.csv","results/ehsa.csv","results/space-time-gi.csv",
    "results/spatial-scan-windows.csv",sprintf("figures/%s.png",c(
      "01-township-counts","02-monthly-trend","03-cube-heatmap","04-local-moran","05-local-gi",
      "06-gi-fdr","07-ehsa","08-mk-trend","09-gi-trajectories","10-spatial-scan")))
  valid <- !force && file.exists(sig) && file.exists(inputs) &&
    all(file.exists(file.path("take-home exercise2",outputs)))
  if (valid) {
    # Rehash actual available inputs: a changed file must invalidate the cache.
    old <- readr::read_csv(inputs,show_col_types=FALSE)
    overrides <- c(Sys.getenv("ACLED_MYANMAR_FILE"),Sys.getenv("MIMU_TOWNSHIP_FILE"))
    original_names <- c("ACLED_Data_Myanmar_Jan2021-Sep2025.zip",
                        "Myanmar Township Boundaries_MIMU_PCodev9.4.zip")
    local_names <- c(original_names[1],"mimu-v94.zip")
    local_files <- vapply(seq_along(local_names),function(j) {
      candidates <- c(overrides[j],file.path("take-home exercise2/data/raw",local_names[j]),
        file.path(path.expand("~"),"Downloads",original_names[j]),
        file.path(Sys.getenv("USERPROFILE"),"Downloads",original_names[j]))
      found <- candidates[nzchar(candidates) & file.exists(candidates)]
      if(length(found)) found[1] else ""
    },character(1))
    if (all(file.exists(local_files))) {
      hashes <- vapply(local_files,digest::digest,character(1),algo="sha256",file=TRUE)
      valid <- identical(unname(hashes),old$sha256)
    } else valid <- FALSE
    valid <- valid && identical(readLines(sig,warn=FALSE),pipeline_signature())
  }
  if (!valid) {
    source("take-home exercise2/R/verify-methods.R",local=environment())
    source("take-home exercise2/R/prepare.R",local=environment())
    source("take-home exercise2/R/analysis.R",local=environment())
    source("take-home exercise2/R/figures.R",local=environment())
    write_cache_manifest()
  } else message("Reusing verified results: matching input, code, R and package fingerprints.")
  invisible(TRUE)
}
run_exercise2(force=identical(Sys.getenv("TH2_FORCE"),"1"))
