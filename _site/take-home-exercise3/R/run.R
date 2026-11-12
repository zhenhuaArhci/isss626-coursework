# Run with source("take-home-exercise3/R/run.R") from ISSS626-GAA.Rproj.
# Rscript take-home-exercise3/R/run.R also works from the project root.
exercise <- if (dir.exists("take-home-exercise3/R")) "take-home-exercise3" else "."
if (!file.exists(file.path(exercise, "R", "download-data.R"))) {
  stop("Open ISSS626-GAA.Rproj or set the working directory to the exercise folder.")
}
packages <- c("sf", "terra", "data.table", "ggplot2", "scales", "curl", "jsonlite")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install required packages: ", paste(missing, collapse = ", "))

for (step in c("download-data.R", "prepare-data.R", "fit-model.R", "visualise.R")) {
  message("Running ", step)
  source(file.path(exercise, "R", step), local = TRUE, encoding = "UTF-8")
}
message("Exercise 3 complete. Results: ", normalizePath(results, winslash = "/"))
