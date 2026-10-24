# Run from the website root in RStudio:
# source("take home exercise1/R/download-data.R")
# Acquisition only: this script does not filter records or perform analysis.
local({
  needed <- c("jsonlite", "digest")
  missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Install these R packages first: ", paste(missing, collapse = ", "))

  exercise_dir <- "take home exercise1"
  manifest_path <- file.path(exercise_dir, "data/metadata/downloads.json")
  if (!file.exists(manifest_path)) stop("Open ISSS626-GAA.Rproj and run from the website root.")
  manifest <- jsonlite::fromJSON(manifest_path)
  boundary <- jsonlite::fromJSON(file.path(exercise_dir, "data/metadata/geoboundaries-adm1.json"))
  raw_dir <- file.path(exercise_dir, "data/raw")
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  old_timeout <- getOption("timeout")
  options(timeout = max(300, old_timeout))
  on.exit(options(timeout = old_timeout), add = TRUE)

  verify_file <- function(path, name) {
    expected <- manifest$sha256[match(name, manifest$file)]
    if (is.na(expected)) stop("No reference checksum for ", name)
    actual <- digest::digest(file = path, algo = "sha256", serialize = FALSE)
    if (!identical(actual, expected)) {
      stop("Checksum differs for ", name,
           ". Preserve the original file and investigate the source version; do not silently replace the manifest.")
    }
    invisible(TRUE)
  }

  fetch_file <- function(name, url) {
    destination <- file.path(raw_dir, name)
    if (file.exists(destination)) {
      verify_file(destination, name)
      message("Verified cached input: ", name)
      return(invisible(destination))
    }
    temporary <- tempfile(fileext = paste0(".", tools::file_ext(name)))
    on.exit(unlink(temporary), add = TRUE)
    status <- download.file(url, temporary, mode = "wb", quiet = TRUE)
    if (!identical(status, 0L)) stop("Download failed: ", name)
    verify_file(temporary, name)
    if (!file.copy(temporary, destination, overwrite = FALSE)) stop("Could not save ", name)
    message("Downloaded and verified: ", name)
    invisible(destination)
  }

  csv_name <- "thai_road_accident_2019_2022.csv"
  parquet_name <- "thai_road_accident_2019_2022.parquet"
  for (name in c(csv_name, parquet_name)) {
    destination <- file.path(raw_dir, name)
    if (file.exists(destination)) verify_file(destination, name)
  }
  if (!all(file.exists(file.path(raw_dir, c(csv_name, parquet_name))))) {
    archive <- fetch_file("thailand-road-accident-2019-2022.zip",
      "https://www.kaggle.com/api/v1/datasets/download/thaweewatboy/thailand-road-accident-2019-2022")
    unpacked <- tempfile("thailand-inputs-")
    dir.create(unpacked)
    on.exit(unlink(unpacked, recursive = TRUE), add = TRUE)
    unzip(archive, files = c(csv_name, parquet_name), exdir = unpacked)
    for (name in c(csv_name, parquet_name)) {
      source_path <- file.path(unpacked, name)
      verify_file(source_path, name)
      if (!file.exists(file.path(raw_dir, name)) &&
          !file.copy(source_path, file.path(raw_dir, name))) stop("Could not save ", name)
    }
  }
  fetch_file("geoBoundaries-THA-ADM1.geojson", boundary$gjDownloadURL)

  checked_names <- c(csv_name, parquet_name, "geoBoundaries-THA-ADM1.geojson")
  checked <- data.frame(
    file = checked_names,
    bytes = file.info(file.path(raw_dir, checked_names))$size,
    sha256 = vapply(file.path(raw_dir, checked_names),
      function(path) digest::digest(file = path, algo = "sha256", serialize = FALSE), character(1)),
    row.names = NULL
  )
  print(checked, row.names = FALSE)
  message("Original inputs verified. Run prepare.R and analysis.R, or render the technical report.")
})
