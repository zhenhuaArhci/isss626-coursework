raw_dir <- "hands-on-exercise-1b/data/raw"
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

download_datagov <- function(dataset_id, destination, attempts = 4) {
  poll_url <- paste0(
    "https://api-open.data.gov.sg/v1/public/api/datasets/",
    dataset_id,
    "/poll-download"
  )

  for (attempt in seq_len(attempts)) {
    response <- try(jsonlite::fromJSON(poll_url), silent = TRUE)
    if (!inherits(response, "try-error") &&
        identical(response$code, 0L) &&
        !is.null(response$data$url)) {
      download.file(response$data$url, destination, mode = "wb", quiet = FALSE)
      return(invisible(destination))
    }
    if (attempt < attempts) Sys.sleep(12)
  }

  stop("data.gov.sg did not return a download URL for ", dataset_id)
}

boundary_file <- file.path(raw_dir, "mp19-subzone.geojson")
if (!file.exists(boundary_file)) {
  download_datagov(
    "d_8594ae9ff96d0c708bc2af633048edfb",
    boundary_file
  )
}

population_zip <- file.path(raw_dir, "singstat-population-2024.zip")
population_url <- paste0(
  "https://www.singstat.gov.sg/files/",
  "08022f5e-e664-4274-bd17-a052be52c478.zip"
)

if (!file.exists(population_zip)) {
  download.file(population_url, population_zip, mode = "wb", quiet = FALSE)
}

population_dir <- file.path(raw_dir, "singstat-population-2024")
if (!dir.exists(population_dir) ||
    length(list.files(population_dir, pattern = "[.]csv$", recursive = TRUE)) == 0) {
  dir.create(population_dir, recursive = TRUE, showWarnings = FALSE)
  unzip(population_zip, exdir = population_dir)
}

message("Exercise 1B data are ready in hands-on-exercise-1b/data/raw")
