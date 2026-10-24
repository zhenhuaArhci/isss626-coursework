dir.create("hands-on-exercise-1a/data/raw", recursive = TRUE, showWarnings = FALSE)

download_datagov <- function(dataset_id, destination) {
  poll_url <- paste0(
    "https://api-open.data.gov.sg/v1/public/api/datasets/",
    dataset_id,
    "/poll-download"
  )

  response <- jsonlite::fromJSON(poll_url)
  if (!identical(response$code, 0L) || is.null(response$data$url)) {
    stop("data.gov.sg did not return a download URL for ", dataset_id)
  }

  download.file(response$data$url, destination, mode = "wb", quiet = FALSE)
}

files <- c(
  subzones = "hands-on-exercise-1a/data/raw/mp14-subzone.geojson",
  preschools = "hands-on-exercise-1a/data/raw/preschools.geojson",
  cycling = "hands-on-exercise-1a/data/raw/cycling-paths.geojson",
  airbnb = "hands-on-exercise-1a/data/raw/airbnb-listings.csv"
)

datagov_ids <- c(
  subzones = "d_226cacceceff94f0c8b814962a5307c9",
  preschools = "d_61eefab99958fd70e6aab17320a71f1c",
  cycling = "d_8f468b25193f64be8a16fa7d8f60f553"
)

for (name in names(datagov_ids)) {
  if (!file.exists(files[[name]])) {
    download_datagov(datagov_ids[[name]], files[[name]])
    Sys.sleep(11)
  }
}

airbnb_url <- paste0(
  "https://data.insideairbnb.com/singapore/sg/singapore/",
  "2026-06-29/visualisations/listings.csv"
)

if (!file.exists(files[["airbnb"]])) {
  download.file(airbnb_url, files[["airbnb"]], mode = "wb", quiet = FALSE)
}

message("Exercise 1A data are ready in hands-on-exercise-1a/data/raw")
