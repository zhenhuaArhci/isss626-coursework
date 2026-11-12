# Run from the ISSS626-GAA RStudio project or call run.R from this exercise.
raw <- file.path(exercise, "data", "raw")
dir.create(raw, recursive = TRUE, showWarnings = FALSE)

ridership_repo <- paste0(
  "https://raw.githubusercontent.com/uditmall/Singapore-MRT-LRT-station-passenger-volume/",
  "90960b2dd1270f9691522ae4df31226be2552dc5/data/"
)
coordinate_repo <- paste0(
  "https://raw.githubusercontent.com/elliotwutingfeng/singapore_train_station_coordinates/",
  "9f5972c71db8a529801eee9c527946a45a78e409/"
)
datamall <- "https://datamall.lta.gov.sg/content/dam/datamall/datasets/Geospatial/"
dgs <- "https://api-open.data.gov.sg/v1/public/api/datasets/"
months <- c("2026-08", "2026-09", "2026-10")

sources <- c(
  "stations.zip" = paste0(datamall, "TrainStation_Sep2026.zip"),
  "bus-stops.zip" = paste0(datamall, "BusStopLocation_Sep2026.zip"),
  "future-stations.csv" = paste0(coordinate_repo, "future_stations.csv"),
  "operational-codes.csv" = paste0(coordinate_repo, "operational_stations.csv"),
  "ridership-stations.js" = paste0(ridership_repo, "stations.js"),
  "ridership-manifest.js" = paste0(ridership_repo, "manifest.js"),
  setNames(paste0(ridership_repo, months, ".js"), paste0(months, ".js")),
  "population.tif" = paste0(
    "https://data.worldpop.org/GIS/Population/Global_2015_2030/R2025A/2025/",
    "SGP/v1/100m/constrained/sgp_pop_2025_CN_100m_R2025A_v1.tif"
  ),
  "planning-areas.geojson" = paste0(dgs, "d_4765db0e87b9c86336792efe8a1f7a66/poll-download"),
  "hdb.geojson" = paste0(dgs, "d_16b157c52ed637edd6ba1232e026258d/poll-download")
)

download_input <- function(name, url) {
  path <- file.path(raw, name)
  if (file.exists(path) && file.size(path) > 0) return(invisible(path))
  tmp <- tempfile(tmpdir = raw)
  on.exit(unlink(tmp))
  for (attempt in seq_len(3)) {
    result <- tryCatch({
      target <- url
      if (endsWith(url, "/poll-download")) {
        info <- jsonlite::fromJSON(rawToChar(curl::curl_fetch_memory(url)$content))
        target <- info$data$url
        if (is.null(target)) stop("No download URL returned for ", name)
      }
      curl::curl_download(target, tmp, quiet = TRUE)
      if (!file.rename(tmp, path)) stop("Could not store ", name)
      TRUE
    }, error = identity)
    if (isTRUE(result)) return(invisible(path))
    if (attempt < 3) Sys.sleep(attempt)
  }
  stop("Download failed for ", name, ": ", conditionMessage(result), call. = FALSE)
}
invisible(Map(download_input, names(sources), sources))
write.csv(
  data.frame(file = names(sources), source = unname(sources),
             md5 = unname(tools::md5sum(file.path(raw, names(sources))))),
  file.path(exercise, "data", "input-checksums.csv"), row.names = FALSE
)

read_shape_zip <- function(name) {
  zip <- file.path(raw, name)
  entries <- unzip(zip, list = TRUE)$Name
  shape <- grep("\\.shp$", entries, value = TRUE, ignore.case = TRUE)
  stopifnot(length(shape) == 1L)
  target <- file.path(raw, tools::file_path_sans_ext(name))
  unzip(zip, exdir = target)
  sf::st_read(file.path(target, shape), quiet = TRUE)
}

read_array <- function(name, prefix, suffix) {
  txt <- paste(readLines(file.path(raw, name), warn = FALSE, encoding = "UTF-8"), collapse = "")
  if (!startsWith(txt, prefix) || !endsWith(txt, suffix)) stop("Unexpected JSON wrapper: ", name)
  jsonlite::fromJSON(substr(txt, nchar(prefix) + 1L, nchar(txt) - nchar(suffix)),
                     simplifyVector = FALSE)
}
