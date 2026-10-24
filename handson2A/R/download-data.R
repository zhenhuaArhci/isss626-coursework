# Run from the website root. Downloads are cached, never silently refreshed.
raw_dir <- "handson2A/data/raw"
options(timeout = max(300, getOption("timeout")))
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
sources <- c(
  "childcare.geojson" = "d_5d668e3f544335f8028f546827b773b4",
  "mp19-subzone.geojson" = "d_8594ae9ff96d0c708bc2af633048edfb"
)
for (filename in names(sources)) {
  destination <- file.path(raw_dir, filename)
  if (file.exists(destination)) {
    sf::st_read(destination, quiet = TRUE)
    next
  }
  endpoint <- paste0("https://api-open.data.gov.sg/v1/public/api/datasets/",
                     sources[[filename]], "/poll-download")
  for (attempt in 1:5) {
    response <- try(jsonlite::fromJSON(endpoint), silent = TRUE)
    if (!inherits(response, "try-error") && !is.null(response$data$url)) break
    if (attempt == 5) stop("Download unavailable: ", filename)
    Sys.sleep(12)
  }
  # Download atomically; omit signed download URLs from diagnostic messages.
  partial <- tempfile(fileext = ".geojson")
  ok <- tryCatch({
    suppressWarnings(download.file(response$data$url, partial, mode = "wb", quiet = TRUE))
    sf::st_read(partial, quiet = TRUE)
    file.copy(partial, destination)
  }, error = function(e) FALSE)
  unlink(partial)
  if (!isTRUE(ok)) stop("Download or validation failed for ", filename, "; rerun this script.")
  message("Downloaded ", filename)
  Sys.sleep(11)
}
manifest <- data.frame(
  file = names(sources), dataset_id = unname(sources),
  bytes = file.info(file.path(raw_dir, names(sources)))$size,
  md5 = unname(tools::md5sum(file.path(raw_dir, names(sources))))
)
print(manifest, row.names = FALSE)
