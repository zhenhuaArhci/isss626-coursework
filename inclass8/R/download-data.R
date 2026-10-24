# Download data only; never execute code from the data mirror.
revision <- "78e7216992f8fc46abaff599de4f15224805db57"
base_url <- paste0("https://raw.githubusercontent.com/andrewhuangk/Geospatial/",
  revision, "/Hands-on_Ex/Hands-on_Ex08/data/")
files <- c("aspatial/pop2019.csv", "aspatial/Store_sales_data.csv",
  "aspatial/storeCD.csv", unlist(lapply(c("STORES_TC", "TOWN_TC", "VILLAGE_TC"),
    function(x) paste0("geospatial/", x, ".", c("shp", "shx", "dbf", "prj", "cpg")))))
for (file in files) {
  target <- file.path("inclass8/data/raw", file)
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  if (!file.exists(target)) download.file(paste0(base_url, file), target, mode = "wb")
}
manifest_file <- "inclass8/data/input-checksums.csv"
actual <- unname(tools::md5sum(file.path("inclass8/data/raw", files)))
if (file.exists(manifest_file)) {
  manifest <- read.csv(manifest_file)
  stopifnot(identical(files, manifest$file), identical(actual, manifest$md5))
} else {
  write.csv(data.frame(file = files, md5 = actual), manifest_file, row.names = FALSE)
}
cat("All 18 pinned teaching input files downloaded and verified.\n")
