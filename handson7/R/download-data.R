# Run from the coursework project root. Downloads data only, never remote code.
revision <- "acaa1f9995d3d778a02a09c6bbdd5ef4f4eae773"
base_url <- paste0("https://raw.githubusercontent.com/tskam/IS415/", revision,
                   "/Hands-on_Ex/Hands-on_EX11/data/")
files <- c("aspatial/OD_Matrix.csv",
  paste0("geospatial/MP14_SUBZONE_NO_SEA_PL.", c("shp", "shx", "dbf", "prj")),
  paste0("geospatial/hexagons.", c("shp", "shx", "dbf", "prj", "cpg")),
  paste0("geospatial/ELDERCARE.", c("shp", "shx", "dbf", "prj", "cpg")))
for (file in files) {
  target <- file.path("handson7/data/raw", file)
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  if (!file.exists(target)) download.file(paste0(base_url, file), target, mode = "wb")
}
manifest_file <- "handson7/data/input-checksums.csv"
actual <- unname(tools::md5sum(file.path("handson7/data/raw", files)))
if (file.exists(manifest_file)) {
  manifest <- read.csv(manifest_file)
  stopifnot(identical(files, manifest$file), identical(actual, manifest$md5))
} else {
  write.csv(data.frame(file = files, md5 = actual), manifest_file, row.names = FALSE)
}
cat("All 15 pinned teaching input files downloaded and verified.\n")
