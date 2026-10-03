# Run from the Quarto project root. Only data files are downloaded.
dir.create("handson3/data/raw", recursive = TRUE, showWarnings = FALSE)
revision <- "0f695aaacf44676f3d82aff6747873ce614168bb"
base_url <- paste0("https://raw.githubusercontent.com/endurrus/IS415-GAA/",
  revision, "/In-Class_Ex/In-Class_Ex04/data/rawdata/")
files <- c(paste0("Kepulauan_Bangka_Belitung.", c("cpg", "dbf", "prj", "shp", "shx")),
           "forestfires.csv")
for (file in files) {
  target <- file.path("handson3/data/raw", file)
  if (!file.exists(target)) download.file(paste0(base_url, file), target, mode = "wb")
}
manifest <- read.csv("handson3/data/input-checksums.csv", stringsAsFactors = FALSE)
actual <- tools::md5sum(file.path("handson3/data/raw", manifest$file))
stopifnot(identical(unname(actual), manifest$md5))
cat("Pinned classroom data verified against the recorded input checksums.\n")
