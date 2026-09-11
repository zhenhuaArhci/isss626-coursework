library(sf)
library(tidyverse)

data_dir <- "hands-on-exercise-1a/data/raw"

mpsz <- st_read(file.path(data_dir, "mp14-subzone.geojson"), quiet = TRUE) |>
  st_make_valid() |>
  st_transform(3414)

preschool <- st_read(file.path(data_dir, "preschools.geojson"), quiet = TRUE) |>
  st_transform(3414)

cycling <- st_read(file.path(data_dir, "cycling-paths.geojson"), quiet = TRUE) |>
  st_transform(3414)

airbnb <- read_csv(file.path(data_dir, "airbnb-listings.csv"), show_col_types = FALSE) |>
  filter(!is.na(longitude), !is.na(latitude)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(3414)

mpsz_summary <- mpsz |>
  mutate(
    preschool_count = lengths(st_intersects(mpsz, preschool)),
    area_km2 = as.numeric(st_area(geometry)) / 1e6,
    preschool_density = preschool_count / area_km2
  )

tampines_west <- mpsz |>
  filter(SUBZONE_N == "TAMPINES WEST")

cycling_in_tampines <- st_intersection(cycling, tampines_west)
cycling_buffer <- st_buffer(cycling_in_tampines, dist = 5)
cycling_buffer_union <- st_union(cycling_buffer)

results_1a <- list(
  mpsz = mpsz,
  preschool = preschool,
  cycling = cycling,
  airbnb = airbnb,
  mpsz_summary = mpsz_summary,
  tampines_west = tampines_west,
  cycling_buffer = cycling_buffer,
  cycling_buffer_union = cycling_buffer_union
)
