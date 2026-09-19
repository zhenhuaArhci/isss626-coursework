## ----packages
library(sf)
library(dplyr)
library(spatstat.geom)
library(spatstat.explore)
library(spatstat.random)
library(terra)
library(tmap)

## ----import
raw_dir <- "handson2A/data/raw"
required <- file.path(raw_dir, c("childcare.geojson", "mp19-subzone.geojson"))
if (!all(file.exists(required))) stop("Run source('handson2A/R/download-data.R') first.")
childcare_raw <- st_read(required[1], quiet = TRUE)
subzones_raw <- st_read(required[2], quiet = TRUE)
input_audit <- data.frame(
  dataset = c("Child Care Services", "MP2019 subzones"),
  records = c(nrow(childcare_raw), nrow(subzones_raw)),
  geometry = c(as.character(unique(st_geometry_type(childcare_raw))),
               as.character(unique(st_geometry_type(subzones_raw)))),
  epsg = c(st_crs(childcare_raw)$epsg, st_crs(subzones_raw)$epsg),
  empty = c(sum(st_is_empty(childcare_raw)), sum(st_is_empty(subzones_raw))),
  invalid = c(sum(!st_is_valid(childcare_raw)), sum(!st_is_valid(subzones_raw)))
)
input_audit

## ----prepare-window
stopifnot(!any(st_is_empty(childcare_raw)), !any(st_is_empty(subzones_raw)),
          all(st_geometry_type(childcare_raw) == "POINT"))
childcare <- st_transform(childcare_raw, 3414)
subzones <- st_transform(st_make_valid(subzones_raw), 3414)
subzones <- filter(subzones, SUBZONE_N != "SOUTHERN GROUP",
                   !PLN_AREA_N %in% c("WESTERN ISLANDS", "NORTH-EASTERN ISLANDS"))
stopifnot(st_crs(childcare) == st_crs(subzones), !st_is_longlat(childcare))
singapore <- st_union(subzones)
sg_window <- as.owin(singapore)
xy <- st_coordinates(childcare)
inside <- inside.owin(xy[, 1], xy[, 2], sg_window)
duplicate_location <- duplicated(as.data.frame(xy))
# Co-located records may be different operators. Keep them for service intensity.
childcare_ppp <- ppp(xy[inside, 1], xy[inside, 2], window = sg_window,
                     checkdup = FALSE)
unitname(childcare_ppp) <- c("metre", "metres")
childcare_unique <- unique(childcare_ppp)
quality_audit <- data.frame(
  measure = c("Imported records", "Retained subzones", "Outside study window",
              "Records at repeated coordinates (beyond first)", "Records in window",
              "Distinct locations in window", "Study area (km2)"),
  value = c(nrow(childcare), nrow(subzones), sum(!inside), sum(duplicate_location),
            npoints(childcare_ppp), npoints(childcare_unique), area.owin(sg_window) / 1e6)
)
quality_audit
dir.create("handson2A/data/derived", recursive = TRUE, showWarnings = FALSE)
saveRDS(subzones, "handson2A/data/derived/subzones-clean.rds")

## ----planning-areas
area_names <- c("PUNGGOL", "TAMPINES", "CHOA CHU KANG", "JURONG WEST")
areas <- lapply(area_names, function(nm) st_union(filter(subzones, PLN_AREA_N == nm)))
names(areas) <- area_names
area_windows <- lapply(areas, as.owin)
area_patterns <- lapply(area_windows, function(w) childcare_ppp[w])
area_summary <- data.frame(
  planning_area = area_names,
  records = vapply(area_patterns, npoints, integer(1)),
  locations = vapply(area_patterns, function(x) npoints(unique(x)), integer(1)),
  km2 = vapply(area_windows, area.owin, numeric(1)) / 1e6
) |>
  mutate(records_per_km2 = records / km2)
area_summary
