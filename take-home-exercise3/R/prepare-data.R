library(sf)
library(data.table)

derived <- file.path(exercise, "data", "derived")
results <- file.path(exercise, "results")
figures <- file.path(exercise, "figures")
for (folder in c(derived, results, figures)) dir.create(folder, recursive = TRUE, showWarnings = FALSE)

project_crs <- 3414  # SVY21 / Singapore TM; metres
station_rows <- read_array("ridership-stations.js", "window.MRT_STATIONS=", ";")
manifest <- read_array("ridership-manifest.js", "window.MRT_MANIFEST=", ";")
stations <- rbindlist(lapply(station_rows, function(x) {
  data.table(destination = x[[1]], station = x[[2]], mirror_lon = x[[4]], mirror_lat = x[[3]])
}))
# The mirror has two fare-code rows for Stevens. Aggregate its boardings before fitting.
mirror_ids <- stations$destination
stations[, destination := fifelse(station == "Stevens", "DT10/TE11", destination)]
complex_ids <- stations$destination
stations <- unique(stations, by = "destination")
station_codes <- fread(file.path(raw, "operational-codes.csv"), encoding = "UTF-8")
# Expand line codes while retaining each interchange as one physical paid-entry complex.
station_rows_codes <- stations[, .(code = unlist(strsplit(destination, "/", fixed = TRUE))),
                              by = destination]
station_rows_codes[, line := sub("[0-9].*$", "", code)]
station_rows_codes[line == "PTC", line := "PE"]
# PE/PW are two loops of the same Punggol LRT service; SE/SW likewise for Sengkang.
station_rows_codes[line %in% c("PE", "PW"), line := "PLRT"]
station_rows_codes[line %in% c("SE", "SW", "STC"), line := "SLRT"]
station_codes_match <- station_rows_codes[, .(
  service_count = uniqueN(line),
  mrt = as.integer(any(!line %in% c("BP", "PLRT", "SLRT")))
), by = destination]
stations <- merge(stations, station_codes_match, by = "destination", sort = FALSE)

station_shape <- st_transform(read_shape_zip("stations.zip"), project_crs)
shape_codes <- strsplit(toupper(trimws(station_shape$STN_NAM)), "\\s*/\\s*")
footprints <- split(rep(seq_along(shape_codes), lengths(shape_codes)), unlist(shape_codes))
# A station with multiple footprints uses the centroid of the combined footprint.
official_point <- function(codes, label, fallback_lon, fallback_lat) {
  codes <- strsplit(codes, "/", fixed = TRUE)[[1]]
  official <- intersect(codes, names(footprints))
  if (length(official)) {
    polygons <- st_geometry(station_shape)[unique(unlist(footprints[official]))]
    return(c(st_coordinates(st_centroid(st_union(st_make_valid(polygons))))[1, ],
             official = 1))
  }
  by_name <- station_codes[tolower(station_name) == tolower(label)]
  if (nrow(by_name)) {
    fallback_lon <- mean(by_name$lon)
    fallback_lat <- mean(by_name$lat)
  }
  point <- st_sfc(st_point(c(fallback_lon, fallback_lat)), crs = 4326)
  c(st_coordinates(st_transform(point, project_crs))[1, ], official = 0)
}
xy <- t(mapply(official_point, stations$destination, stations$station,
               stations$mirror_lon, stations$mirror_lat))
stations[, `:=`(x = xy[, "X"], y = xy[, "Y"], official_footprint = as.logical(xy[, "official"]))]
stations[, c("mirror_lon", "mirror_lat") := NULL]
stations[, `:=`(status = "Existing", corridor = "", planned_codes = "")]

# Convert the mirror's rounded station-by-hour day-type means to average daily tap-ins.
station_taps <- rbindlist(lapply(months, function(month_id) {
  item <- read_array(paste0(month_id, ".js"), paste0('MRT.registerMonth("', month_id, '",'), ");")
  days <- manifest[[match(month_id, vapply(manifest, `[[`, character(1), "month"))]]$days
  wd <- vapply(item[["in"]]$wd, function(x) sum(unlist(x)), numeric(1))
  we <- vapply(item[["in"]]$we, function(x) sum(unlist(x)), numeric(1))
  stopifnot(length(wd) == length(mirror_ids), length(we) == length(mirror_ids), all(wd >= 0), all(we >= 0))
  data.table(destination = complex_ids, month = month_id,
             weekday_daily = wd, weekend_daily = we, days = days$wd + days$we,
             tap_ins = (wd * days$wd + we * days$we) / (days$wd + days$we))
}))
station_taps <- station_taps[, .(weekday_daily = sum(weekday_daily), weekend_daily = sum(weekend_daily),
                                days = first(days), tap_ins = sum(tap_ins)),
                             by = .(destination, month)]
obs <- station_taps[, .(
  observed_train = weighted.mean(tap_ins[month %in% months[1:2]], days[month %in% months[1:2]]),
  observed_test = tap_ins[month == months[3]]
), by = destination]
stations <- merge(stations, obs, by = "destination", all.x = TRUE, sort = FALSE)
# Teck Lee and Ten Mile Junction are examples where historical/inactive names may exist
# in geometry layers. The calibration universe is active complexes with observed tap-ins.
inactive <- stations[observed_train <= 0 | observed_test <= 0, .(destination, station)]
stations <- stations[observed_train > 0 & observed_test > 0]
setorder(stations, destination)
stopifnot(!anyDuplicated(stations$destination), !anyDuplicated(tolower(stations$station)))

future <- fread(file.path(raw, "future-stations.csv"), encoding = "UTF-8")
future <- future[grepl("^(JE|JS|JW|CR|CP)[0-9]", station_code) & station_code != "CR1"]
future[, corridor := fifelse(grepl("^J", station_code), "JRL", "CRL")]
future[, name_key := tolower(gsub("[^a-z0-9]", "", tolower(station_name)))]
stations[, name_key := tolower(gsub("[^a-z0-9]", "", tolower(station)))]
future_xy <- st_coordinates(st_transform(st_as_sf(future, coords = c("lon", "lat"), crs = 4326),
                                         project_crs))
future[, `:=`(x = future_xy[, 1], y = future_xy[, 2])]
future_groups <- future[, .(
  destination = first(station_code), station = first(station_name),
  x = mean(x), y = mean(y), planned_codes = paste(station_code, collapse = "/"),
  service_count = uniqueN(corridor), corridor = paste(sort(unique(corridor)), collapse = "/")
), by = name_key]
stopifnot(nrow(future_groups) < nrow(future))
existing_upgrades <- merge(stations, future_groups, by = "name_key", suffixes = c("", "_future"))
distance_check <- with(existing_upgrades, sqrt((x - x_future)^2 + (y - y_future)^2))
stopifnot(length(distance_check) >= 10L, all(distance_check < 650))
new_stations <- future_groups[!name_key %in% stations$name_key]
# Catch unmerged adjacent interchange entries caused by a spelling or naming change.
nearest_existing <- apply(as.matrix(dist(rbind(new_stations[, .(x, y)], stations[, .(x, y)])))[
  seq_len(nrow(new_stations)), nrow(new_stations) + seq_len(nrow(stations)), drop = FALSE], 1, min)
if (any(nearest_existing < 160)) stop("Review nearby future station match: ",
                                      paste(new_stations$station[nearest_existing < 160], collapse = ", "))
new_stations[, `:=`(status = "Proposed", mrt = 1L, official_footprint = FALSE,
                    observed_train = NA_real_, observed_test = NA_real_)]

areas <- st_read(file.path(raw, "planning-areas.geojson"), quiet = TRUE) |>
  st_transform(project_crs) |> st_make_valid()
land <- st_sf(geometry = st_simplify(st_union(st_geometry(areas)), dTolerance = 30))
population <- terra::rast(file.path(raw, "population.tif"))
pixels <- terra::as.data.frame(population, xy = TRUE, na.rm = TRUE)
names(pixels)[3] <- "population"
pixels <- pixels[pixels$population > 0, ]
pixel_sf <- st_transform(st_as_sf(pixels, coords = c("x", "y"), crs = 4326), project_crs)
pixels <- as.data.table(cbind(st_coordinates(pixel_sf), population = pixels$population))
# Assign each raster cell by its centre to one 500 m square without interpolating counts.
origins <- pixels[, .(x = weighted.mean(X, population), y = weighted.mean(Y, population),
                      market_population = sum(population), pixel_count = .N),
                  by = .(east = floor(X / 500), north = floor(Y / 500))]
origins[, origin := sprintf("SG_%03d_%03d", east, north)]
setorder(origins, origin)
stopifnot(isTRUE(all.equal(sum(origins$market_population), sum(pixels$population))))
points <- function(d) st_as_sf(d, coords = c("x", "y"), crs = project_crs, remove = FALSE)
origin_sf <- points(origins)
area_index <- st_intersects(origin_sf, areas)
origins[, planning_area := vapply(area_index, function(x) {
  if (length(x)) areas$PLN_AREA_N[x[1L]] else "Outside 2019 polygons"
}, character(1))]

bus <- st_transform(read_shape_zip("bus-stops.zip"), project_crs)
bus <- bus[bus$STATUS == "OP" & !is.na(bus$BUS_STOP_N) & nzchar(bus$BUS_STOP_N), ]
bus <- bus[!duplicated(bus$BUS_STOP_N), ]
hdb <- st_read(file.path(raw, "hdb.geojson"), quiet = TRUE) |> st_transform(project_crs)
hdb <- hdb[!st_is_empty(hdb), ]
hdb <- st_point_on_surface(st_geometry(hdb))
raffles <- stations[station == "Raffles Place"]
stopifnot(nrow(raffles) == 1)
add_attractors <- function(d) {
  d <- copy(d)
  d[, bus_500m := lengths(st_is_within_distance(points(d), bus, dist = 500))]
  d[, hdb_800m := lengths(st_is_within_distance(points(d), hdb, dist = 800))]
  d[, cbd_km := sqrt((x - raffles$x)^2 + (y - raffles$y)^2) / 1000]
  d[]
}
stations <- add_attractors(stations)
new_stations <- add_attractors(new_stations)
future_groups[, reused := name_key %in% stations$name_key]
future_groups[, destination_id := fifelse(reused, stations$destination[match(name_key, stations$name_key)],
                                          destination)]
fwrite(future_groups[, .(destination_id, station, corridor, planned_codes, reused, x, y)],
       file.path(results, "planned_station_crosswalk.csv"))
fwrite(station_taps, file.path(derived, "station_month_tap_ins.csv"))
fwrite(origins, file.path(results, "origins.csv"))
fwrite(inactive, file.path(results, "inactive_station_rows.csv"))
