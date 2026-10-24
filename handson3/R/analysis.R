# Run from the Quarto project root, after R/download-data.R.
# Chapter 6: Spatio-Temporal Point Patterns Analysis.

## ---- h3-packages
library(sf)
library(spatstat.geom)
library(spatstat.explore)
library(sparr)
library(stpp)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(tmap)
library(ggplot2)
library(plotly)
library(knitr)
tmap_mode("plot")
options(scipen = 5)
theme_set(theme_minimal(base_size = 12) +
  theme(plot.background = element_rect(fill = "#faf7f0", colour = NA),
        panel.grid.minor = element_blank(),
        plot.title = element_text(colour = "#8f3f2d", face = "bold")))

## ---- h3-cache
# Cache expensive numerical calculations; input hashes, package versions and
# every numerical setting form the key. An absent or changed key recomputes.
dir.create("handson3/data/derived", recursive = TRUE, showWarnings = FALSE)
input_files <- c("forestfires.csv", "Kepulauan_Bangka_Belitung.shp",
                 "Kepulauan_Bangka_Belitung.dbf", "Kepulauan_Bangka_Belitung.prj")
input_md5 <- tools::md5sum(file.path("handson3/data/raw", input_files))
cache_compute <- function(name, settings, calculation) {
  path <- file.path("handson3/data/derived", paste0(name, ".rds"))
  key <- list(inputs = input_md5, settings = settings,
    versions = vapply(c("R", "sf", "sparr", "spatstat.geom", "spatstat.explore", "stpp"),
      function(p) if (p == "R") as.character(getRversion()) else
        as.character(packageVersion(p)), character(1)))
  if (file.exists(path)) {
    previous <- readRDS(path)
    if (identical(previous$key, key)) return(previous$result)
  }
  result <- force(calculation)
  saveRDS(list(key = key, result = result), path)
  result
}

## ---- h3-data
kbb <- st_read("handson3/data/raw/Kepulauan_Bangka_Belitung.shp", quiet = TRUE)
# Drop the redundant zero-valued Z coordinate before union and projection.
kbb_sf <- kbb |> st_zm(drop = TRUE, what = "ZM") |>
  st_union() |> st_transform(32748)
kbb_owin <- as.owin(kbb_sf)
fire <- read_csv("handson3/data/raw/forestfires.csv", show_col_types = FALSE) |>
  mutate(DayofYear = as.integer(yday(acq_date)),
         Month_num = as.integer(month(acq_date)),
         Month_fac = factor(Month_num, levels = 1:12, labels = month.name))
stopifnot(nrow(fire) == 741L, all(year(fire$acq_date) == 2023),
  !anyNA(fire[c("longitude", "latitude", "acq_date", "DayofYear")]),
  all(is.finite(fire$longitude)), all(is.finite(fire$latitude)),
  all(fire$instrument == "MODIS"), all(st_is_valid(st_zm(kbb))),
  st_crs(kbb_sf)$epsg == 32748)
fire_sf <- fire |> st_as_sf(coords = c("longitude", "latitude"), crs = 4326,
                          remove = FALSE) |> st_transform(32748)
xy <- st_coordinates(fire_sf)
inside <- inside.owin(xy[, 1], xy[, 2], kbb_owin)
stopifnot(all(inside))
audit <- tibble(Boundary_features = nrow(kbb), MODIS_detections = nrow(fire),
  Outside_window = sum(!inside), First_date = min(fire$acq_date),
  Last_date = max(fire$acq_date), Area_km2 = area.owin(kbb_owin) / 1e6)
kable(audit, digits = 2, caption = "Actual input data and study window")
monthly_counts <- fire |> count(Month_num, Month_fac, name = "detections") |>
  arrange(Month_num)
kable(monthly_counts, caption = "Monthly MODIS detection counts")

## ---- h3-overall
tm_shape(kbb_sf) +
  tm_polygons(fill = "#f4e8df", col = "#71675e", lwd = 0.5) +
  tm_shape(fire_sf) + tm_dots(fill = "#9a432e", col = "#9a432e", size = 0.12) +
  tm_layout(bg.color = "#faf7f0", frame = FALSE) +
  tm_title("MODIS detections · Bangka study area, 2023")

## ---- h3-monthly-map
tm_shape(kbb_sf) +
  tm_polygons(fill = "#f4e8df", col = "#71675e", lwd = 0.4) +
  tm_shape(fire_sf) + tm_dots(fill = "#9a432e", col = "#9a432e", size = 0.5) +
  tm_facets(by = "Month_fac", ncol = 4, free.coords = FALSE, drop.units = TRUE) +
  tm_layout(bg.color = "#faf7f0", frame = FALSE)

## ---- h3-monthly-chart
ggplot(monthly_counts, aes(Month_fac, detections)) +
  geom_col(fill = "#9a432e", width = 0.7) +
  geom_text(aes(label = detections), vjust = -0.3, size = 3.5) +
  scale_x_discrete(labels = month.abb) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Seasonal distribution of satellite detections",
       x = NULL, y = "MODIS detections")

## ---- h3-ppp
fire_month_ppp <- fire_sf |> select(Month_num) |> as.ppp()
fire_month_owin <- fire_month_ppp[kbb_owin]
fire_yday_ppp <- fire_sf |> select(DayofYear) |> as.ppp()
fire_yday_owin <- fire_yday_ppp[kbb_owin]
stopifnot(npoints(fire_month_owin) == nrow(fire),
  npoints(fire_yday_owin) == nrow(fire),
  !any(duplicated(fire_month_owin)), !any(duplicated(fire_yday_owin)))
summary(fire_month_owin)
summary(fire_yday_owin)

## ---- h3-monthly-kde
st_kde <- cache_compute("monthly-kde", list(sres = 128, tlim = c(1, 12),
  bandwidth = "defaults", edge = "uniform"),
  spattemp.density(fire_month_owin, sres = 128, verbose = FALSE))
summary(st_kde)

## ---- h3-monthly-kde-map
palette_fire <- colorRampPalette(c("#fffaf2", "#f4d4a0", "#d7834a", "#8f3f2d", "#4b241c"))(64)
draw_slices <- function(object, times, title_prefix) {
  previous_par <- par(no.readonly = TRUE)
  on.exit(par(previous_par))
  par(mfrow = c(2, 3), mar = c(2.4, 2.6, 2.6, 3.1), cex.axis = 0.65, cex.main = 0.95)
  for (time in times) {
    plot(object, tselect = time, override.par = FALSE, fix.range = TRUE,
         sleep = 0, col = palette_fire, main = paste(title_prefix, time))
  }
}
draw_slices(st_kde, 7:12, "Month")

## ---- h3-monthly-interactive
# The raster values are joint densities. Multiply by n and 10^6 to display
# estimated detection intensity per square kilometre per month.
upper <- max(vapply(st_kde$z, function(im) max(im$v, na.rm = TRUE), numeric(1))) *
  nrow(fire) * 1e6
kde_widget <- plot_ly()
for (i in 1:12) {
  im <- st_kde$z[[i]]
  kde_widget <- add_trace(kde_widget, x = im$xcol / 1000, y = im$yrow / 1000,
    z = im$v * nrow(fire) * 1e6, type = "heatmap", visible = i == 1,
    colors = c("#fffaf2", "#f4d4a0", "#d7834a", "#8f3f2d", "#4b241c"),
    zmin = 0, zmax = upper,
    # Reserve a full colour bar for the one visible month. Plotly R otherwise
    # stacks a short bar for each trace, including the eleven hidden months.
    colorbar = list(len = 0.8, y = 0.5, yanchor = "middle",
                    title = list(text = "Detections/km²/month", side = "right")),
    hovertemplate = "Easting: %{x:.1f} km<br>Northing: %{y:.1f} km<br>Intensity: %{z:.4f}<extra></extra>")
}
kde_widget <- layout(kde_widget,
  title = "Monthly density · slide through 2023",
  xaxis = list(title = "UTM easting (km)"),
  yaxis = list(title = "UTM northing (km)", scaleanchor = "x"),
  sliders = list(list(active = 0, currentvalue = list(prefix = "Month: "),
    steps = lapply(1:12, function(i) list(method = "restyle",
      args = list("visible", seq_len(12) == i), label = month.abb[i])))),
  margin = list(l = 80, r = 110, b = 100, t = 65),
  paper_bgcolor = "#faf7f0", plot_bgcolor = "#faf7f0")
config(kde_widget, displaylogo = FALSE, responsive = TRUE)

## ---- h3-daily-kde
kde_day_default <- cache_compute("daily-default", list(sres = 128,
  bandwidth = "defaults", tlim = range(fire$DayofYear), edge = "uniform"),
  spattemp.density(fire_yday_owin, sres = 128, verbose = FALSE))
summary(kde_day_default)
selected_days <- as.integer(yday(as.Date(paste0("2023-", sprintf("%02d", 7:12), "-15"))))

## ---- h3-daily-default-map
draw_slices(kde_day_default, selected_days, "Day")

## ---- h3-bootstrap
set.seed(1234)
boot_bw <- cache_compute("bootstrap-bandwidth", list(seed = 1234, sres = 64,
  tres = 64, start = "defaults", edge = "uniform", tlim = range(fire$DayofYear)),
  BOOT.spattemp(fire_yday_owin, sres = 64, tres = 64, verbose = FALSE))
stopifnot(length(boot_bw) == 2, all(is.finite(boot_bw)), all(boot_bw > 0))
kable(tibble(Spatial_metres = unname(boot_bw[1]), Temporal_days = unname(boot_bw[2])),
      digits = 4, caption = "Computed bootstrap bandwidths")

## ---- h3-daily-improved
# Retain both the chapter's rounded values and our actual optimised values.
kde_day_guide <- cache_compute("daily-guide", list(h = 9000, lambda = 19,
  sres = 128, tlim = range(fire$DayofYear), edge = "uniform"),
  spattemp.density(fire_yday_owin, h = 9000, lambda = 19, sres = 128, verbose = FALSE))
kde_day_boot <- cache_compute("daily-bootstrap", list(h = boot_bw[1], lambda = boot_bw[2],
  sres = 128, tlim = range(fire$DayofYear), edge = "uniform"),
  spattemp.density(fire_yday_owin, h = boot_bw[1], lambda = boot_bw[2],
                  sres = 128, verbose = FALSE))
summary(kde_day_guide)
bandwidths <- tibble(Method = c("Monthly default", "Daily default", "Chapter rounded", "Actual bootstrap"),
  Spatial_metres = c(st_kde$h, kde_day_default$h, kde_day_guide$h, kde_day_boot$h),
  Temporal_bandwidth = c(st_kde$lambda, kde_day_default$lambda, 19, kde_day_boot$lambda),
  Temporal_unit = c("months", "days", "days", "days"))
kable(bandwidths, digits = 4, caption = "Executed bandwidth specifications")

## ---- h3-daily-guide-map
draw_slices(kde_day_guide, selected_days, "Day")

## ---- h3-daily-bootstrap-map
draw_slices(kde_day_boot, selected_days, "Day")

## ---- h3-stpp
fire_df <- data.frame(x = xy[, 1], y = xy[, 2], t = fire$DayofYear)
fire_stpp <- as.3dpoints(fire_df)
stopifnot(inherits(fire_stpp, "stpp"), all(fire_df$t %% 1 == 0))
class(fire_stpp)
# STIKhat accepts one polygon ring, whereas the union contains two parts.
# The largest part contains every detection; the small offshore part has none.
parts <- st_cast(kbb_sf, "POLYGON")
main_island <- parts[which.max(st_area(parts))]
stik_boundary <- st_simplify(main_island, dTolerance = 100, preserveTopology = TRUE)
stopifnot(all(lengths(st_covered_by(fire_sf, main_island)) > 0),
  length(stik_boundary[[1]]) == 1L,
  all(lengths(st_covered_by(fire_sf, stik_boundary)) > 0),
  abs(as.numeric(st_area(stik_boundary) / st_area(main_island)) - 1) < 0.001)
s_region_matrix <- st_coordinates(stik_boundary)[, 1:2]
boundary_audit <- tibble(Original_vertices = nrow(st_coordinates(main_island)),
  Simplified_vertices = nrow(s_region_matrix), Tolerance_metres = 100,
  Area_change_percent = 100 * (as.numeric(st_area(stik_boundary) / st_area(main_island)) - 1),
  Retained_detections = nrow(fire))
kable(boundary_audit, digits = 4, caption = "STIK boundary adaptation and checks")

## ---- h3-space-time-map
ggplot() + geom_sf(data = st_as_sf(kbb_sf), fill = "#f4e8df", colour = "#71675e", linewidth = 0.2) +
  geom_sf(data = fire_sf, aes(colour = DayofYear), size = 1) +
  scale_colour_gradientn(colours = c("#a1b39a", "#dda17e", "#8f3f2d"), name = "Day of year") +
  labs(title = "Detection locations coloured by time")

## ---- h3-stik
# Explicitly restrict the observations to the chapter's time window.
# Otherwise STIKhat silently filters them internally.
stik_df <- fire_df |> filter(t >= 150, t <= 250) |> arrange(t, x, y)
stik_points <- as.3dpoints(stik_df)
set.seed(1234)
kbb_stik <- cache_compute("stik", list(seed = 1234, t.region = c(150, 250),
  dist = seq(1000, 20000, 1000), times = 1:25, infectious = TRUE,
  correction = "isotropic", simplify_metres = 100, single_main_ring = TRUE),
  STIKhat(stik_points, s.region = s_region_matrix, t.region = c(150, 250),
    dist = seq(1000, 20000, 1000), times = 1:25, infectious = TRUE))
stopifnot(all(is.finite(kbb_stik$Khat)), all(is.finite(kbb_stik$Ktheo)))
stik_grid <- expand_grid(distance_metres = kbb_stik$dist, lag_days = kbb_stik$times) |>
  mutate(Khat = as.vector(t(kbb_stik$Khat)),
    Ktheo = as.vector(t(kbb_stik$Ktheo)), excess = Khat - Ktheo,
    ratio = Khat / Ktheo)
kable(stik_grid |> filter(distance_metres %in% c(5000, 10000, 20000), lag_days %in% c(7, 14, 25)),
      digits = 3, caption = "Selected K estimates and their Poisson reference")

## ---- h3-stik-map
# Render the same centred quantity as plotK(..., L = TRUE) without plotK's
# graphics-device replacement, which interferes with Quarto figure capture.
ggplot(stik_grid, aes(distance_metres / 1000, lag_days)) +
  geom_tile(aes(fill = excess / 1e6)) +
  geom_contour(aes(z = excess / 1e6), colour = "#433b35", bins = 12, linewidth = 0.25) +
  scale_fill_gradient2(low = "#607560", mid = "#fffaf2", high = "#9a432e",
    midpoint = 0, name = "K − πu²v\n(km² · day)") +
  labs(title = "Space-time K departure from the Poisson reference",
       subtitle = "Days 150–250 · future-event form · isotropic edge correction",
       x = "Spatial distance (km)", y = "Temporal lag (days)")

## ---- h3-permutation
# A supplementary test of association, preserving all locations and all dates.
# This does not require a homogeneous spatial or temporal marginal distribution.
spatial_pairs <- which(upper.tri(as.matrix(dist(xy))) &
                        as.matrix(dist(xy)) <= 10000, arr.ind = TRUE)
count_close <- function(times) sum(abs(times[spatial_pairs[, 1]] -
                                     times[spatial_pairs[, 2]]) <= 14)
observed_pairs <- count_close(fire$DayofYear)
set.seed(1234)
permuted_pairs <- replicate(199, count_close(sample(fire$DayofYear)))
association_p <- (1 + sum(permuted_pairs >= observed_pairs)) / 200
association <- tibble(Spatial_radius_km = 10, Temporal_lag_days = 14,
  Observed_close_pairs = observed_pairs, Permutation_mean = mean(permuted_pairs),
  Permutations = 199, Upper_tail_p = association_p)
kable(association, digits = 3, caption = "Supplementary space–time association test")

## ---- h3-export
dir.create("handson3/results", recursive = TRUE, showWarnings = FALSE)
write_csv(audit, "handson3/results/data-audit.csv")
write_csv(monthly_counts, "handson3/results/monthly-counts.csv")
write_csv(bandwidths, "handson3/results/bandwidths.csv")
write_csv(boundary_audit, "handson3/results/stik-boundary-audit.csv")
write_csv(stik_grid, "handson3/results/stik-grid.csv")
write_csv(association, "handson3/results/space-time-association.csv")
write_csv(tibble(simulation = 1:199, close_pairs = permuted_pairs),
          "handson3/results/permutation-counts.csv")
density_summaries <- bind_rows(lapply(list(default = kde_day_default,
  chapter = kde_day_guide, bootstrap = kde_day_boot), function(object) {
    tibble(DayofYear = object$tgrid,
      Maximum_joint_density = vapply(object$z, function(im) max(im$v, na.rm = TRUE), numeric(1)),
      Temporal_density = approx(object$temporal.z$x, object$temporal.z$y,
                                 xout = object$tgrid, rule = 2)$y)
  }), .id = "method")
write_csv(density_summaries, "handson3/results/daily-density-summary.csv")
versions <- tibble(package = c("R", "sf", "spatstat.geom", "spatstat.explore", "sparr",
  "stpp", "tmap", "plotly"), version = c(as.character(getRversion()),
  vapply(c("sf", "spatstat.geom", "spatstat.explore", "sparr", "stpp", "tmap", "plotly"),
         function(p) as.character(packageVersion(p)), character(1))))
write_csv(versions, "handson3/results/package-versions.csv")
kable(versions, caption = "Reproduction environment")
