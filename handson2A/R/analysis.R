# This file is read by knitr::read_chunk; each labelled section runs in the page.
# For an interactive run, source prepare.R before sourcing this script.
## ----national-map
tmap_mode("plot")
tm_shape(subzones) + tm_polygons(fill = "#eef3f8", col = "white", lwd = 0.2) +
  tm_shape(childcare[inside, ]) + tm_dots(col = "#465d91", size = 0.025) +
  tm_title("Childcare service records in Singapore") +
  tm_scalebar(position = tm_pos_in("left", "bottom")) +
  tm_compass(position = tm_pos_in("right", "top"))

## ----nearest-neighbour
set.seed(20260919)
ce_z <- clarkevans.test(childcare_ppp, correction = "none",
                       alternative = "clustered", method = "asymptotic")
ce_mc <- clarkevans.test(childcare_ppp, correction = "none",
                        alternative = "clustered", method = "MonteCarlo", nsim = 999)
ce_unique <- clarkevans.test(childcare_unique, correction = "none",
                            alternative = "clustered", method = "MonteCarlo", nsim = 999)
ce_table <- data.frame(
  analysis = c("Service records: normal approximation", "Service records: Monte Carlo",
               "Distinct locations: Monte Carlo"),
  R = c(unname(ce_z$statistic), unname(ce_mc$statistic), unname(ce_unique$statistic)),
  p = c(ce_z$p.value, ce_mc$p.value, ce_unique$p.value)
)
ce_table

## ----bandwidths
childcare_km <- spatstat.geom::rescale(childcare_ppp, 1000, "km")
bw_diggle <- bw.diggle(childcare_km)
bw_ppl <- bw.ppl(childcare_km)
bw_cvl <- bw.CvL(childcare_km)
bw_scott <- bw.scott(childcare_km)
bandwidth_table <- data.frame(
  method = c("Diggle", "Likelihood cross-validation", "Cronie-van Lieshout", "Scott x", "Scott y"),
  bandwidth_km = c(bw_diggle, bw_ppl, bw_cvl, bw_scott)
)
bandwidth_table

## ----kde-units
# 100 m square cells resolve the smoothing surface more clearly than a 128x128 default grid.
kde_m <- density(childcare_ppp, sigma = as.numeric(bw_diggle) * 1000,
                 edge = TRUE, kernel = "gaussian", eps = 100)
kde_km <- density(childcare_km, sigma = as.numeric(bw_diggle),
                  edge = TRUE, kernel = "gaussian", eps = 0.1)
stopifnot(isTRUE(all.equal(kde_m$v * 1e6, kde_km$v, tolerance = 1e-6)))
data.frame(units = c("records / m2", "records / km2"),
           maximum = c(max(kde_m$v, na.rm = TRUE), max(kde_km$v, na.rm = TRUE)),
           integrated_records = c(integral.im(kde_m), integral.im(kde_km)))

## ----bandwidth-comparison
kde_ppl <- density(childcare_km, sigma = as.numeric(bw_ppl), edge = TRUE, eps = 0.1)
plot_surfaces <- function(surfaces, titles, ncol = 2) {
  old <- par(mfrow = c(ceiling(length(surfaces) / ncol), ncol), mar = c(2, 2, 3, 3))
  on.exit(par(old))
  common_range <- range(unlist(lapply(surfaces, function(x) x$v)), na.rm = TRUE)
  for (i in seq_along(surfaces)) {
    plot(surfaces[[i]], main = titles[i], zlim = common_range,
         col = hcl.colors(100, "Viridis"), ribargs = list(las = 1))
  }
}
plot_surfaces(list(kde_km, kde_ppl), c("Diggle bandwidth", "Likelihood bandwidth"))

## ----kernel-comparison
kernels <- c("gaussian", "epanechnikov", "quartic", "disc")
kernel_maps <- lapply(kernels, function(k) density(childcare_km,
  sigma = as.numeric(bw_diggle), kernel = k, edge = TRUE, eps = 0.1))
plot_surfaces(kernel_maps, tools::toTitleCase(kernels))

## ----adaptive-kde
kde_fixed <- density(childcare_km, sigma = 0.6, edge = TRUE, eps = 0.1)
kde_adaptive <- adaptive.density(childcare_km, method = "kernel",
                                 h0 = as.numeric(bw_ppl), eps = 0.1)
plot_surfaces(list(kde_fixed, kde_adaptive), c("Fixed: 600 m", "Adaptive kernel"))

## ----cartographic-kde
# Keep the raster coordinates in metres, but convert its VALUES to records/km2.
kde_raster <- rast(kde_m) * 1e6
crs(kde_raster) <- "EPSG:3414"
names(kde_raster) <- "records_km2"
stopifnot(all(abs(as.vector(ext(kde_raster)) -
                    c(kde_m$xrange, kde_m$yrange)) < 1e-5))
tm_shape(kde_raster) +
  tm_raster(col = "records_km2", col.scale = tm_scale_continuous(values = "viridis"),
            col.legend = tm_legend(title = "Records per km²")) +
  tm_shape(subzones) + tm_borders(col = "white", lwd = 0.2) +
  tm_title("Childcare intensity: Diggle bandwidth") +
  tm_compass(position = tm_pos_in("right", "top")) +
  tm_scalebar(position = tm_pos_in("left", "bottom")) +
  tm_layout(frame = FALSE)

## ----area-pattern-map
old <- par(mfrow = c(2, 2), mar = c(2, 2, 3, 1))
for (nm in area_names) plot(spatstat.geom::rescale(area_patterns[[nm]], 1000, "km"),
                           main = tools::toTitleCase(tolower(nm)), pch = 16, cex = 0.5)
par(old)

## ----area-tests
set.seed(20260920)
area_ce <- lapply(area_patterns[c("CHOA CHU KANG", "TAMPINES")], function(x)
  clarkevans.test(x, correction = "none", alternative = "two.sided",
                 method = "MonteCarlo", nsim = 999))
data.frame(area = names(area_ce), R = vapply(area_ce, function(x) unname(x$statistic), numeric(1)),
           p = vapply(area_ce, function(x) x$p.value, numeric(1)))

## ----area-kde
area_km <- lapply(area_patterns, spatstat.geom::rescale, s = 1000, unitname = "km")
area_bw <- vapply(area_km, function(x) as.numeric(bw.diggle(x)), numeric(1))
area_kde <- Map(function(x, b) density(x, sigma = b, edge = TRUE, eps = 0.05), area_km, area_bw)
data.frame(area = area_names, bandwidth_m = 1000 * area_bw)
plot_surfaces(area_kde, tools::toTitleCase(tolower(area_names)))

## ----interactive-points
tmap_mode("view")
interactive_points <- tm_shape(subzones) + tm_borders(col = "#6f82aa") +
  tm_shape(childcare[inside, ]) +
  tm_dots(col = "#465d91", popup = tm_popup(
    vars = c("Centre" = "NAME", "Address" = "ADDRESSSTREETNAME")))
print(interactive_points)
tmap_mode("plot")
