## ---- h7-packages
library(sf)
library(dplyr)
library(readr)
library(tidyr)
library(SpatialAcc)
library(tmap)
library(ggplot2)
tmap_mode("plot")
theme_set(theme_minimal(base_size = 12))
dir.create("handson7/results", recursive = TRUE, showWarnings = FALSE)

## ---- h7-inputs
manifest <- read_csv("handson7/data/input-checksums.csv", show_col_types = FALSE)
stopifnot(identical(unname(tools::md5sum(file.path("handson7/data/raw", manifest$file))),
                    manifest$md5))
read_layer <- function(name) {
  st_read(file.path("handson7/data/raw/geospatial", paste0(name, ".shp")), quiet = TRUE) |>
    st_transform(3414) |>
    st_make_valid()
}
mpsz <- read_layer("MP14_SUBZONE_NO_SEA_PL")
hexagons <- read_layer("hexagons") |> select(fid) |> mutate(demand = 100)
eldercare <- read_layer("ELDERCARE") |> select(fid, ADDRESSPOS) |> mutate(capacity = 100)
stopifnot(!anyDuplicated(hexagons$fid), !anyDuplicated(eldercare$fid),
          nrow(mpsz) == 323, nrow(hexagons) == 3125, nrow(eldercare) == 120,
          all(st_is_valid(mpsz)), all(st_is_valid(hexagons)),
          st_crs(mpsz)$epsg == 3414, st_crs(eldercare)$epsg == 3414)
input_summary <- tibble(dataset = c("Subzones", "Hexagons", "Eldercare centres"),
                        features = c(nrow(mpsz), nrow(hexagons), nrow(eldercare)),
                        CRS = "EPSG:3414 (metres)")
knitr::kable(input_summary)

## ---- h7-matrix
ODMatrix <- read_csv("handson7/data/raw/aspatial/OD_Matrix.csv", show_col_types = FALSE)
stopifnot(nrow(ODMatrix) == nrow(hexagons) * nrow(eldercare),
          !anyDuplicated(ODMatrix[c("origin_id", "destination_id")]),
          setequal(ODMatrix$origin_id, hexagons$fid),
          setequal(ODMatrix$destination_id, eldercare$fid),
          all(is.finite(ODMatrix$total_cost)), all(ODMatrix$total_cost > 0),
          max(abs(ODMatrix$total_cost - ODMatrix$entry_cost -
                    ODMatrix$network_cost - ODMatrix$exit_cost)) < 0.01)
wide <- ODMatrix |>
  select(origin_id, destination_id, total_cost) |>
  pivot_wider(names_from = destination_id, values_from = total_cost)
# Match keys explicitly: never assume the GIS records have pivot-table order.
wide <- wide[match(hexagons$fid, wide$origin_id), ]
distmat_km <- as.matrix(wide[, as.character(eldercare$fid)]) / 1000
rownames(distmat_km) <- as.character(hexagons$fid)
stopifnot(identical(rownames(distmat_km), as.character(hexagons$fid)),
          identical(colnames(distmat_km), as.character(eldercare$fid)),
          !anyNA(distmat_km), all(is.finite(distmat_km)))
knitr::kable(tibble(OD_pairs = nrow(ODMatrix), origins = nrow(distmat_km),
                   destinations = ncol(distmat_km),
                   minimum_km = min(distmat_km), maximum_km = max(distmat_km)), digits = 3)

## ---- h7-overview
tm_shape(mpsz) + tm_polygons(fill = "#f0eee8", col = "#d1cbc2", lwd = 0.5) +
  tm_shape(hexagons) + tm_borders(col = "#7195a4", lwd = 0.25) +
  tm_shape(eldercare) + tm_dots(fill = "#bd4734", col = "white", size = 0.25) +
  tm_title("Teaching study area and eldercare locations") +
  tm_scalebar() + tm_compass(type = "arrow", size = 1.5) +
  tm_layout(frame = FALSE)

## ---- h7-hansen
hexagons$Hansen <- ac(hexagons$demand, eldercare$capacity, distmat_km,
                      power = 2, family = "Hansen")
# With equal supply, the package agrees with explicit exponential potential.
hansen_manual <- as.vector(exp(-2 * distmat_km) %*% eldercare$capacity)
stopifnot(isTRUE(all.equal(hexagons$Hansen, hansen_manual)))
access_map <- function(field, title) {
  value <- hexagons[[field]]
  has_zero <- any(value == 0)
  breaks <- unique(as.numeric(quantile(value[value > 0],
    probs = seq(0, 1, length.out = if (has_zero) 10 else 11))))
  lower <- formatC(head(breaks, -1), format = "e", digits = 2)
  upper <- formatC(tail(breaks, -1), format = "e", digits = 2)
  labels <- paste(lower, upper, sep = " to ")
  classes <- as.character(cut(value, breaks, labels = labels, include.lowest = TRUE))
  if (has_zero) {
    classes[value == 0] <- "0 (outside catchment)"
    labels <- c("0 (outside catchment)", labels)
  }
  plot_data <- hexagons
  plot_data$map_class <- factor(classes, levels = labels, ordered = TRUE)
  stopifnot(!anyNA(plot_data$map_class))
  palette <- grDevices::colorRampPalette(c("#f7fbff", "#08306b"))(length(labels))
  tm_shape(plot_data) +
    tm_polygons(fill = "map_class",
      fill.scale = tm_scale_categorical(values = palette),
      fill.legend = tm_legend(title = "Accessibility", text.size = 0.65),
      col = "#ffffff", lwd = 0.1) +
    tm_shape(eldercare) + tm_dots(fill = "#bd4734", col = "white", size = 0.18) +
    tm_title(title) + tm_compass(type = "arrow", size = 1.5) + tm_scalebar() +
    tm_layout(frame = FALSE, legend.outside = TRUE)
}
access_map("Hansen", "Eldercare accessibility: Hansen")

## ---- h7-regions
# Assign each cell once, to the planning region with the largest overlap.
# Dissolving subzones first avoids duplicate cells from boundary intersections.
regions <- mpsz |> select(REGION_N) |> group_by(REGION_N) |> summarise()
regional_hex <- st_join(hexagons, regions, largest = TRUE)
stopifnot(nrow(regional_hex) == nrow(hexagons))
unmatched <- sum(is.na(regional_hex$REGION_N))
regional_hex$REGION_N[is.na(regional_hex$REGION_N)] <- "OUTSIDE BOUNDARY"
region_plot <- function(data, field, logarithm = FALSE) {
  data$value <- data[[field]]
  if (logarithm) data$value <- log(data$value)
  ggplot(st_drop_geometry(data), aes(x = REGION_N, y = value)) +
    geom_boxplot(fill = "#b9d9e8", outlier.alpha = 0.3) +
    stat_summary(fun = mean, geom = "point", colour = "#bd4734", size = 2) +
    labs(x = NULL, y = if (logarithm) paste("Natural log of", field) else field,
         title = paste(field, "accessibility by planning region"),
         caption = "Red point = mean; each hexagon contributes once. Equal cell weights.") +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))
}
region_plot(regional_hex, "Hansen", logarithm = TRUE)

## ---- h7-kd2sfca
hexagons$KD2SFCA <- ac(hexagons$demand, eldercare$capacity, distmat_km,
                       d0 = 5, power = 2, family = "KD2SFCA")
# Independent two-step calculation: competing demand then origin access.
weights <- exp(-(distmat_km^2)) * (distmat_km <= 5)
weighted_demand <- colSums(weights * hexagons$demand)
stopifnot(all(weighted_demand > 0))
ratios <- eldercare$capacity / weighted_demand
kd_manual <- as.vector(weights %*% ratios)
stopifnot(isTRUE(all.equal(hexagons$KD2SFCA, kd_manual)),
          abs(sum(hexagons$demand * hexagons$KD2SFCA) - sum(eldercare$capacity)) < 1e-6)
access_map("KD2SFCA", "Eldercare accessibility: KD2SFCA (5 km)")

## ---- h7-kd-region
regional_hex$KD2SFCA <- hexagons$KD2SFCA
region_plot(regional_hex, "KD2SFCA")

## ---- h7-sam
hexagons$SAM <- ac(hexagons$demand, eldercare$capacity, distmat_km,
                   power = 2, family = "SAM")
sam_manual <- rowSums(sweep(1 / distmat_km^2, 2, eldercare$capacity, `*`)) /
  hexagons$demand
stopifnot(isTRUE(all.equal(hexagons$SAM, unname(sam_manual))),
          identical(hexagons$SAM, unname(ac(hexagons$demand, eldercare$capacity,
                                     distmat_km, d0 = 5, power = 2, family = "SAM"))))
access_map("SAM", "Eldercare accessibility: SAM")

## ---- h7-sam-region
regional_hex$SAM <- hexagons$SAM
region_plot(regional_hex, "SAM")

## ---- h7-comparison
methods <- c("Hansen", "KD2SFCA", "SAM")
scores <- st_drop_geometry(regional_hex) |> select(fid, REGION_N, all_of(methods))
stopifnot(all(vapply(scores[methods], function(x) all(is.finite(x) & x >= 0), logical(1))))
regional_summary <- scores |>
  pivot_longer(all_of(methods), names_to = "method", values_to = "accessibility") |>
  group_by(REGION_N, method) |>
  summarise(cells = n(), mean = mean(accessibility), median = median(accessibility),
            minimum = min(accessibility), maximum = max(accessibility), .groups = "drop")
knitr::kable(regional_summary |> select(REGION_N, method, cells, mean, median) |>
  mutate(across(c(mean, median), ~ formatC(.x, format = "g", digits = 4))))
rank_correlations <- cor(scores[methods], method = "spearman")
knitr::kable(rank_correlations, digits = 3, caption = "Spearman rank correlations")
best_regions <- regional_summary |> group_by(method) |> slice_max(median, n = 1, with_ties = FALSE)
worst_regions <- regional_summary |> group_by(method) |> slice_min(median, n = 1, with_ties = FALSE)

## ---- h7-sensitivity
sensitivity <- bind_rows(lapply(c(3, 5, 8), function(cutoff) {
  value <- ac(hexagons$demand, eldercare$capacity, distmat_km,
              d0 = cutoff, power = 2, family = "KD2SFCA")
  tibble(catchment_km = cutoff, zero_access_cells = sum(value == 0),
         median = median(value), max = max(value),
         rank_correlation_to_5km = cor(value, hexagons$KD2SFCA, method = "spearman"))
}))
knitr::kable(sensitivity |>
  mutate(across(c(median, max, rank_correlation_to_5km),
                ~ formatC(.x, format = "g", digits = 4))))
hansen_slow <- ac(hexagons$demand, eldercare$capacity, distmat_km,
                   power = 0.5, family = "Hansen")
hansen_rank_cor <- cor(hansen_slow, hexagons$Hansen, method = "spearman")
write_csv(scores, "handson7/results/accessibility-by-hexagon.csv")
write_csv(regional_summary, "handson7/results/regional-summary.csv")
write_csv(sensitivity, "handson7/results/catchment-sensitivity.csv")
write_csv(as.data.frame(rank_correlations) |> tibble::rownames_to_column("method"),
          "handson7/results/rank-correlations.csv")

## ---- h7-session
sessionInfo()
