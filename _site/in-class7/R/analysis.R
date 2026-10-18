## ---- c7-packages
library(sf)
library(dplyr)
library(readr)
library(tidyr)
library(tmap)
library(ggplot2)
library(accessibility)
library(catchment)
tmap_mode("plot")
theme_set(theme_minimal(base_size = 12))
dir.create("in-class7/results", recursive = TRUE, showWarnings = FALSE)

## ---- c7-inputs
manifest <- read_csv("in-class7/data/input-checksums.csv", show_col_types = FALSE)
stopifnot(identical(unname(tools::md5sum(file.path("in-class7/data/raw", manifest$file))), manifest$md5))
read_layer <- function(name) {
  st_read(file.path("in-class7/data/raw/geospatial", paste0(name, ".shp")), quiet = TRUE) |>
    st_transform(3414) |> st_make_valid()
}
mpsz <- read_layer("MP14_SUBZONE_NO_SEA_PL")
hexagons <- read_layer("hexagons") |> select(fid) |> rename(id = fid) |> mutate(demand = 100)
eldercare <- read_layer("ELDERCARE") |> select(fid) |> mutate(capacity = 100)
ODMatrix <- read_csv("in-class7/data/raw/aspatial/OD_Matrix.csv", show_col_types = FALSE)
stopifnot(nrow(hexagons) == 3125, nrow(eldercare) == 120, nrow(mpsz) == 323,
  !anyDuplicated(hexagons$id), !anyDuplicated(eldercare$fid),
  nrow(ODMatrix) == nrow(hexagons) * nrow(eldercare),
  !anyDuplicated(ODMatrix[c("origin_id", "destination_id")]),
  setequal(ODMatrix$origin_id, hexagons$id), setequal(ODMatrix$destination_id, eldercare$fid),
  all(is.finite(ODMatrix$total_cost)), all(ODMatrix$total_cost > 0),
  all(st_is_valid(hexagons)), all(st_is_valid(mpsz)), st_crs(hexagons)$epsg == 3414)
knitr::kable(tibble(dataset = c("Subzones", "Demand hexagons", "Eldercare facilities", "OD pairs"),
  records = c(nrow(mpsz), nrow(hexagons), nrow(eldercare), nrow(ODMatrix))))

## ---- c7-matrix
ODMatrix_tidied <- ODMatrix |> transmute(from_id = origin_id, to_id = destination_id,
                                        dist = total_cost)
wide <- ODMatrix_tidied |> pivot_wider(names_from = to_id, values_from = dist)
wide <- wide[match(hexagons$id, wide$from_id), ]
distmat_km <- as.matrix(wide[, as.character(eldercare$fid)]) / 1000
dimnames(distmat_km) <- list(as.character(hexagons$id), as.character(eldercare$fid))
stopifnot(!anyNA(distmat_km), all(is.finite(distmat_km)),
  identical(rownames(distmat_km), as.character(hexagons$id)),
  identical(colnames(distmat_km), as.character(eldercare$fid)))
knitr::kable(tibble(origins = nrow(distmat_km), destinations = ncol(distmat_km),
                    minimum_km = min(distmat_km), maximum_km = max(distmat_km)), digits = 3)

## ---- c7-nearest
# Active accessibility uses opportunities at destinations: supply belongs to facilities.
facility_land_use <- st_drop_geometry(eldercare) |> transmute(id = fid, capacity)
dist2closest <- cost_to_closest(travel_matrix = ODMatrix_tidied,
  land_use_data = facility_land_use, opportunity = "capacity", travel_cost = "dist",
  n = 1, group_by = character(0), active = TRUE, fill_missing_ids = TRUE)
hex_acc <- hexagons |> left_join(dist2closest |> rename(nearest_m = dist), by = "id")
hex_acc$nearest_km <- hex_acc$nearest_m / 1000
stopifnot(nrow(hex_acc) == nrow(hexagons),
  isTRUE(all.equal(hex_acc$nearest_km, unname(apply(distmat_km, 1, min)))))
nearest_summary <- tibble(minimum_km = min(hex_acc$nearest_km),
  median_km = median(hex_acc$nearest_km), maximum_km = max(hex_acc$nearest_km),
  cells_beyond_5km = sum(hex_acc$nearest_km > 5))
knitr::kable(nearest_summary, digits = 3)
access_map <- function(field, title, distance = FALSE) {
  value <- hex_acc[[field]]
  positive <- value[value > 0]
  breaks <- unique(as.numeric(quantile(positive, seq(0, 1, length.out = 8))))
  labels <- paste(formatC(head(breaks, -1), format = "g", digits = 3),
                  formatC(tail(breaks, -1), format = "g", digits = 3), sep = " to ")
  classes <- as.character(cut(value, breaks, labels = labels, include.lowest = TRUE))
  if (any(value == 0)) {
    classes[value == 0] <- "0 (outside catchment)"
    labels <- c("0 (outside catchment)", labels)
  }
  plot_data <- hex_acc
  plot_data$map_class <- factor(classes, levels = labels, ordered = TRUE)
  stopifnot(!anyNA(plot_data$map_class))
  palette <- colorRampPalette(if (distance) c("#fff7ec", "#7f2704") else c("#f7fbff", "#08306b"))(length(labels))
  tm_shape(plot_data) + tm_polygons(fill = "map_class",
    fill.scale = tm_scale_categorical(values = palette),
    fill.legend = tm_legend(title = if (distance) "Distance (km)" else "Supply / demand", text.size = 0.7),
    col = "white", lwd = 0.1) +
    tm_shape(eldercare) + tm_dots(size = 0.17, fill = "#bd4734", col = "white") +
    tm_title(title) + tm_compass(type = "arrow", size = 1.5) + tm_scalebar() +
    tm_layout(frame = FALSE, legend.outside = TRUE)
}
access_map("nearest_km", "Distance to the nearest eldercare facility", distance = TRUE)

## ---- c7-2sfca
hex_acc$FCA2 <- catchment_ratio(consumers = hexagons$demand,
  providers = eldercare$capacity, cost = distmat_km, max_cost = 5)
access_map("FCA2", "2SFCA: 5 km catchment")

## ---- c7-e2sfca
step_weights <- list(c(5, .01), c(4, .05), c(3, .1), c(2, .4), c(1, 1))
hex_acc$E2SFCA <- catchment_ratio(consumers = hexagons$demand,
  providers = eldercare$capacity, cost = distmat_km, weight = step_weights)
access_map("E2SFCA", "E2SFCA: stepped distance decay")

## ---- c7-kd2sfca
hex_acc$KD2SFCA <- catchment_ratio(consumers = hexagons$demand,
  providers = eldercare$capacity, cost = distmat_km, weight = "gaussian", scale = 2)
access_map("KD2SFCA", "Gaussian KD2SFCA: scale = 2 km, untruncated")

## ---- c7-3sfca
hex_acc$FCA3 <- catchment_ratio(consumers = hexagons$demand,
  providers = eldercare$capacity, cost = distmat_km, weight = step_weights,
  normalize_weight = TRUE)
access_map("FCA3", "3SFCA: selection-adjusted stepped weights")

## ---- c7-m2sfca
hex_acc$M2SFCA <- catchment_ratio(consumers = hexagons$demand,
  providers = eldercare$capacity, cost = distmat_km, weight = step_weights,
  adjust_providers = function(w) w^2)
access_map("M2SFCA", "M2SFCA: squared provider-side weights")

## ---- c7-validation
# Independent dense calculations verify all five package outputs.
D <- distmat_km
binary <- 1 * (D < 5)
stepped <- matrix(0, nrow(D), ncol(D))
for (step in step_weights) stepped[D <= step[1]] <- step[2]
gaussian <- exp(-D^2 / (2 * 2^2))
selection <- stepped^2 / pmax(rowSums(stepped), 1e-100)
manual <- function(demand_weight, supply_weight = demand_weight) {
  denominator <- colSums(demand_weight * hexagons$demand)
  denominator[abs(denominator) < .Machine$double.eps] <- 1
  as.vector(supply_weight %*% (eldercare$capacity / denominator))
}
manual_results <- list(FCA2 = manual(binary), E2SFCA = manual(stepped),
  KD2SFCA = manual(gaussian), FCA3 = manual(selection), M2SFCA = manual(stepped, stepped^2))
methods <- names(manual_results)
validation <- bind_rows(lapply(methods, function(method) {
  value <- hex_acc[[method]]
  stopifnot(all(is.finite(value) & value >= 0),
    isTRUE(all.equal(value, manual_results[[method]], check.attributes = FALSE)))
  tibble(method, max_absolute_error = max(abs(value - manual_results[[method]])),
    demand_weighted_total = sum(value * hexagons$demand),
    zero_access_cells = sum(value == 0))
}))
stopifnot(all(abs(validation$demand_weighted_total[validation$method != "M2SFCA"] -
  sum(eldercare$capacity)) < 1e-6))
knitr::kable(validation |> mutate(max_absolute_error = formatC(max_absolute_error, format = "e", digits = 2)), digits = 3)

## ---- c7-comparison
scores <- st_drop_geometry(hex_acc) |> select(id, nearest_km, all_of(methods))
summary_results <- scores |> pivot_longer(all_of(methods), names_to = "method", values_to = "access") |>
  group_by(method) |> summarise(minimum = min(access), median = median(access),
    mean = mean(access), maximum = max(access), zero_access_cells = sum(access == 0), .groups = "drop")
knitr::kable(summary_results |> mutate(across(c(minimum, median, mean, maximum),
  ~ formatC(.x, format = "g", digits = 4))))
rank_correlations <- cor(scores[methods], method = "spearman")
knitr::kable(rank_correlations, digits = 3, caption = "Spearman rank correlations")
long <- scores |> pivot_longer(all_of(methods), names_to = "method", values_to = "access")
ggplot(long, aes(x = method, y = access)) + geom_boxplot(fill = "#b9d9e8", outlier.alpha = 0.2) +
  labs(x = NULL, y = "Modelled supply per unit demand", title = "Comparing catchment methods",
    caption = "Each hexagon has equal demand. Zero access is retained.")

## ---- c7-regions
regions <- mpsz |> select(REGION_N) |> group_by(REGION_N) |> summarise()
# Intersect only geometry: do not split and repeat accessibility attributes.
overlap <- st_intersection(st_geometry(hex_acc), st_geometry(regions))
indices <- attr(overlap, "idx")
assignment <- tibble(cell = indices[, 1], region = indices[, 2],
  area = as.numeric(st_area(overlap))) |>
  group_by(cell) |> slice_max(area, n = 1, with_ties = FALSE) |> ungroup()
regional_hex <- hex_acc
regional_hex$REGION_N <- regions$REGION_N[assignment$region[
  match(seq_len(nrow(hex_acc)), assignment$cell)]]
stopifnot(nrow(regional_hex) == nrow(hex_acc))
regional_hex$REGION_N[is.na(regional_hex$REGION_N)] <- "OUTSIDE BOUNDARY"
regional_summary <- regional_hex |> st_drop_geometry() |>
  pivot_longer(all_of(methods), names_to = "method", values_to = "access") |>
  group_by(REGION_N, method) |> summarise(cells = n(), median = median(access),
    mean = mean(access), zero_access_cells = sum(access == 0), .groups = "drop")
knitr::kable(regional_summary |> mutate(across(c(mean, median), ~ formatC(.x, format = "g", digits = 4))))
best_regions <- regional_summary |> group_by(method) |> slice_max(median, n = 1, with_ties = FALSE)

## ---- c7-sensitivity
sensitivity <- bind_rows(lapply(c(3, 5, 8), function(cutoff) {
  value <- catchment_ratio(hexagons$demand, eldercare$capacity, distmat_km, max_cost = cutoff)
  tibble(catchment_km = cutoff, zero_access_cells = sum(value == 0), median = median(value),
    rank_correlation_to_5km = cor(value, hex_acc$FCA2, method = "spearman"))
}))
knitr::kable(sensitivity, digits = 4)
write_csv(scores, "in-class7/results/accessibility-by-hexagon.csv")
write_csv(summary_results, "in-class7/results/model-summary.csv")
write_csv(regional_summary, "in-class7/results/regional-summary.csv")
write_csv(validation, "in-class7/results/validation.csv")
write_csv(sensitivity, "in-class7/results/catchment-sensitivity.csv")
write_csv(nearest_summary, "in-class7/results/nearest-summary.csv")
write_csv(as.data.frame(rank_correlations) |> tibble::rownames_to_column("method"),
  "in-class7/results/rank-correlations.csv")

## ---- c7-session
sessionInfo()
