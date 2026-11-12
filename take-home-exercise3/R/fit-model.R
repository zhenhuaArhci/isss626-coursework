# Construct both interaction tables from the same population grid and CRS.
# Matrices preserve row/column correspondence with origins and destination tables.
distance_matrix <- function(d) {
  sqrt(outer(origins$x, d$x, "-")^2 + outer(origins$y, d$y, "-")^2)
}

make_scenario <- function(lines) {
  base <- copy(stations)
  additions <- future[future$corridor %in% lines]
  upgrades <- additions[name_key %in% base$name_key, .(
    added_lines = uniqueN(corridor), corridor = paste(sort(unique(corridor)), collapse = "/"),
    planned_codes = paste(station_code, collapse = "/")
  ), by = name_key]
  base[upgrades, on = "name_key", `:=`(service_count = service_count + i.added_lines,
                                     corridor = i.corridor, planned_codes = i.planned_codes, mrt = 1L)]
  rbindlist(list(base, new_stations[corridor %in% lines]), use.names = TRUE, fill = TRUE)
}

expanded <- make_scenario(c("JRL", "CRL"))
scenario_sets <- list(Existing = stations, JRL = make_scenario("JRL"),
                      CRL = make_scenario("CRL"), Both = expanded)
stopifnot(nrow(expanded) == nrow(stations) + nrow(new_stations),
          !anyDuplicated(expanded$destination), all(stations$destination %in% expanded$destination))

population_weight <- origins$market_population / sum(origins$market_population)
distance_floor <- 150
x_raw <- function(d) {
  cbind(log_bus = log1p(d$bus_500m), log_hdb = log1p(d$hdb_800m),
        extra_services = d$service_count - 1, mrt = d$mrt, log_cbd = log1p(d$cbd_km))
}
baseline_x <- x_raw(stations)
x_center <- colMeans(baseline_x)
x_scale <- apply(baseline_x, 2, sd)
standardise <- function(d) scale(x_raw(d), center = x_center, scale = x_scale)
distance0 <- distance_matrix(stations)
log_distance <- function(d) log(pmax(d, distance_floor) / 1000)
log_d0 <- log_distance(distance0)
x0 <- standardise(stations)

huff <- function(theta, log_d, x) {
  attractiveness <- exp(drop(x %*% theta[-1L]))
  w <- sweep(exp(-theta[1L] * log_d), 2, attractiveness, "*")
  w / rowSums(w)
}
aggregate_share <- function(p) drop(crossprod(population_weight, p))

# Marginal cross-entropy calibrates aggregate station shares. A mild ridge
# penalty shrinks standardised attraction coefficients because they correlate.
observed_share <- stations$observed_train / sum(stations$observed_train)
ridge <- 0.001
loss <- function(theta, ix = seq_len(nrow(stations))) {
  p <- aggregate_share(huff(theta, log_d0, x0))[ix]
  q <- observed_share[ix]
  -sum((q / sum(q)) * log(p / sum(p))) + ridge * sum(theta[-1L]^2) / 2
}
fit_huff <- function(ix = seq_len(nrow(stations))) {
  optim(c(2, rep(0, ncol(x0))), loss, ix = ix, method = "L-BFGS-B",
        lower = c(0.25, rep(-2.5, ncol(x0))), upper = c(6, rep(2.5, ncol(x0))),
        control = list(factr = 1e7))
}
fit <- fit_huff()
stopifnot(fit$convergence == 0L, all(is.finite(fit$par)))
theta <- fit$par
names(theta) <- c("distance_decay", colnames(x0))
decay_only <- optimize(function(b) loss(c(b, rep(0, ncol(x0)))), c(0.25, 6))$minimum
theta_distance <- c(decay_only, rep(0, ncol(x0)))
p0 <- huff(theta, log_d0, x0)
stopifnot(all(is.finite(p0)), max(abs(rowSums(p0) - 1)) < 1e-10)
nearest0 <- max.col(-distance0, ties.method = "first")
nearest_share <- as.numeric(rowsum(population_weight, nearest0)[, 1])
nearest_index <- as.integer(rownames(rowsum(population_weight, nearest0)))
nearest_null <- numeric(nrow(stations))
nearest_null[nearest_index] <- nearest_share
share0 <- aggregate_share(p0)
training_total <- sum(stations$observed_train)
test_total <- sum(stations$observed_test)

score <- function(observed, predicted) {
  c(mae = mean(abs(predicted - observed)), rmse = sqrt(mean((predicted - observed)^2)),
    wape_pct = 100 * sum(abs(predicted - observed)) / sum(observed),
    spearman = cor(observed, predicted, method = "spearman"))
}
model_shares <- list("Nearest station" = nearest_null,
                     "Distance-only Huff" = aggregate_share(huff(theta_distance, log_d0, x0)),
                     "Attraction Huff" = share0)
validation <- rbindlist(lapply(names(model_shares), function(model) {
  s <- model_shares[[model]]
  rbindlist(list(
    cbind(data.table(model = model, period = "Calibration (Aug-Sep)"),
           as.data.table(as.list(score(stations$observed_train, s * training_total)))),
    cbind(data.table(model = model, period = "Holdout (Oct)"),
           as.data.table(as.list(score(stations$observed_test, s * training_total))))
  ))
}), fill = TRUE)
stopifnot(all(is.finite(validation$mae)), all(is.finite(validation$wape_pct)))
fwrite(validation, file.path(results, "model_validation.csv"))
fwrite(data.table(term = names(theta), coefficient = theta,
                  centre = c(NA_real_, x_center), scale = c(NA_real_, x_scale)),
       file.path(results, "coefficients.csv"))

# Geographic holdout checks transfer beyond nearby calibration stations.
# Compare conditional shares inside each held-out region at that region's observed total.
region_index <- st_intersects(points(stations), areas)
region <- vapply(region_index, function(i) if (length(i)) areas$REGION_N[i[1L]] else NA_character_,
                 character(1))
blocks <- rbindlist(lapply(sort(unique(na.omit(region))), function(label) {
  ix <- which(region == label)
  if (length(ix) < 5L) return(NULL)
  fitted <- fit_huff(setdiff(seq_len(nrow(stations)), ix))
  simple <- optimize(function(b) loss(c(b, rep(0, ncol(x0))), setdiff(seq_len(nrow(stations)), ix)),
                     c(0.25, 6))$minimum
  stopifnot(fitted$convergence == 0L)
  observed <- stations$observed_test[ix]
  a <- aggregate_share(huff(fitted$par, log_d0, x0))[ix]
  b <- aggregate_share(huff(c(simple, rep(0, ncol(x0))), log_d0, x0))[ix]
  data.table(region = label, stations = length(ix),
             attraction_wape_pct = unname(score(observed, a / sum(a) * sum(observed))["wape_pct"]),
             distance_wape_pct = unname(score(observed, b / sum(b) * sum(observed))["wape_pct"]))
}))
fwrite(blocks, file.path(results, "spatial_holdout.csv"))

evaluate <- function(d, params, daily_total) {
  raw_d <- distance_matrix(d)
  probability <- huff(params, log_distance(raw_d), standardise(d))
  stopifnot(max(abs(rowSums(probability) - 1)) < 1e-10)
  tap_ins <- aggregate_share(probability) * daily_total
  stopifnot(abs(sum(tap_ins) - daily_total) < 1e-6)
  list(distance = raw_d, probability = probability, tap_ins = tap_ins)
}

expansion <- lapply(scenario_sets, evaluate, params = theta, daily_total = test_total)
destinations <- copy(expanded)
destinations[, `:=`(
  fitted_train_taps = share0[match(destination, stations$destination)] * training_total,
  predicted_oct_taps = share0[match(destination, stations$destination)] * training_total,
  baseline_taps = expansion$Existing$tap_ins[match(destination, stations$destination)],
  expanded_taps = expansion$Both$tap_ins
)]
destinations[, delta_taps := expanded_taps - fcoalesce(baseline_taps, 0)]
destinations[, delta_pct := 100 * delta_taps / baseline_taps]
destinations[, name_key := NULL]
fwrite(destinations, file.path(results, "destinations.csv"))
fwrite(stations[, .(destination, station, official_footprint, service_count, bus_500m, hdb_800m,
                    cbd_km, observed_train, observed_test, region)], file.path(results, "baseline_stations.csv"))

# Export row-level model inputs and allocations locally. A compact sample travels with the report.
write_interactions <- function(d, evaluated, scenario) {
  index <- CJ(i = seq_len(nrow(origins)), j = seq_len(nrow(d)), sorted = TRUE)
  matrix_index <- as.matrix(index)
  long <- data.table(
    origin = origins$origin[index$i], destination = d$destination[index$j],
    distance_m = round(evaluated$distance[matrix_index], 2),
    probability = signif(evaluated$probability[matrix_index], 9),
    market_population = round(origins$market_population[index$i], 4),
    bus_500m = d$bus_500m[index$j], hdb_800m = d$hdb_800m[index$j],
    service_count = d$service_count[index$j], mrt = d$mrt[index$j],
    cbd_km = round(d$cbd_km[index$j], 4)
  )
  stopifnot(nrow(long) == nrow(origins) * nrow(d), !anyDuplicated(long, by = c("origin", "destination")))
  name <- paste0("interactions-", scenario)
  fwrite(long, file.path(derived, paste0(name, ".csv.gz")), compress = "gzip")
  sample <- long[origin == origins$origin[which.max(origins$market_population)]]
  fwrite(sample[order(-probability)][1:10], file.path(results, paste0(name, "-sample.csv")))
  nrow(long)
}
interaction_counts <- c(
  existing = write_interactions(stations, expansion$Existing, "existing"),
  expanded = write_interactions(expanded, expansion$Both, "expanded")
)

scenario_totals <- rbindlist(lapply(names(expansion), function(label) {
  d <- scenario_sets[[label]]
  p <- expansion[[label]]$tap_ins
  is_new <- d$status == "Proposed"
  data.table(scenario = label, complexes = nrow(d), new_complexes = sum(is_new),
             new_taps = sum(p[is_new]), existing_taps = sum(p[!is_new]), network_taps = sum(p))
}))
fwrite(scenario_totals, file.path(results, "scenario_totals.csv"))

# Stress-test the distance coefficient without refitting other features.
decays <- theta[1L] * c(0.75, 1, 1.25)
sensitivity <- rbindlist(lapply(decays, function(beta) {
  params <- c(beta, theta[-1L])
  e <- evaluate(expanded, params, test_total)
  data.table(decay = beta, destination = expanded$destination, station = expanded$station,
             status = expanded$status, corridor = expanded$corridor, tap_ins = e$tap_ins)
}))
fwrite(sensitivity, file.path(results, "distance_sensitivity.csv"))

# Compare changes in choice and physical access. The latter uses no ridership fit.
origins[, `:=`(
  closest_existing_m = distance0[cbind(seq_len(.N), nearest0)],
  closest_expanded_m = apply(expansion$Both$distance, 1, min),
  baseline_choice = stations$destination[max.col(p0, ties.method = "first")],
  expanded_choice = expanded$destination[max.col(expansion$Both$probability, ties.method = "first")],
  probability_new = rowSums(expansion$Both$probability[, expanded$status == "Proposed", drop = FALSE])
)]
origins[, distance_saved_m := closest_existing_m - closest_expanded_m]
origins[, newly_within_800m := closest_existing_m > 800 & closest_expanded_m <= 800]
stopifnot(min(origins$distance_saved_m) > -1e-7)
access <- origins[, .(
  population = sum(market_population),
  mean_m_saved = weighted.mean(distance_saved_m, market_population),
  newly_within_800m = sum(market_population[newly_within_800m]),
  choice_changed_pct = 100 * weighted.mean(baseline_choice != expanded_choice, market_population),
  probability_new_pct = 100 * weighted.mean(probability_new, market_population)
), by = planning_area]
setorder(access, -newly_within_800m)
fwrite(access, file.path(results, "access_by_planning_area.csv"))
fwrite(origins, file.path(results, "origins.csv"))
fwrite(origins[, .(origin, x, y, market_population, planning_area, closest_existing_m,
                   closest_expanded_m, distance_saved_m, newly_within_800m, baseline_choice,
                   expanded_choice, probability_new)], file.path(results, "catchment_grid.csv"))

audit <- data.table(
  item = c("raw_lta_station_footprints", "mirror_fare_rows", "baseline_complexes", "official_geometry_matches",
           "excluded_inactive_complexes", "operating_bus_stops", "hdb_features", "positive_population_pixels",
           "origin_cells", "population_total", "baseline_interaction_rows", "expanded_interaction_rows",
           "proposed_complexes", "interchange_upgrades", "training_daily_taps", "october_daily_taps",
           "newly_within_800m", "max_interchange_coordinate_difference_m", "distance_only_decay"),
  value = c(nrow(station_shape), length(mirror_ids), nrow(stations), sum(stations$official_footprint),
            nrow(inactive), nrow(bus), length(hdb), nrow(pixels), nrow(origins), sum(origins$market_population),
            interaction_counts, nrow(new_stations), nrow(existing_upgrades), training_total, test_total,
            sum(origins$market_population[origins$newly_within_800m]), max(distance_check), decay_only)
)
fwrite(audit, file.path(results, "data_audit.csv"))
