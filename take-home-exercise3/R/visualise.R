library(ggplot2)

blue <- "#176681"
rust <- "#A8462E"
green <- "#287562"
ink <- "#162F39"
muted <- "#566A71"
sea <- "#EBF4F6"
land_fill <- "#F8F6ED"
line_colors <- c(JRL = rust, CRL = blue)
chart_theme <- theme_minimal(base_size = 12, base_family = "sans") +
  theme(plot.background = element_rect(fill = "white", colour = NA),
        text = element_text(colour = ink),
        plot.title = element_text(face = "bold", size = 16, margin = margin(b = 5)),
        plot.subtitle = element_text(colour = muted, size = 11, margin = margin(b = 12)),
        plot.caption = element_text(colour = muted, size = 9, hjust = 0, margin = margin(t = 10)),
        legend.position = "bottom", panel.grid.minor = element_blank(),
        plot.margin = margin(14, 20, 12, 15))
map_theme <- chart_theme +
  theme(panel.background = element_rect(fill = sea, colour = NA),
        panel.grid.major = element_line(colour = "#D7E5E8", linewidth = 0.28),
        axis.title = element_blank(), axis.text = element_text(size = 8, colour = muted),
        legend.box = "horizontal")
map_extent <- coord_sf(crs = st_crs(project_crs), datum = st_crs(4326),
                       xlim = c(4600, 55000), ylim = c(24200, 50300), expand = FALSE)
land_layer <- geom_sf(data = land, fill = land_fill, colour = "#AAB8B5", linewidth = 0.32)
save_figure <- function(name, plot, width = 10.4, height = 6.1) {
  ggsave(file.path(figures, paste0(name, ".png")), plot,
         width = width, height = height, dpi = 160, bg = "white")
}

network <- ggplot() + land_layer +
  geom_sf(data = points(destinations[status == "Existing" & corridor == ""]),
          aes(shape = "Existing"), colour = "#53676F", size = 1.4, alpha = 0.65) +
  geom_sf(data = points(destinations[status == "Existing" & corridor != ""]),
          aes(shape = "Existing interchange upgrade"), size = 2.7, colour = ink,
          stroke = 0.85, fill = "white") +
  geom_sf(data = points(destinations[status == "Proposed"]),
          aes(colour = corridor, shape = "Proposed"), size = 2.6, alpha = 0.9) +
  scale_colour_manual(values = line_colors, name = "Proposed line") +
  scale_shape_manual(values = c(Existing = 16, "Existing interchange upgrade" = 22, Proposed = 17),
                     name = "Station status") +
  labs(title = "Two rail corridors expand the station choice set",
       subtitle = "Jurong Region Line and Cross Island Line phases 1-2 plus Punggol Extension",
       caption = "LTA, URA and Elliot Wutingfeng's coordinate compilation | SVY21 / EPSG:3414") +
  map_extent + map_theme
save_figure("01-network", network)

holdout <- copy(destinations[status == "Existing"])
holdout[, mode := fifelse(mrt == 1L, "MRT or MRT-LRT", "LRT only")]
holdout[, abs_error := abs(predicted_oct_taps - observed_test)]
labels <- holdout[order(-abs_error)][1:4]
labels[, `:=`(
  label_x = observed_test + c(-10500, -9400, 0, 6500),
  label_y = predicted_oct_taps + c(-3200, 6000, -4100, 4200)
)]
comparison <- ggplot(holdout, aes(observed_test, predicted_oct_taps)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, colour = muted) +
  geom_point(aes(colour = mode), alpha = 0.69, size = 2.25) +
  geom_segment(data = labels, aes(xend = label_x, yend = label_y), colour = muted, linewidth = 0.4) +
  geom_label(data = labels, aes(x = label_x, y = label_y, label = station), size = 3,
             fill = "white", colour = ink, label.padding = grid::unit(0.15, "lines"),
             show.legend = FALSE) +
  scale_colour_manual(values = c("MRT or MRT-LRT" = blue, "LRT only" = rust), name = NULL) +
  scale_x_continuous(labels = scales::label_number(scale = .001, suffix = "k")) +
  scale_y_continuous(labels = scales::label_number(scale = .001, suffix = "k")) +
  coord_equal() +
  labs(title = "October holdout exposes the largest station errors",
       subtitle = "Each point is one physical station complex. The diagonal indicates equal values.",
       x = "Observed tap-ins per day", y = "Predicted tap-ins per day",
       caption = "Observed LTA DataMall tap-ins via Udit Narayan Mall's public aggregation") + chart_theme
save_figure("02-holdout", comparison, width = 8.4, height = 7.3)

new_rank <- destinations[status == "Proposed"][order(-expanded_taps)][1:12]
spread <- sensitivity[status == "Proposed", .(lo = min(tap_ins), hi = max(tap_ins)), by = destination]
new_rank <- merge(new_rank, spread, by = "destination")
ranking <- ggplot(new_rank, aes(expanded_taps, reorder(station, expanded_taps), colour = corridor)) +
  geom_segment(aes(x = lo, xend = hi, yend = reorder(station, expanded_taps)), linewidth = 1.3, alpha = .58) +
  geom_point(size = 3.4) +
  scale_colour_manual(values = line_colors, name = "Proposed line") +
  scale_x_continuous(labels = scales::label_comma(), expand = expansion(mult = c(0.08, 0.08))) +
  labs(title = "Highest modelled demand among proposed station complexes",
       subtitle = "Dots show the fitted model. Lines span a +/-25% distance-decay stress test.",
       x = "Reallocated tap-ins per day", y = NULL,
       caption = "Combined JRL + CRL scenario | October 2026-equivalent network demand") +
  chart_theme + theme(panel.grid.major.y = element_blank())
save_figure("03-proposed-rank", ranking, height = 6.1)

existing_map <- copy(destinations[status == "Existing"])
existing_map[, direction := fifelse(delta_taps >= 0, "Gain", "Loss")]
existing_map <- existing_map[order(abs(delta_taps))]
impact <- ggplot() + land_layer +
  geom_sf(data = points(destinations[status == "Proposed"]), shape = 4,
          size = 1.8, stroke = .62, colour = "#66787C") +
  geom_sf(data = points(existing_map), aes(colour = direction, size = abs(delta_taps)), alpha = .73) +
  scale_colour_manual(values = c(Gain = blue, Loss = rust), name = "Change at existing station") +
  scale_size_area(max_size = 7, breaks = c(2000, 8000, 20000), labels = scales::label_comma(),
                  name = "Tap-ins per day") +
  labs(title = "Existing stations redistribute demand after expansion",
       subtitle = "Circle area shows the size of the modelled change. Crosses mark new complexes.",
       caption = "The baseline and expansion use the same October 2026-equivalent network total.") +
  map_extent + map_theme
save_figure("04-existing-impact", impact)

catchment <- copy(origins)
catchment[, `:=`(grid_x = east * 500 + 250, grid_y = north * 500 + 250)]
catchment[, change := cut(probability_new * 100, c(0, 10, 25, 50, 75, 100), include.lowest = TRUE,
                         labels = c("<=10%", ">10-25%", ">25-50%", ">50-75%", ">75%"))]
catchment_map <- ggplot() + land_layer +
  geom_tile(data = catchment, aes(grid_x, grid_y, fill = change), width = 500, height = 500) +
  geom_sf(data = points(destinations[status == "Proposed"]), colour = ink, shape = 4, size = 1.35) +
  scale_fill_manual(values = c("#E7F0E8", "#C3DCCA", "#86BCA9", "#428C80", "#1B595A"),
                    name = "Combined probability of selecting a new station", drop = FALSE) +
  labs(title = "New stations draw from overlapping residential catchments",
       subtitle = sprintf("The colour combines the choice probabilities of all %d proposed station complexes.", nrow(new_stations)),
       caption = "WorldPop 2025 | populated 500 m squares | unpopulated cells are left blank") +
  map_extent + map_theme
save_figure("05-catchment-shift", catchment_map)

area_top <- access[newly_within_800m > 0][1:min(10L, .N)]
access_chart <- ggplot(area_top, aes(newly_within_800m, reorder(tools::toTitleCase(tolower(planning_area)),
                                                            newly_within_800m))) +
  geom_col(fill = green, width = 0.68) +
  scale_x_continuous(labels = scales::label_comma()) +
  labs(title = "The largest gains in proximity are geographically concentrated",
       subtitle = "Residents whose nearest station moves from beyond 800 m to within 800 m",
       x = "Estimated residents newly within 800 m", y = NULL,
       caption = "Straight-line distance from population-weighted cell centres | WorldPop 2025 / URA boundaries") +
  chart_theme + theme(panel.grid.major.y = element_blank())
save_figure("06-access-gains", access_chart, height = 5.6)
