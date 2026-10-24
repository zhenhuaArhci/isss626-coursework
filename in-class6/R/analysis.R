# Execute from the Quarto project root.
# Inputs are the supplied Hunan time series and the shared county shapefile.

## ---- c6-packages
library(sf)
library(sfdep)
library(tmap)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(Kendall)
library(plotly)
library(knitr)
spdep::set.coresOption(NULL)
tmap_mode("plot")
options(scipen = 5)
theme_set(theme_minimal(base_size = 12) +
  theme(plot.background = element_rect(fill = "#faf7f0", colour = NA),
        panel.grid.minor = element_blank(),
        plot.title = element_text(colour = "#8f3f2d", face = "bold")))

## ---- c6-data
hunan <- st_read("handson4/data/geospatial", layer = "Hunan", quiet = TRUE) |>
  st_transform(32650) |>
  arrange(County)
GDPPC <- read_csv("in-class6/data/aspatial/Hunan_GDPPC.csv",
  col_types = cols(Year = col_integer(), County = col_character(),
                   GDPPC = col_double()))
years <- sort(unique(GDPPC$Year))
stopifnot(nrow(hunan) == 88L, !anyDuplicated(hunan$County),
  !anyDuplicated(GDPPC[c("County", "Year")]),
  setequal(hunan$County, GDPPC$County), identical(years, 2005:2021),
  nrow(GDPPC) == nrow(hunan) * length(years),
  !anyNA(GDPPC), all(is.finite(GDPPC$GDPPC)), all(GDPPC$GDPPC > 0),
  all(st_is_valid(hunan)), st_crs(hunan)$epsg == 32650)
kable(head(GDPPC), caption = "Actual Hunan GDP per capita observations")

## ---- c6-cube
GDPPC_st <- spacetime(GDPPC, hunan, .loc_col = "County", .time_col = "Year")
stopifnot(is_spacetime_cube(GDPPC_st))
is_spacetime_cube(GDPPC_st)
cube_summary <- tibble(Counties = nrow(hunan), Years = length(years),
                      First_year = min(years), Last_year = max(years),
                      Observations = nrow(GDPPC))
kable(cube_summary, caption = "Validated, complete space-time cube")

## ---- c6-weights
GDPPC_nb <- GDPPC_st |>
  activate("geometry") |>
  mutate(nb = include_self(st_contiguity(geometry)),
         wt = st_inverse_distance(nb, geometry, scale = 1, alpha = 1),
         .before = 1) |>
  set_nbs("nb") |>
  set_wts("wt")
geo_nb <- attr(GDPPC_nb, "geometry")$nb
geo_wt <- attr(GDPPC_nb, "geometry")$wt
stopifnot(all(vapply(geo_wt, function(w) all(is.finite(w)), logical(1))),
  all(vapply(seq_along(geo_nb), function(i) {
    geo_wt[[i]][match(i, geo_nb[[i]])] == 0
  }, logical(1))))
summary(geo_nb)

## ---- c6-gi
set.seed(1234)
gi_stars <- GDPPC_nb |>
  group_by(Year) |>
  mutate(gi_star = local_gstar_perm(GDPPC, nb, wt, nsim = 499,
                                   alternative = "two.sided", iseed = 1234)) |>
  unnest(gi_star) |>
  ungroup()
stopifnot(nrow(gi_stars) == nrow(GDPPC), all(is.finite(gi_stars$gi_star)))
kable(gi_stars |> select(County, Year, GDPPC, gi_star, p_sim) |> head(),
      digits = 4, caption = "Annual local statistics; unadjusted permutation p-values")

## ---- c6-changsha
cbg <- gi_stars |>
  filter(County == "Changsha") |>
  arrange(Year) |>
  select(County, Year, gi_star)
changsha_plot <- ggplot(cbg, aes(Year, gi_star)) +
  geom_line(colour = "#9a432e", linewidth = 0.9) +
  geom_point(colour = "#9a432e", size = 2) +
  scale_x_continuous(breaks = seq(2005, 2021, 2)) +
  labs(title = "Changsha: annual local clustering statistic",
       x = "Year", y = "Local Gi* score (guide's inverse-distance weights)")
changsha_plot

## ---- c6-interactive
ggplotly(changsha_plot, tooltip = c("x", "y")) |>
  layout(paper_bgcolor = "#faf7f0", plot_bgcolor = "#faf7f0") |>
  config(displaylogo = FALSE, responsive = TRUE)

## ---- c6-mk
changsha_mk <- cbg |>
  summarise(mk = list(unclass(Kendall::MannKendall(gi_star)))) |>
  unnest_wider(mk)
kable(changsha_mk, digits = 6, caption = "Changsha Mann–Kendall test (sl is the p-value)")
county_mk <- gi_stars |>
  arrange(County, Year) |>
  group_by(County) |>
  summarise(mk = list(unclass(Kendall::MannKendall(gi_star))), .groups = "drop") |>
  unnest_wider(mk) |>
  mutate(p_bh = p.adjust(sl, method = "BH"))
top_trends <- county_mk |> arrange(sl, desc(abs(tau))) |> slice_head(n = 10)
kable(top_trends, digits = 6, caption = "Ten smallest trend p-values; BH adjustment across 88 counties")

## ---- c6-ehsa
set.seed(1234)
ehsa <- emerging_hotspot_analysis(x = GDPPC_st, .var = "GDPPC",
  k = 1, nsim = 99, threshold = 0.01, include_gi = TRUE)
# Record the cube's geometry order when attaching per-bin statistics.
bin_results <- bind_cols(
  tibble(County = rep(attr(GDPPC_st, "geometry")$County, times = length(years)),
         Year = rep(years, each = nrow(hunan))),
  as_tibble(attr(ehsa, "gi_star")))
bin_summary <- bin_results |>
  group_by(County) |>
  summarise(mk_tau = as.numeric(MannKendall(gi_star)$tau),
    mk_p = as.numeric(MannKendall(gi_star)$sl),
    hot_years = sum(gi_star > 0 & p_sim <= 0.01),
    cold_years = sum(gi_star < 0 & p_sim <= 0.01), .groups = "drop")
ehsa <- as_tibble(ehsa) |>
  left_join(bin_summary, by = c("location" = "County")) |>
  mutate(p_bh = p.adjust(p_value, method = "BH"))
stopifnot(nrow(ehsa) == 88L, !anyNA(ehsa),
  all(abs(ehsa$tau - ehsa$mk_tau) < 1e-7),
  all(abs(ehsa$p_value - ehsa$mk_p) < 1e-7))
class_counts <- ehsa |> count(classification, sort = TRUE)
kable(class_counts, caption = "EHSA classes: 99 simulations, k = 1, threshold = 0.01")

## ---- c6-distribution
ggplot(class_counts, aes(n, reorder(classification, n))) +
  geom_col(fill = "#9a432e", width = 0.7) +
  geom_text(aes(label = n), hjust = -0.2) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Distribution of emerging hot spot classes",
       x = "Number of counties", y = NULL)

## ---- c6-map
hunan_ehsa <- hunan |> left_join(ehsa, by = c("County" = "location"))
classes <- sort(unique(ehsa$classification))
class_colours <- c("consecutive coldspot" = "#889d86",
  "consecutive hotspot" = "#bd6e38", "diminishing coldspot" = "#b3bb9c",
  "diminishing hotspot" = "#c99177", "historical coldspot" = "#d6dbc8",
  "historical hotspot" = "#e1c3b0", "intensifying coldspot" = "#3f5f4b",
  "intensifying hotspot" = "#742d24", "new coldspot" = "#567a63",
  "new hotspot" = "#b84328", "no pattern detected" = "#ded9d0",
  "oscilating coldspot" = "#8f8c5e", "oscilating hotspot" = "#a07944",
  "persistent coldspot" = "#607560", "persistent hotspot" = "#934e36",
  "sporadic coldspot" = "#a1b39a", "sporadic hotspot" = "#dda17e")
hunan_ehsa$classification <- factor(hunan_ehsa$classification, levels = classes)
ehsa_map <- tm_shape(hunan_ehsa) +
  tm_polygons(fill = "classification", col = "#fffaf2", lwd = 0.5,
    fill.scale = tm_scale_categorical(values = unname(class_colours[classes]),
                                      levels = classes, levels.drop = FALSE),
    fill.legend = tm_legend(title = "EHSA class", position = tm_pos_out())) +
  tm_layout(bg.color = "#faf7f0", frame = FALSE) +
  tm_title("Hunan · All EHSA classifications")
ehsa_map

## ---- c6-filtered-map
ehsa_sig <- hunan_ehsa |> filter(p_value < 0.05)
tm_shape(hunan_ehsa) +
  tm_polygons(fill = "#ded9d0", col = "#fffaf2", lwd = 0.5) +
  tm_shape(ehsa_sig) +
  tm_polygons(fill = "classification", col = "#fffaf2", lwd = 0.5,
    fill.scale = tm_scale_categorical(values = unname(class_colours[classes]),
                                      levels = classes, levels.drop = FALSE),
    fill.legend = tm_legend(title = "Class (trend p < 0.05)", position = tm_pos_out())) +
  tm_layout(bg.color = "#faf7f0", frame = FALSE) +
  tm_title("Hunan · Counties with a significant Gi* trend")

## ---- c6-sensitivity
set.seed(1234)
ehsa_999 <- emerging_hotspot_analysis(x = GDPPC_st, .var = "GDPPC",
  k = 1, nsim = 999, threshold = 0.01)
sensitivity <- ehsa |>
  select(location, class_99 = classification) |>
  left_join(as_tibble(ehsa_999) |>
    select(location, class_999 = classification), by = "location") |>
  mutate(changed = class_99 != class_999)
stopifnot(nrow(sensitivity) == 88L, !anyNA(sensitivity),
  all(abs(ehsa$tau - ehsa_999$tau[match(ehsa$location, ehsa_999$location)]) < 1e-7))
kable(sensitivity |> count(class_99, class_999, changed, sort = TRUE),
  caption = "Class stability when increasing simulations from 99 to 999")

## ---- c6-export
dir.create("in-class6/results", recursive = TRUE, showWarnings = FALSE)
write_csv(gi_stars |> select(-nb, -wt), "in-class6/results/annual-gi.csv")
write_csv(county_mk, "in-class6/results/annual-mann-kendall.csv")
write_csv(ehsa, "in-class6/results/ehsa-99.csv")
write_csv(bin_results, "in-class6/results/ehsa-bin-statistics.csv")
write_csv(sensitivity, "in-class6/results/class-sensitivity.csv")
package_versions <- tibble(package = c("R", "sf", "sfdep", "spdep", "tmap",
  "Kendall", "plotly", "dplyr"), version = c(as.character(getRversion()),
  vapply(c("sf", "sfdep", "spdep", "tmap", "Kendall", "plotly", "dplyr"),
         function(p) as.character(packageVersion(p)), character(1))))
write_csv(package_versions, "in-class6/results/package-versions.csv")
kable(package_versions, caption = "Reproduction environment")
