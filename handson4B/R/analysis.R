# Run from the ISSS626-GAA project root. Shared inputs: handson4/data.
# Chapter 10: https://r4gdsa.netlify.app/chap10.html

## ---- b-packages
library(sf)
library(sfdep)
library(tmap)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(knitr)
tmap_mode("plot")
spdep::set.coresOption(NULL) # Sequential permutation execution.
options(scipen = 5)

## ---- b-data
hunan <- st_read("handson4/data/geospatial", layer = "Hunan", quiet = TRUE) |>
  st_transform(32650)
hunan2012 <- read_csv("handson4/data/aspatial/Hunan_2012.csv",
                      show_col_types = FALSE)
stopifnot(!anyDuplicated(hunan$County), !anyDuplicated(hunan2012$County),
          setequal(hunan$County, hunan2012$County))
hunan <- left_join(hunan, hunan2012, by = "County") |>
  select(NAME_2, ID_3, NAME_3, County, GDPPC)
stopifnot(nrow(hunan) == 88L, !anyNA(hunan$GDPPC),
          all(is.finite(hunan$GDPPC)), all(st_is_valid(hunan)),
          !st_is_longlat(hunan), st_crs(hunan)$epsg == 32650)
kable(head(st_drop_geometry(hunan)), caption = "Projected county data joined by County")
st_crs(hunan)

## ---- b-baseline
equal_map <- tm_shape(hunan) +
  tm_polygons(fill = "GDPPC",
    fill.scale = tm_scale_intervals(style = "equal", n = 5,
                                    values = "brewer.blues"),
    fill.legend = tm_legend(title = "GDPPC", position = tm_pos_out())) +
  tm_title("Equal intervals")
quantile_map <- tm_shape(hunan) +
  tm_polygons(fill = "GDPPC",
    fill.scale = tm_scale_intervals(style = "quantile", n = 5,
                                    values = "brewer.blues"),
    fill.legend = tm_legend(title = "GDPPC", position = tm_pos_out())) +
  tm_title("Quantiles")
tmap_arrange(equal_map, quantile_map, ncol = 2)

## ---- b-lisa
wm_q <- hunan |>
  mutate(nb = st_contiguity(geometry, queen = TRUE),
         wt = st_weights(nb, style = "W"), .before = 1)
summary(wm_q$nb)
stopifnot(all(spdep::card(wm_q$nb) > 0L),
          all(abs(vapply(wm_q$wt, sum, numeric(1)) - 1) < 1e-10))
set.seed(1234)
lisa <- wm_q |>
  mutate(local_moran = local_moran(GDPPC, nb, wt, nsim = 99,
                                   alternative = "two.sided", iseed = 1234),
         .before = 1) |>
  unnest(local_moran)
stopifnot(nrow(lisa) == 88L, identical(lisa$County, hunan$County),
          !anyNA(lisa$p_ii), all(lisa$p_ii >= 0 & lisa$p_ii <= 1))
kable(lisa |> st_drop_geometry() |>
        select(County, GDPPC, ii, z_ii, p_ii, p_ii_sim, mean) |> head(),
      digits = 4, caption = "Local Moran results: normal and permutation p-values")

## ---- b-lisa-diagnostics
ii_map <- tm_shape(lisa) +
  tm_polygons(fill = "ii",
    fill.scale = tm_scale_intervals(style = "pretty", n = 5,
                                    values = "brewer.rd_bu"),
    fill.legend = tm_legend(title = "Local I", position = tm_pos_out())) +
  tm_title("Local Moran's I")
p_map <- tm_shape(lisa) +
  tm_polygons(fill = "p_ii",
    fill.scale = tm_scale_intervals(breaks = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                                    values = "-brewer.reds"),
    fill.legend = tm_legend(title = "Normal p", position = tm_pos_out())) +
  tm_title("Normal-approximation p-values")
tmap_arrange(ii_map, p_map, ncol = 2)

## ---- b-scatter
lisa <- lisa |>
  mutate(lag_GDPPC = st_lag(GDPPC, nb, wt),
         z_GDPPC = as.numeric(scale(GDPPC)),
         z_lag_GDPPC = as.numeric(scale(lag_GDPPC)),
         lag_z = st_lag(z_GDPPC, nb, wt))
quadrant_colours <- c("High-High" = "#b33b3b", "Low-Low" = "#3264a8",
                      "Low-High" = "#8cc8e6", "High-Low" = "#e69ea4")
ggplot(lisa, aes(GDPPC, lag_GDPPC, colour = mean)) +
  geom_point(size = 2) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, colour = "grey25") +
  geom_hline(yintercept = mean(lisa$lag_GDPPC), linetype = 2, colour = "grey55") +
  geom_vline(xintercept = mean(lisa$GDPPC), linetype = 2, colour = "grey55") +
  scale_colour_manual(values = quadrant_colours) +
  labs(x = "County GDPPC", y = "Spatial lag of GDPPC", colour = "Mean quadrant",
       title = "GDPPC and its neighbours' weighted average") +
  theme_minimal(base_size = 12)

## ---- b-standard-scatter
ggplot(lisa, aes(z_GDPPC, z_lag_GDPPC, colour = mean)) +
  geom_point(size = 2) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, colour = "grey25") +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey55") +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey55") +
  scale_colour_manual(values = quadrant_colours) +
  labs(x = "Standardised GDPPC", y = "Separately standardised spatial lag",
       colour = "Mean quadrant", title = "Standardised scatterplot from Chapter 10") +
  theme_minimal(base_size = 12)

## ---- b-lisa-clusters
lisa_levels <- c("Insignificant", "Low-Low", "Low-High", "High-Low", "High-High")
lisa_colours <- c("#d7dce1", "#3264a8", "#8cc8e6", "#e69ea4", "#b33b3b")
lisa <- lisa |>
  mutate(LISA_cluster = factor(if_else(p_ii < 0.05, as.character(mean),
                                        "Insignificant"), levels = lisa_levels))
lisa_map <- tm_shape(lisa) +
  tm_polygons(fill = "LISA_cluster",
    fill.scale = tm_scale_categorical(values = lisa_colours),
    fill.legend = tm_legend(title = "Local I class", position = tm_pos_out())) +
  tm_title("LISA clusters: normal p < 0.05")
lisa_map
lisa_counts <- lisa |> st_drop_geometry() |> count(LISA_cluster, .drop = FALSE)
kable(lisa_counts, caption = "Classroom LISA classification: unadjusted normal p-values")
kable(lisa |> st_drop_geometry() |>
        filter(LISA_cluster != "Insignificant") |>
        select(County, GDPPC, lag_GDPPC, ii, p_ii, LISA_cluster) |> arrange(p_ii),
      digits = 4, caption = "Counties retained by the classroom LISA rule")

## ---- b-distance-weights
ct <- critical_threshold(st_geometry(hunan))
ct
hunan_fdw <- hunan |>
  mutate(nb = include_self(st_dist_band(geometry, upper = ct)),
         wt = st_weights(nb, style = "W"), .before = 1)
hunan_adw <- hunan |>
  mutate(nb = include_self(st_knn(geometry, k = 6)),
         wt = st_weights(nb, style = "W"), .before = 1)
stopifnot(all(spdep::card(hunan_fdw$nb) >= 2L),
          all(spdep::card(hunan_adw$nb) == 7L),
          all(vapply(seq_len(nrow(hunan)),
                     function(i) i %in% hunan_fdw$nb[[i]], logical(1))))
weight_summary <- tibble(Weights = c("Fixed distance", "Six nearest neighbours"),
  Minimum_other_neighbours = c(min(spdep::card(hunan_fdw$nb) - 1L), 6L),
  Maximum_other_neighbours = c(max(spdep::card(hunan_fdw$nb) - 1L), 6L),
  Mean_other_neighbours = c(mean(spdep::card(hunan_fdw$nb) - 1L), 6))
kable(weight_summary, digits = 2)

## ---- b-gistar
set.seed(1234)
HCSA_fdw <- hunan_fdw |>
  mutate(gistar = local_gstar_perm(GDPPC, nb, wt, nsim = 99,
                                   alternative = "two.sided", iseed = 1234)) |>
  unnest(gistar)
set.seed(1234)
HCSA_adw <- hunan_adw |>
  mutate(gistar = local_gstar_perm(GDPPC, nb, wt, nsim = 99,
                                   alternative = "two.sided", iseed = 1234)) |>
  unnest(gistar)
stopifnot(nrow(HCSA_fdw) == 88L, identical(HCSA_fdw$County, hunan$County),
          all(is.finite(HCSA_fdw$gi_star)), !anyNA(HCSA_fdw$p_sim))
kable(HCSA_fdw |> st_drop_geometry() |>
        select(County, GDPPC, gi_star, std_dev, p_value, p_sim, cluster) |> head(),
      digits = 4, caption = "Fixed-distance Gi* and its conditional-permutation output")

## ---- b-gistar-diagnostics
gi_map <- tm_shape(HCSA_fdw) +
  tm_polygons(fill = "gi_star",
    fill.scale = tm_scale_intervals(style = "pretty", n = 6,
                                    values = "-brewer.rd_bu"),
    fill.legend = tm_legend(title = "Gi* z-score", position = tm_pos_out())) +
  tm_title("Fixed-distance Gi*")
gi_p_map <- tm_shape(HCSA_fdw) +
  tm_polygons(fill = "p_sim",
    fill.scale = tm_scale_intervals(breaks = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                                    values = "-brewer.reds"),
    fill.legend = tm_legend(title = "Permutation p", position = tm_pos_out())) +
  tm_title("Two-sided permutation p-values")
tmap_arrange(gi_map, gi_p_map, ncol = 2)

## ---- b-hcsa-clusters
hcsa_levels <- c("Insignificant", "Hot spot", "Cold spot")
hcsa_colours <- c("#d7dce1", "#b33b3b", "#3264a8")
classify_gi <- function(z, p) {
  factor(case_when(p > 0.05 ~ "Insignificant",
                   z > 0 ~ "Hot spot", z < 0 ~ "Cold spot",
                   TRUE ~ "Insignificant"), levels = hcsa_levels)
}
HCSA_fdw <- HCSA_fdw |> mutate(HCSA_cluster = classify_gi(gi_star, p_sim))
HCSA_adw <- HCSA_adw |> mutate(HCSA_cluster = classify_gi(gi_star, p_sim))
hcsa_map <- function(x, title) {
  tm_shape(x) + tm_polygons(fill = "HCSA_cluster",
    fill.scale = tm_scale_categorical(values = hcsa_colours),
    fill.legend = tm_legend(title = "Gi* class", position = tm_pos_out())) +
    tm_title(title)
}
tmap_arrange(hcsa_map(HCSA_fdw, "Fixed-distance hot/cold spots"),
             hcsa_map(HCSA_adw, "Six-neighbour hot/cold spots"), ncol = 2)
hcsa_counts <- bind_rows(
  HCSA_fdw |> st_drop_geometry() |> count(HCSA_cluster, .drop = FALSE) |>
    mutate(Weights = "Fixed distance"),
  HCSA_adw |> st_drop_geometry() |> count(HCSA_cluster, .drop = FALSE) |>
    mutate(Weights = "Six nearest neighbours"))
kable(hcsa_counts, caption = "Class counts using 99 permutations and unadjusted p ≤ 0.05")
classification_changes <- sum(HCSA_fdw$HCSA_cluster != HCSA_adw$HCSA_cluster)
focal_sign_disagreement <- sum((HCSA_fdw$cluster == "High") !=
                               (HCSA_fdw$gi_star > 0))

## ---- b-sensitivity
# Extra to the chapter: 999 permutations and separate BH families for 88 counties.
lisa999 <- local_moran(hunan$GDPPC, wm_q$nb, wm_q$wt, nsim = 999,
                       alternative = "two.sided", iseed = 1234)
gi999 <- local_gstar_perm(hunan$GDPPC, hunan_fdw$nb, hunan_fdw$wt, nsim = 999,
                          alternative = "two.sided", iseed = 1234)
lisa <- lisa |> mutate(p_perm999 = lisa999$p_ii_sim,
  p_BH = p.adjust(p_perm999, method = "BH"),
  LISA_BH = factor(if_else(p_BH < 0.05, as.character(mean), "Insignificant"),
                   levels = lisa_levels))
HCSA_fdw <- HCSA_fdw |> mutate(p_perm999 = gi999$p_sim,
  p_BH = p.adjust(p_perm999, method = "BH"),
  HCSA_BH = classify_gi(gi_star, p_BH))
sensitivity_counts <- tibble(
  Method = c("Local Moran's I", "Fixed-distance Gi*"),
  Classroom_significant = c(sum(lisa$p_ii < 0.05), sum(HCSA_fdw$p_sim <= 0.05)),
  Perm999_significant = c(sum(lisa$p_perm999 < 0.05), sum(HCSA_fdw$p_perm999 <= 0.05)),
  Perm999_BH_significant = c(sum(lisa$p_BH < 0.05), sum(HCSA_fdw$p_BH <= 0.05)))
kable(sensitivity_counts, caption = "Sensitivity to simulation count, inference method and BH correction")
lisa_bh_map <- tm_shape(lisa) +
  tm_polygons(fill = "LISA_BH",
    fill.scale = tm_scale_categorical(values = lisa_colours),
    fill.legend = tm_legend(title = "Local I class", position = tm_pos_out())) +
  tm_title("Local I: 999 permutations + BH")
hcsa_bh_map <- tm_shape(HCSA_fdw) +
  tm_polygons(fill = "HCSA_BH",
    fill.scale = tm_scale_categorical(values = hcsa_colours),
    fill.legend = tm_legend(title = "Gi* class", position = tm_pos_out())) +
  tm_title("Gi*: 999 permutations + BH")
tmap_arrange(lisa_bh_map, hcsa_bh_map, ncol = 2)

## ---- b-export
dir.create("handson4B/results", recursive = TRUE, showWarnings = FALSE)
write_csv(lisa |> st_drop_geometry() |>
  select(County, GDPPC, lag_GDPPC, ii, z_ii, p_ii, p_ii_sim,
         mean, LISA_cluster, p_perm999, p_BH, LISA_BH),
  "handson4B/results/local-moran.csv")
write_csv(HCSA_fdw |> st_drop_geometry() |>
  select(County, GDPPC, gi_star, std_dev, p_value, p_sim,
         HCSA_cluster, p_perm999, p_BH, HCSA_BH),
  "handson4B/results/gistar-fixed.csv")
write_csv(HCSA_adw |> st_drop_geometry() |>
  select(County, GDPPC, gi_star, p_sim, HCSA_cluster),
  "handson4B/results/gistar-knn.csv")
write_csv(sensitivity_counts, "handson4B/results/sensitivity.csv")
stopifnot(sum(lisa_counts$n) == 88L,
          all(is.finite(lisa$p_perm999)), all(is.finite(HCSA_fdw$p_perm999)))

## ---- b-session
kable(tibble(Package = c("sf", "sfdep", "spdep", "tmap", "dplyr", "tidyr"),
  Version = vapply(c("sf", "sfdep", "spdep", "tmap", "dplyr", "tidyr"),
                   function(p) as.character(packageVersion(p)), character(1))))
R.version.string
