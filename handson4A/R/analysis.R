# Run from the ISSS626-GAA project root. Shared inputs: handson4/data.
# Chapter 9: https://r4gdsa.netlify.app/chap09.html

## ---- a-packages
library(sf)
library(spdep)
library(tmap)
library(dplyr)
library(readr)
library(ggplot2)
library(knitr)
tmap_mode("plot")
options(scipen = 5)

## ---- a-data
hunan <- st_read("handson4/data/geospatial", layer = "Hunan", quiet = TRUE)
hunan2012 <- read_csv("handson4/data/aspatial/Hunan_2012.csv",
                      show_col_types = FALSE)
stopifnot(!anyDuplicated(hunan$County), !anyDuplicated(hunan2012$County),
          setequal(hunan$County, hunan2012$County))
hunan <- left_join(hunan, hunan2012, by = "County") |>
  select(NAME_2, ID_3, NAME_3, County, GDPPC)
stopifnot(nrow(hunan) == 88L, !anyNA(hunan$GDPPC),
          all(is.finite(hunan$GDPPC)), all(st_is_valid(hunan)),
          isTRUE(st_is_longlat(hunan)))
kable(head(st_drop_geometry(hunan)), caption = "Joined Hunan county data, 2012")
st_crs(hunan)
gdppc_summary <- tibble(Counties = nrow(hunan), Mean = mean(hunan$GDPPC),
  Median = median(hunan$GDPPC), SD = sd(hunan$GDPPC),
  Minimum = min(hunan$GDPPC), Maximum = max(hunan$GDPPC))
kable(gdppc_summary, digits = 2)

## ---- a-baseline
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

## ---- a-weights
wm_q <- poly2nb(hunan, queen = TRUE, row.names = hunan$County)
summary(wm_q)
rswm_q <- nb2listw(wm_q, style = "W", zero.policy = TRUE)
rswm_q
stopifnot(all(card(wm_q) > 0L),
          all(abs(vapply(rswm_q$weights, sum, numeric(1)) - 1) < 1e-10),
          identical(attr(wm_q, "region.id"), hunan$County))
coords <- st_coordinates(st_centroid(st_transform(hunan, 32650)))
plot(st_geometry(st_transform(hunan, 32650)), col = "#f0f4f8",
     border = "#bac7d1", main = "Queen contiguity between Hunan counties")
plot(wm_q, coords, add = TRUE, col = "#456b89", pch = 20, cex = 0.4)

## ---- a-moran
moran_result <- moran.test(hunan$GDPPC, listw = rswm_q,
  alternative = "greater", zero.policy = TRUE, na.action = na.fail)
moran_result
set.seed(1234)
moran_perm <- moran.mc(hunan$GDPPC, listw = rswm_q, nsim = 999,
  alternative = "greater", zero.policy = TRUE, na.action = na.fail)
moran_perm

## ---- a-moran-scatter
z_gdppc <- as.numeric(scale(hunan$GDPPC))
moran_scatter <- tibble(County = hunan$County, z = z_gdppc,
                        lag_z = lag.listw(rswm_q, z_gdppc))
scatter_fit <- lm(lag_z ~ z, data = moran_scatter)
stopifnot(abs(unname(coef(scatter_fit)[2]) -
              unname(moran_result$estimate[1])) < 1e-10)
ggplot(moran_scatter, aes(z, lag_z)) +
  geom_hline(yintercept = 0, colour = "grey65", linetype = 2) +
  geom_vline(xintercept = 0, colour = "grey65", linetype = 2) +
  geom_point(colour = "#356a92", size = 2) +
  geom_smooth(method = "lm", se = FALSE, colour = "#b94948") +
  labs(x = "Standardised county GDPPC", y = "W × standardised GDPPC",
       title = "Moran scatterplot",
       subtitle = paste("Regression slope =", round(coef(scatter_fit)[2], 4))) +
  theme_minimal(base_size = 12)

## ---- a-moran-permutation
moran_sim <- tibble(statistic = moran_perm$res[seq_len(999)])
kable(tibble(Mean = mean(moran_sim$statistic),
  Variance = var(moran_sim$statistic), Minimum = min(moran_sim$statistic),
  Maximum = max(moran_sim$statistic)), digits = 6,
  caption = "Moran's I: 999 simulated values only")
ggplot(moran_sim, aes(statistic)) +
  geom_histogram(bins = 25, fill = "#779db7", colour = "white") +
  geom_vline(xintercept = -1 / (nrow(hunan) - 1), linetype = 2,
             colour = "#4d5965") +
  geom_vline(xintercept = unname(moran_perm$statistic), colour = "#b94948",
             linewidth = 1) +
  labs(x = "Permuted Moran's I", y = "Frequency",
       title = "Random-label reference distribution",
       subtitle = "Red: observed I; dashed: theoretical null expectation") +
  theme_minimal(base_size = 12)

## ---- a-geary
geary_result <- geary.test(hunan$GDPPC, listw = rswm_q,
  alternative = "greater", zero.policy = TRUE)
geary_result
set.seed(1234)
geary_perm <- geary.mc(hunan$GDPPC, listw = rswm_q, nsim = 999,
  alternative = "greater", zero.policy = TRUE)
geary_perm

## ---- a-geary-permutation
geary_sim <- tibble(statistic = geary_perm$res[seq_len(999)])
kable(tibble(Mean = mean(geary_sim$statistic),
  Variance = var(geary_sim$statistic), Minimum = min(geary_sim$statistic),
  Maximum = max(geary_sim$statistic)), digits = 6,
  caption = "Geary's C: 999 simulated values only")
ggplot(geary_sim, aes(statistic)) +
  geom_histogram(bins = 25, fill = "#779db7", colour = "white") +
  geom_vline(xintercept = 1, linetype = 2, colour = "#4d5965") +
  geom_vline(xintercept = unname(geary_perm$statistic), colour = "#b94948",
             linewidth = 1) +
  labs(x = "Permuted Geary's C", y = "Frequency",
       title = "Random-label reference distribution",
       subtitle = "Red: observed C; dashed: null expectation of 1") +
  theme_minimal(base_size = 12)

## ---- a-moran-correlogram
MI_corr <- sp.correlogram(wm_q, hunan$GDPPC, order = 6,
                         method = "I", style = "W", zero.policy = TRUE)
plot(MI_corr, main = "Moran's I across six neighbour orders")
print(MI_corr)

## ---- a-geary-correlogram
GC_corr <- sp.correlogram(wm_q, hunan$GDPPC, order = 6,
                         method = "C", style = "W", zero.policy = TRUE)
plot(GC_corr, main = "Geary's C across six neighbour orders")
print(GC_corr)

## ---- a-export
global_results <- tibble(Statistic = c("Moran's I", "Geary's C"),
  Observed = c(unname(moran_result$estimate[1]), unname(geary_result$estimate[1])),
  Null_expectation = c(unname(moran_result$estimate[2]), unname(geary_result$estimate[2])),
  Analytical_p = c(moran_result$p.value, geary_result$p.value),
  Permutation_p = c(moran_perm$p.value, geary_perm$p.value),
  Permutations = 999L, Seed = 1234L)
correlogram_results <- bind_rows(
  as_tibble(MI_corr$res, .name_repair = "minimal") |>
    setNames(c("Estimate", "Expectation", "Variance")) |>
    mutate(Statistic = "Moran's I", Order = row_number()),
  as_tibble(GC_corr$res, .name_repair = "minimal") |>
    setNames(c("Estimate", "Expectation", "Variance")) |>
    mutate(Statistic = "Geary's C", Order = row_number())) |>
  mutate(Z = (Estimate - Expectation) / sqrt(Variance),
         P_two_sided = 2 * pnorm(abs(Z), lower.tail = FALSE)) |>
  group_by(Statistic) |>
  mutate(P_BH = p.adjust(P_two_sided, method = "BH")) |>
  ungroup()
dir.create("handson4A/results", recursive = TRUE, showWarnings = FALSE)
write_csv(global_results, "handson4A/results/global-tests.csv")
write_csv(correlogram_results, "handson4A/results/correlograms.csv")
kable(global_results, digits = 6, caption = "Global autocorrelation results")
kable(correlogram_results, digits = 6,
      caption = "Correlogram estimates and BH adjustment within each six-lag family")

## ---- a-session
kable(tibble(Package = c("sf", "spdep", "tmap", "dplyr", "readr", "ggplot2"),
  Version = vapply(c("sf", "spdep", "tmap", "dplyr", "readr", "ggplot2"),
                   function(p) as.character(packageVersion(p)), character(1))))
R.version.string
