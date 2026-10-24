# Run from the project root; chunks are also read into index.qmd.
## ---- i8-packages
library(sf)
library(dplyr)
library(tidyr)
library(readr)
library(MCI)
library(tmap)
library(ggplot2)
tmap_mode("plot")

## ---- i8-import
raw <- "inclass8/data/raw"
stores <- st_read(file.path(raw, "geospatial/STORES_TC.shp"), quiet = TRUE)
town <- st_read(file.path(raw, "geospatial/TOWN_TC.shp"), quiet = TRUE)
village_all <- st_read(file.path(raw, "geospatial/VILLAGE_TC.shp"), quiet = TRUE)
pop2019 <- read_csv(file.path(raw, "aspatial/pop2019.csv"),
  col_types = cols(.default = col_guess(), V_ID = col_character()))
store_sales <- read_csv(file.path(raw, "aspatial/Store_sales_data.csv"),
  show_col_types = FALSE)
stopifnot(st_crs(stores)$epsg == 3826, st_crs(town) == st_crs(stores),
  st_crs(village_all) == st_crs(stores), all(st_is_valid(village_all)),
  !anyDuplicated(pop2019$V_ID))

## ---- i8-prepare
store_sales_aggre <- store_sales %>%
  group_by(`Store Code`) %>%
  summarise(Total_Bill = sum(Bills, na.rm = TRUE),
    Total_Sales = sum(Amount, na.rm = TRUE), .groups = "drop")
stores <- stores %>%
  left_join(store_sales_aggre, by = c("Store_CD" = "Store Code")) %>%
  mutate(Store_size = 100)
pop2019 <- pop2019 %>% mutate(V_ID = gsub("-", "", V_ID, fixed = TRUE))
village_joined <- village_all %>%
  left_join(pop2019, by = c("VILLCODE" = "V_ID"))
excluded_villages <- village_joined %>% filter(is.na(P_CNT)) %>%
  st_drop_geometry() %>% select(VILLCODE, TOWNNAME, VILLNAME)
unmatched_population <- anti_join(pop2019, st_drop_geometry(village_all),
  by = c("V_ID" = "VILLCODE"))
village <- village_joined %>% filter(!is.na(P_CNT))
stopifnot(!anyNA(stores$Total_Sales), all(village$P_CNT > 0),
  !anyNA(village[c("H_CNT", "M_CNT", "F_CNT")]))

## ---- i8-distance
# Work on the geometry alone to avoid assuming attributes are spatially constant.
# This is an interior representative point, not a geometric centroid.
village_center <- st_point_on_surface(st_geometry(village))
distmat <- st_distance(village_center, stores)
rownames(distmat) <- village$VILLCODE
colnames(distmat) <- stores$Store_CD
distmat_long <- as.data.frame(unclass(units::drop_units(distmat))) %>%
  tibble::rownames_to_column("VILLCODE") %>%
  pivot_longer(-VILLCODE, names_to = "Store_CD", values_to = "distance")
stopifnot(all(is.finite(distmat_long$distance)), all(distmat_long$distance > 0))
interaction_matrix <- distmat_long %>%
  left_join(st_drop_geometry(stores) %>% select(Store_CD, Store_size),
    by = "Store_CD") %>% select(VILLCODE, Store_CD, Store_size, distance) %>%
  as.data.frame()

## ---- i8-huff
# Explicitly retain the Chapter 18 / MCI default power-model parameters.
huff_share <- huff.shares(interaction_matrix,
  "VILLCODE", "Store_CD", "Store_size", "distance", gamma = 1, lambda = -2)
# MCI exposes the denominator as a list column; flatten it for clean exports.
huff_share$sum_U_ij <- as.numeric(huff_share$sum_U_ij)
huff_share_wider <- huff_share %>% select(VILLCODE, Store_CD, p_ij) %>%
  pivot_wider(names_from = Store_CD, values_from = p_ij)

## ---- i8-bt
market_share <- huff_share %>% filter(Store_CD == "BT")
Trade_area <- village %>%
  left_join(market_share %>% select(VILLCODE, p_ij), by = "VILLCODE")
selected_store <- stores %>% filter(Store_CD == "BT")
bt_map <- tm_shape(Trade_area) +
  tm_polygons(fill = "p_ij", col = "grey35", lwd = 0.35,
    fill.scale = tm_scale_intervals(style = "quantile", n = 10,
      values = "brewer.blues"),
    fill.legend = tm_legend(title = "Patronage probability\n10 quantile classes",
      position = tm_pos_out("right", "center"), text.size = 0.65)) +
  tm_shape(selected_store) + tm_dots(size = 0.35, fill = "red", col = "black") +
  tm_title("Patronage probability of store BT by village", size = 1) +
  tm_compass(type = "8star", size = 2, position = c("right", "bottom")) +
  tm_scalebar(position = c("left", "bottom")) +
  tm_grid(alpha = 0.2) + tm_layout(frame = TRUE)

## ---- i8-total
huff_share_pp <- huff_share %>%
  left_join(st_drop_geometry(village) %>% select(VILLCODE, P_CNT),
    by = "VILLCODE") %>% as.data.frame()
huff_total <- shares.total(huff_share_pp, "VILLCODE", "Store_CD",
  "p_ij", "P_CNT")
store_sales_est <- stores %>%
  left_join(huff_total, by = c("Store_CD" = "suppliers_single"))
sales_test <- cor.test(store_sales_est$sum_E_j, store_sales_est$Total_Sales,
  method = "pearson")
sales_plot <- ggplot(st_drop_geometry(store_sales_est),
    aes(sum_E_j, Total_Sales / 1e6)) +
  geom_smooth(method = "lm", se = TRUE, colour = "#8f3f2d", fill = "#ead5cc") +
  geom_point(colour = "#256c79", size = 2.5) +
  geom_text(aes(label = Store_CD), nudge_y = 0.14, size = 3, check_overlap = TRUE) +
  labs(x = "Population-weighted patronage potential (people equivalents)",
    y = "Observed sales (millions of source currency units)",
    title = "Modelled patronage potential versus observed sales",
    subtitle = sprintf("28 stores | Pearson r = %.3f | p = %.3g",
      unname(sales_test$estimate), sales_test$p.value)) + theme_minimal(base_size = 12)

## ---- i8-dominant
# Largest probability wins; stable alphabetical order resolves exact ties.
huff_TA <- huff_share %>% group_by(VILLCODE) %>%
  arrange(desc(p_ij), Store_CD, .by_group = TRUE) %>%
  summarise(TA = first(Store_CD), winning_probability = first(p_ij),
    probability_margin = first(p_ij) - nth(p_ij, 2),
    tied_maximum = sum(abs(p_ij - max(p_ij)) < 1e-12), .groups = "drop")
store_codes <- sort(stores$Store_CD)
village_TA <- village %>% left_join(huff_TA, by = "VILLCODE") %>%
  mutate(TA = factor(TA, levels = store_codes))
# One named colour per store; never collapse the 28 categories into bins.
ta_colours <- setNames(cols4all::c4a("glasbey", 28), store_codes)

## ---- i8-ta-map
ta_map <- tm_shape(village_TA) +
  tm_polygons(fill = "TA", col = "grey25", lwd = 0.55,
    fill.scale = tm_scale_categorical(values = ta_colours,
      levels = store_codes, levels.drop = FALSE),
    fill.legend = tm_legend(title = "TA", text.size = 0.75,
      position = tm_pos_out("right", "center"), frame = TRUE)) +
  tm_title("Trade Areas of stores", size = 1.2) +
  tm_layout(frame = TRUE, bg.color = "white", legend.outside.size = 0.12) +
  tm_compass(type = "8star", size = 2, position = c("right", "bottom")) +
  tm_scalebar(position = c("left", "bottom")) +
  tm_grid(alpha = 0.2)

## ---- i8-results
store_summary <- st_drop_geometry(store_sales_est) %>%
  select(Store_CD, Store_Name, Total_Bill, Total_Sales, sum_E_j) %>%
  left_join(huff_TA %>% count(TA, name = "dominant_villages"),
    by = c("Store_CD" = "TA")) %>% arrange(desc(sum_E_j))
run_summary <- data.frame(metric = c("input_villages", "matched_villages",
  "excluded_villages", "unmatched_population_rows", "stores", "od_pairs",
  "population", "min_distance_m", "max_distance_m", "legend_levels",
  "pearson_r", "pearson_p", "tied_origins"),
  value = c(nrow(village_all), nrow(village), nrow(excluded_villages),
    nrow(unmatched_population), nrow(stores), nrow(distmat_long),
    sum(village$P_CNT), min(distmat_long$distance), max(distmat_long$distance),
    nlevels(village_TA$TA), unname(sales_test$estimate), sales_test$p.value,
    sum(huff_TA$tied_maximum > 1)))
dir.create("inclass8/results", showWarnings = FALSE, recursive = TRUE)
write_csv(store_summary, "inclass8/results/store-summary.csv")
write_csv(run_summary, "inclass8/results/run-summary.csv")
write_csv(st_drop_geometry(village_TA) %>% select(VILLCODE, TOWNNAME, VILLNAME,
  TA, winning_probability, probability_margin, P_CNT),
  "inclass8/results/village-trade-areas.csv")
write_csv(excluded_villages, "inclass8/results/excluded-villages.csv")
write_csv(unmatched_population, "inclass8/results/unmatched-population.csv")
write_csv(data.frame(Store_CD = store_codes, colour = unname(ta_colours)),
  "inclass8/results/legend-colours.csv")
writeLines(capture.output(sessionInfo()), "inclass8/results/session-info.txt")
