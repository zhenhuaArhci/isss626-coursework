library(sf)
library(tidyverse)
library(tmap)

data_dir <- "hands-on-exercise-1b/data/raw"
population_files <- list.files(
  file.path(data_dir, "singstat-population-2024"),
  pattern = "[.]csv$",
  recursive = TRUE,
  full.names = TRUE
)
population_file <- population_files[
  !startsWith(basename(population_files), "Notes_")
][1]

mpsz2019 <- st_read(file.path(data_dir, "mp19-subzone.geojson"), quiet = TRUE) |>
  st_make_valid() |>
  st_transform(3414)

population_raw <- read_csv(population_file, show_col_types = FALSE)

young_groups <- c("0_to_4", "5_to_9", "10_to_14", "15_to_19")
working_groups <- c(
  "20_to_24", "25_to_29", "30_to_34", "35_to_39", "40_to_44",
  "45_to_49", "50_to_54", "55_to_59", "60_to_64"
)
aged_groups <- c("65_to_69", "70_to_74", "75_to_79", "80_to_84",
                 "85_to_89", "90_and_over")

population2024 <- population_raw |>
  group_by(PA, SZ, AG) |>
  summarise(Pop = sum(Pop, na.rm = TRUE), .groups = "drop") |>
  group_by(PA, SZ) |>
  summarise(
    YOUNG = sum(Pop[AG %in% young_groups]),
    ECONOMY_ACTIVE = sum(Pop[AG %in% working_groups]),
    AGED = sum(Pop[AG %in% aged_groups]),
    TOTAL = sum(Pop),
    .groups = "drop"
  ) |>
  mutate(
    DEPENDENCY = if_else(
      ECONOMY_ACTIVE > 0,
      100 * (YOUNG + AGED) / ECONOMY_ACTIVE,
      NA_real_
    ),
    PA_KEY = str_to_upper(str_squish(PA)),
    SZ_KEY = str_to_upper(str_squish(SZ))
  )

mpsz_pop2024 <- mpsz2019 |>
  mutate(
    PA_KEY = str_to_upper(str_squish(PLN_AREA_N)),
    SZ_KEY = str_to_upper(str_squish(SUBZONE_N))
  ) |>
  left_join(population2024, by = c("PA_KEY", "SZ_KEY"))

results_1b <- list(
  boundaries = mpsz2019,
  population_raw = population_raw,
  population_summary = population2024,
  map_data = mpsz_pop2024,
  unmatched_population = anti_join(
    population2024,
    st_drop_geometry(mpsz2019) |>
      transmute(
        PA_KEY = str_to_upper(str_squish(PLN_AREA_N)),
        SZ_KEY = str_to_upper(str_squish(SUBZONE_N))
      ),
    by = c("PA_KEY", "SZ_KEY")
  )
)
