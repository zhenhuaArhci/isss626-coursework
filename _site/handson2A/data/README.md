# Shared data for Hands-on Exercises 2A and 2B

Run `source("handson2A/R/download-data.R")` from the website root. The script
uses the official data.gov.sg download API, validates GeoJSON and caches it in
`handson2A/data/raw/`. Raw files and reproducible derived files are ignored by
Git and are not needed by the deployed static website.

| Local filename | Source | Coverage | Features | Bytes | MD5 |
|---|---|---|---:|---:|---|
| `childcare.geojson` | [ECDA Child Care Services](https://data.gov.sg/datasets/d_5d668e3f544335f8028f546827b773b4/view) | December 2021 | 1,925 | 1,488,543 | `3cdf1df41203fc05270d283bb1ed1484` |
| `mp19-subzone.geojson` | [URA MP2019 Subzone Boundary (No Sea)](https://data.gov.sg/datasets/d_8594ae9ff96d0c708bc2af633048edfb/view) | Master Plan 2019 | 332 | 3,180,569 | `86bd781ffe03ce9e804fe6701bac24cb` |

The childcare input was obtained on 19 September 2026. The boundary input is
the existing official download already used in Exercise 1B; its checksum was
recorded on 19 September 2026. The download script can retrieve the same layer
for a fresh checkout. A portal refresh date is not an observation date.
Reuse is subject to the [Singapore Open Data Licence](https://data.gov.sg/open-data-licence).
Compare hashes before claiming numerical reproduction after a future download.

## Preparation decisions

- Both original layers use WGS 84. Transform to EPSG:3414 before planar work.
- Validate polygon geometry, exclude the course's Southern Group, Western
  Islands and North-Eastern Islands, and dissolve the remaining 327 subzones.
- The retained window covers about 669.52 km². All 1,925 records fall inside it.
- There are 1,738 distinct coordinates. The 187 additional co-located records
  are retained for service-record intensity in 2A. They are collapsed for the
  principal simple site-pattern analysis in 2B, with an explicit sensitivity check.
- The four planning-area counts are Punggol 72 records/65 sites, Tampines
  117/105, Choa Chu Kang 74/67 and Jurong West 110/99.
- Do not parse an HTML Description field: current GeoJSON attributes are
  already separate columns. Do not reinterpret these data as preschool capacity.

## Execution settings

- Main KDE: Gaussian kernel, Diggle bandwidth fitted to these data, edge
  correction, 100 m square target cells. Local KDE uses 50 m cells.
- Metre-coordinate raster values are multiplied by 1e6 for records/km²;
  EPSG:3414 is assigned only to a raster with metre coordinates.
- National Clark–Evans simulations: seed 20260919, 999 simulations, clustered
  alternative. Local comparisons: seed 20260920, two-sided alternative.
- Second-order site simulations: seed 20260921, 199 fixed-count uniform
  patterns per area, 129 distances over 0–500 m, reused for G/F/K/L.
- G/F use Kaplan–Meier; K/L use Ripley correction. Pointwise envelopes use
  `nrank = 5`. Maximum-deviation p-values are Holm-adjusted across eight tests.
- Record-based L sensitivity: seed 20260922, 199 fixed-count simulations.

The QMD documents print `sessionInfo()`. Main validation used R 4.5.3,
spatstat.geom 3.8-3, spatstat.explore 3.8-2, spatstat.random 3.5-1 and terra 1.9.50.
Existing pages retain their Quarto freeze mechanism. Exercise 2A reuses its
fully executed snapshot; explicitly render that QMD to refresh changed R code.
Exercise 2B executes on each build. Validation reran the existing pages with
their previous frozen outputs set aside. Second-order simulations use 199
realisations for practical runtime, with Monte Carlo resolution 0.005.

Derived files (`subzones-clean.rds` and the 2B `csr-tests.csv`) are regenerated
from source and excluded from version control. Rendering does not require a
previously generated 2A object to execute 2B.
