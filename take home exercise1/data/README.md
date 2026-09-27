# Source data and acquisition

Status: original inputs downloaded; student preparation and analysis pending.

## Acquire with R

Open the website RStudio project and run:

```r
source("take home exercise1/R/download-data.R")
```

This R script verifies existing files or downloads missing inputs. It checks
SHA-256 hashes against `metadata/downloads.json` and stops on mismatches.
It does not filter or analyse records. Inputs remain in the Git-ignored `raw/`
folder; do not commit raw or derived datasets without reviewing their licenses.
The original files were retrieved on 27 September 2026. Download timestamps do
not represent observation dates.

## Accident records

- Publisher: thaweewatboy, Kaggle.
- Dataset: [Thailand Road Accident 2019–2022](https://www.kaggle.com/datasets/thaweewatboy/thailand-road-accident-2019-2022).
- Portal metadata: [kaggle.json](metadata/kaggle.json).
- License listed by Kaggle at acquisition: **CC0: Public Domain**.
- Last update listed by Kaggle: 19 August 2023.
- Original files: `thai_road_accident_2019_2022.csv` and `.parquet`.
- Scope: nationwide source; the assignment requires a **2022** and Greater Bangkok
  restriction in the student's own preparation.
- Coverage, completeness, event semantics, and coordinate accuracy still need
  independent examination. Preserve the publisher's provenance and attribution.

Observed CSV columns:

`acc_code`, `incident_datetime`, `report_datetime`, `province_th`, `province_en`,
`agency`, `route`, `vehicle_type`, `presumed_cause`, `accident_type`,
`number_of_vehicles_involved`, `number_of_fatalities`, `number_of_injuries`,
`weather_condition`, `latitude`, `longitude`, `road_description`, `slope_description`.

If the public download endpoint becomes unavailable, download the original
archive through Kaggle, put it in `raw/`, and rerun the R acquisition script.
Do not overwrite the reference hashes to bypass a failed check.

## Administrative boundaries

- Provider: [geoBoundaries](https://www.geoboundaries.org/), gbOpen, THA ADM1.
- Boundary ID: `THA-ADM1-36821470`.
- Metadata vintage: **2017**.
- Upstream sources listed: OpenStreetMap and Wambacher.
- License listed: **Open Data Commons Open Database License 1.0**.
- [Original metadata](metadata/geoboundaries-adm1.json).
- [Version-pinned full-resolution GeoJSON](https://github.com/wmgeolab/geoBoundaries/raw/9469f09/releaseData/gbOpen/THA/ADM1/geoBoundaries-THA-ADM1.geojson).

The R downloader uses the version-pinned URL from the saved metadata. The student
must document the six-area selection, names, geometry checks, and suitability for
2022. Do not assume that the administrative layer also supplies a road network.
Follow the provider's attribution and license requirements for derived products.

## Road network

No road dataset has been selected or downloaded. If needed for the chosen method,
document source URL, extraction date, represented date, extent, included road
classes, topology treatment, attribution, and license. Consider the historical
fit to 2022 before using a current OpenStreetMap extract.

## Reproducibility

[downloads.json](metadata/downloads.json) records filenames, byte counts,
SHA-256 hashes, and original download timestamps. Input versions are fixed by
these records. Any intentional input update requires an explicit new provenance
record and a fresh analytical run. Keep original inputs unchanged; store student
intermediates in `data/derived/` and final R graphics in `figures/`.
