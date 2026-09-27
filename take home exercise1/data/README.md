# Source data and acquisition

Run `source("take home exercise1/R/download-data.R")` from the website project root. The R helper downloads or verifies the original inputs using the SHA-256 manifest in `metadata/downloads.json`. It stops on a changed file. Acquisition date: 27 September 2026.

## Accident records

- Source: thaweewatboy, [Thailand Road Accident 2019–2022](https://www.kaggle.com/datasets/thaweewatboy/thailand-road-accident-2019-2022), Kaggle.
- License: **CC0: Public Domain**, as listed in `metadata/kaggle.json`.
- Last update listed: 19 August 2023.
- Files: original CSV and Parquet plus their download archive. Analysis uses the CSV.
- Nationwide source: 81,735 records. Preparation retains incident year 2022 and spatially intersects valid points with the study polygon.
- Incident dates are interpreted as written without an unverified timezone conversion.
- All 189 ungeolocated 2022 records have a Bangkok province label and the expressway reporting agency. The analysis records this limitation rather than imputing point locations.

If the public endpoint is unavailable, obtain the original archive from Kaggle and place it in `raw/` using the manifest filename. Rerun the downloader to verify it. Do not replace a reference hash to conceal a version mismatch.

## Administrative boundaries

- Provider: geoBoundaries gbOpen THA ADM1.
- Boundary ID: THA-ADM1-36821470; metadata vintage: **2017**.
- Upstream sources: OpenStreetMap contributors and Wambacher.
- License: [Open Data Commons Open Database License 1.0](https://opendatacommons.org/licenses/odbl/1-0/).
- [Pinned GeoJSON revision](https://github.com/wmgeolab/geoBoundaries/raw/9469f09/releaseData/gbOpen/THA/ADM1/geoBoundaries-THA-ADM1.geojson).
- Original metadata: `metadata/geoboundaries-adm1.json`.
- Selected ISO codes: TH-10, TH-12, TH-13, TH-11, TH-74, TH-73.

The six polygons are validated and transformed to EPSG:32647. Their union defines the window. Maps attribute the boundary sources; no edited boundary database is redistributed. This layer is not a road network.

## Storage and outputs

Raw inputs and regenerated intermediate tables are ignored by Git and excluded from the website. The committed scripts, metadata, tables printed in the report and PNG figures allow the published analysis to be inspected and regenerated. `data/derived/summary.json` records numerical results after each run; the report supplies the full computational context.

No road dataset is used. The analysis and its distance thresholds are planar; Chapter 7 and spNetwork are cited to explain the support and validation needed for a network-based extension.
