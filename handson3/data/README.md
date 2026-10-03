# Inputs for Hands-on Exercise 3

Chapter: https://r4gdsa.netlify.app/chap06

The chapter identifies NASA FIRMS MODIS detections for 2023 and an Indonesian
administrative-boundary extract. Its original raw files are absent from the
instructor's public book repository. This exercise acquires the classroom data
from a public mirror, not from another student's analysis or written results:

https://github.com/endurrus/IS415-GAA/tree/0f695aaacf44676f3d82aff6747873ce614168bb/In-Class_Ex/In-Class_Ex04/data/rawdata

Pinned revision: `0f695aaacf44676f3d82aff6747873ce614168bb`.
Retrieved: 3 October 2026. Only the CSV and shapefile components are downloaded.
The boundary shapefile's Git blob SHA matches a second classroom mirror,
`ryanpxp/IS415-GAA` (`6fbf5146e40d06d54b09bdd5d50865b0c3d9812a`).
The local input checksums are recorded in `input-checksums.csv`.

Original source context:

- NASA FIRMS: https://firms.modaps.eosdis.nasa.gov/
- Boundary portal named by the chapter: https://www.indonesia-geospasial.com/

Actual coverage is the Bangka study area, not all Bangka Belitung province.
The boundary has 298 features and dissolves into two polygon parts. The main
part contains all 741 fire-detection records; the small offshore part has none.
No values or locations are simulated to replace the original observations.
All CSV detections are MODIS observations and have type code 0.

Run `source("handson3/R/download-data.R")` from the project root to acquire
missing inputs, then `source("handson3/R/analysis.R")`. Raw files are tracked
in GitHub but not published in the Netlify site. Numerical `.rds` caches under
`data/derived` are local and excluded from Git. Cache keys include input MD5
hashes, package versions, and numerical settings. CSV results and package
versions are published under `results`.
