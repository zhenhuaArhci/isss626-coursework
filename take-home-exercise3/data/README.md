# Data sources and provenance

| Source | Input used | Notes |
| --- | --- | --- |
| [LTA DataMall static datasets](https://datamall.lta.gov.sg/content/datamall/en/static-data.html) | TrainStation_Sep2026 and BusStopLocation_Sep2026 archives | Station polygons provide centroids. Bus stops are deduplicated by operating stop code. LTA Singapore Open Data Licence. |
| [LTA Passenger Volume by Train Stations](https://datamall.lta.gov.sg/content/datamall/en/dynamic-data.html) via [Udit Narayan Mall’s public station demand map](https://github.com/uditmall/Singapore-MRT-LRT-station-passenger-volume/tree/90960b2dd1270f9691522ae4df31226be2552dc5) | August, September and October 2026, aggregated hourly means by station and day type | LTA source data made available under Singapore Open Data Licence v1.0. August, September and October were accessed by the publisher on 30 Sep, 10 Oct and 10 Nov 2026. Daily totals are reconstructed from the mirror’s rounded means, with weekdays and weekends/public holidays weighted by the accompanying day counts. This is the station-volume product. The grid-to-station interaction tables are model outputs. |
| [Singapore train station coordinates](https://github.com/elliotwutingfeng/singapore_train_station_coordinates/tree/9f5972c71db8a529801eee9c527946a45a78e409) | Future and operational coordinates, at the pinned revision | See the project’s dataset licence and per-row source. Proposed station coordinates are indicative. |
| [WorldPop Global2 counts](https://hub.worldpop.org/project/categories?id=3) | Singapore 2025, Global2 R2025A v1 constrained count raster at 100 m nominal resolution | Positive population counts are summed by projected 500 m cell. WorldPop data are available under CC BY 4.0. |
| [HDB Existing Building](https://data.gov.sg/datasets/d_16b157c52ed637edd6ba1232e026258d/view) | November 2026 retrieval | Building features are counted within 800 m of each station. Singapore Open Data Licence. |
| [URA Master Plan 2019 Planning Area Boundary, land only](https://data.gov.sg/datasets/d_4765db0e87b9c86336792efe8a1f7a66/view) | November 2026 retrieval of the 2019 layer | Planning-area aggregation and regional holdout. Singapore Open Data Licence. |
| [LTA Jurong Region Line](https://www.lta.gov.sg/content/ltagov/en/upcoming_projects/rail_expansion/jurong_region_line.html) and [Cross Island Line](https://www.lta.gov.sg/content/ltagov/en/upcoming_projects/rail_expansion/cross_island_line.html) | Scenario context | Includes JRL with Forest Hill and CRL phases 1–2 plus the Punggol Extension. The scenario excludes CRL phase 3, Changi Terminal 5, and other rail extensions. |

The [input manifest](input-checksums.csv) records the files and MD5 checksums for the published run. The data.gov.sg API resolves temporary download links when the script runs. These datasets can be revised upstream. The JS aggregate files are decoded as JSON, with no execution of third-party JavaScript.

## Join keys and units

- `origin` identifies a 500 m SVY21 grid square. `x` and `y` give its population-weighted centre in metres. `market_population` is an estimate of residents.
- `destination` identifies a station complex. Slashes separate multiple service codes that share one facility.
- `distance_m` gives straight-line distance from an origin centre to a station. `probability` is the fitted Huff choice share.
- `observed_train` and `observed_test` are observed average daily tap-ins. `baseline_taps` and `expanded_taps` are scenario estimates at the same October-equivalent system total.
- `baseline_choice` and `expanded_choice` identify the largest model probability per origin. `probability_new` sums all new-station probabilities.
