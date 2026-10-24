# Teaching inputs and provenance

Method: [Kam Tin Seong, R for Geospatial Data Science and Analytics, Chapter 18](https://r4gdsa.netlify.app/chap18).

The chapter's GitHub source excludes its `data/` directories. The same named teaching inputs are available in the public [andrewhuangk/Geospatial data mirror](https://github.com/andrewhuangk/Geospatial/tree/78e7216992f8fc46abaff599de4f15224805db57/Hands-on_Ex/Hands-on_Ex08/data), pinned to commit `78e7216992f8fc46abaff599de4f15224805db57`, retrieved 24 October 2026. Only input data are downloaded; this exercise's code and explanations were written separately. No general redistribution licence was found, so raw inputs are excluded from this repository and website.

`R/download-data.R` fetches 18 files and checks their MD5 values against `input-checksums.csv`:

- STORES_TC: 28 store points, including store code and name.
- TOWN_TC: 28 town polygons.
- VILLAGE_TC: 622 village polygons.
- pop2019.csv: 625 population records, December 2019 (`108Y12M`).
- Store_sales_data.csv: 5,499 sales records; the included 28 stores are joined by code. The source does not establish a sales observation period or currency metadata.
- storeCD.csv: supplementary code lookup, retained for provenance; not needed to calculate the model.

All geometry uses TWD97 / TM2 zone 121 (EPSG:3826), in metres. The tutorial's overview mentions `_TN` but its import code, map extent and actual files use `_TC` (Taichung); this exercise uses `_TC` consistently.

After removing hyphens from V_ID, five geometry records lack population matches; all five have special `S` codes. Eight population rows have no matching geometry. The retained model has 617 origins, 28 destinations and 17,276 pairs. Both exclusion lists are exported in `results/`; unmatched villages are omitted, never assigned zero population.
