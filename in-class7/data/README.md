# In-class Exercise 7: input provenance

The submitted analysis uses the same historical teaching layers as Hands-on Exercise 7:
323 Master Plan 2014 subzones, 3,125 hexagons, 120 eldercare points and 375,000 road-network origin-destination pairs.

Source: Prof. Kam Tin Seong's [IS415 teaching repository](https://github.com/tskam/IS415/tree/acaa1f9995d3d778a02a09c6bbdd5ef4f4eae773/Hands-on_Ex/Hands-on_EX11/data), pinned at revision `acaa1f9995d3d778a02a09c6bbdd5ef4f4eae773`.
The facility records are historical (2016). Network date and routing settings are undocumented.
Demand and capacity are synthetic constants of 100, not observed population or service places.

Run `source("in-class7/R/download-data.R")` from the coursework project root.
The script downloads only missing inputs and checks all 15 files against the tracked MD5 manifest.
Raw data stays local under `in-class7/data/raw/`, excluded from Git and public site resources.
The total OD cost is in metres; accessibility matrices convert it to kilometres.

The accompanying analysis uses catchment revision `b96fb58adcd0e476cbb634aeb6ce27981fbc1dc3` and records package versions in the page's session information.
