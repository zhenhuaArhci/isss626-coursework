# Hands-on Exercise 7: data provenance

The teaching inputs are from Prof. Kam's public `tskam/IS415` repository,
`Hands-on_Ex/Hands-on_EX11/data`, pinned to revision
`acaa1f9995d3d778a02a09c6bbdd5ef4f4eae773`:
https://github.com/tskam/IS415/tree/acaa1f9995d3d778a02a09c6bbdd5ef4f4eae773/Hands-on_Ex/Hands-on_EX11/data

Retrieved on 17 October 2026. Original inputs remain in the ignored `raw/`
directory. Run `source("handson7/R/download-data.R")` from the project root
to download them; `input-checksums.csv` verifies every file by MD5.

- `MP14_SUBZONE_NO_SEA_PL`: URA Master Plan 2014 subzones (323 polygons).
- `hexagons`: prepared teaching hexagons (3,125 cells, 250 m radius).
- `ELDERCARE`: historical teaching eldercare locations (120 points).
- `OD_Matrix.csv`: network distances in metres, one record per origin/destination
  pair, including entry, network, exit and total costs.

The chapter identifies the hexagons, eldercare layer and OD matrix as specially
prepared teaching/research materials and requests permission for other uses.
This page uses them to complete the linked hands-on exercise. Raw teaching
inputs are not redistributed in this repository or website. See the course
handout for its stated usage conditions: https://r4gdsa.netlify.app/chap17.html

The facility inventory and boundaries are historical, not a current service
directory. Capacity and demand are illustrative constants of 100, not observed
service capacity or elderly population. Road distances are supplied by the
teaching dataset; the road network date and routing settings are unspecified.
