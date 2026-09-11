# Data used in Hands-on Exercise 1B

Run `R/download-data.R` from the repository root to create the ignored
`data/raw` directory and download the source files used by the analysis.

- Master Plan 2019 Subzone Boundary (No Sea): data.gov.sg
- Singapore Residents by Planning Area / Subzone, Age Group, Sex and Type of
  Dwelling, June 2024: Singapore Department of Statistics

The official 2019 boundary is now distributed as GeoJSON, so its planning-area
and subzone attributes can be read directly. The original lesson's HTML-in-KML
tidying step is documented in the page but is not needed for this current file.
