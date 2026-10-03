# Hunan data for in-class6

The time series `aspatial/Hunan_GDPPC.csv` is copied without modification from
the user's `C:/zhenhuaArhci/my_Project/data/aspatial/Hunan_GDPPC.csv`.
It contains county GDP per capita observations for 2005–2021.

The analysis reuses the tracked Hunan shapefile at `handson4/data/geospatial`.
Those files match the user's supplied Hunan shapefile. County boundaries are
treated as fixed throughout the time series; no boundary harmonisation or
inflation adjustment is introduced.

Teaching context: https://r4gdsa.netlify.app/chap11#the-data
Course data repository:
https://github.com/tskam/ISSS626-AY2026-27Aug/tree/master/lesson/Lesson04/data

Run `source("in-class6/R/analysis.R")` from the Quarto project root. Required
packages: sf, sfdep, spdep, tmap, dplyr, tidyr, readr, ggplot2, Kendall,
plotly and knitr. Recorded versions are in `in-class6/results/package-versions.csv`.
Seed 1234 is set for each permutation analysis; annual calculations use one worker.
