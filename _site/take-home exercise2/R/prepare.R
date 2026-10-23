## ---- th2-packages
library(sf)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(spdep)
source("take-home exercise2/R/methods.R")
base_dir <- "take-home exercise2"
for (p in c("data/raw", "data/derived", "results", "figures"))
  dir.create(file.path(base_dir,p), recursive = TRUE, showWarnings = FALSE)

## ---- th2-import
# For an authorized ACLED copy, set ACLED_MYANMAR_FILE to a CSV or ZIP,
# or leave the supplied archive in data/raw or your Downloads folder.
find_input <- function(env_name, filename, local_name = filename) {
  candidates <- c(Sys.getenv(env_name), file.path(base_dir,"data/raw",local_name),
                  file.path(path.expand("~"),"Downloads",filename),
                  file.path(Sys.getenv("USERPROFILE"),"Downloads",filename))
  ok <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (!length(ok)) stop("Missing input ", filename, ". See data/README.md.")
  normalizePath(ok[1], winslash="/", mustWork=TRUE)
}
acled_file <- find_input("ACLED_MYANMAR_FILE", "ACLED_Data_Myanmar_Jan2021-Sep2025.zip")
boundary_file <- find_input("MIMU_TOWNSHIP_FILE", "Myanmar Township Boundaries_MIMU_PCodev9.4.zip", "mimu-v94.zip")
inputs <- tibble(file=c(basename(acled_file),basename(boundary_file)),
                 bytes=file.info(c(acled_file,boundary_file))$size,
                 sha256=vapply(c(acled_file,boundary_file),digest::digest,character(1),algo="sha256",file=TRUE))
write_csv(inputs,file.path(base_dir,"data/input-checksums.csv"))
if (grepl("[.]zip$",acled_file,ignore.case=TRUE)) {
  csv_name <- unzip(acled_file,list=TRUE)$Name
  csv_name <- csv_name[grepl("[.]csv$",csv_name,ignore.case=TRUE)]
  stopifnot(length(csv_name)==1L)
  unzip(acled_file,files=csv_name,exdir=file.path(base_dir,"data/raw/acled"))
  acled_csv <- file.path(base_dir,"data/raw/acled",csv_name)
} else acled_csv <- acled_file
unzip(boundary_file,exdir=file.path(base_dir,"data/raw/boundaries"))
shape <- list.files(file.path(base_dir,"data/raw/boundaries"),pattern="[.]shp$",recursive=TRUE,full.names=TRUE)
stopifnot(length(shape)==1L)
townships_raw <- st_read(shape,quiet=TRUE)
stopifnot(nrow(townships_raw)==330L, !anyDuplicated(townships_raw$TS_PCODE),
          all(abs(townships_raw$PCode_V-9.4)<.001))
valid_before <- sum(st_is_valid(townships_raw))
# A national equal-area projection: metres for topology checks and area summaries.
mmr_crs <- "+proj=laea +lat_0=21 +lon_0=96 +datum=WGS84 +units=m +no_defs"
townships <- townships_raw |> select(TS_PCODE,TS,ST,DT) |>
  arrange(TS_PCODE) |> st_transform(mmr_crs) |> st_make_valid()
stopifnot(all(st_is_valid(townships)),!any(st_is_empty(townships)))
raw <- read_csv(acled_csv,show_col_types=FALSE,guess_max=100000,
  col_types=cols(event_id_cnty=col_character(),event_date=col_date(),.default=col_guess()))
stopifnot(nrow(problems(raw))==0L)

## ---- th2-clean
required <- c("event_id_cnty","event_date","event_type","latitude","longitude","geo_precision","fatalities","admin3")
stopifnot(all(required %in% names(raw)), !anyNA(raw$event_id_cnty))
duplicate_ids <- sum(duplicated(raw$event_id_cnty))
if (duplicate_ids) stop("Duplicate event IDs require an explicit resolution before analysis.")
period <- raw |> filter(country=="Myanmar",event_date>=as.Date("2021-01-01"),event_date<=as.Date("2025-09-30"))
armed_types <- c("Battles","Explosions/Remote violence","Violence against civilians")
armed <- period |> filter(event_type %in% armed_types)
stopifnot(!anyNA(armed$fatalities),all(armed$fatalities>=0))
coordinate_ok <- with(armed,is.finite(latitude)&is.finite(longitude)&latitude>=-90&latitude<=90&longitude>=-180&longitude<=180)
coord <- armed[coordinate_ok,]
precise <- coord |> filter(geo_precision %in% c(1,2))
points <- st_as_sf(precise,coords=c("longitude","latitude"),crs=4326,remove=FALSE) |> st_transform(mmr_crs)
# Geometric containment is authoritative; no offshore event is snapped onshore.
hits <- st_intersects(points,townships)
nhits <- lengths(hits)
# A boundary tie uses the matching ACLED township name if unique, then stable PCode.
picked <- vapply(seq_along(hits),function(i) {
  h <- hits[[i]]
  if (!length(h)) return(NA_integer_)
  named <- h[tolower(townships$TS[h])==tolower(precise$admin3[i])]
  if (length(named)==1L) named else h[1]
},integer(1))
precise$TS_PCODE <- townships$TS_PCODE[picked]
precise$polygon_TS <- townships$TS[picked]
precise$spatial_hits <- nhits
unmatched <- precise |> filter(is.na(TS_PCODE))
# A local audit contains event IDs and coordinates and is never published.
write_csv(unmatched |> select(event_id_cnty,admin1,admin3,latitude,longitude,event_type),
          file.path(base_dir,"data/derived/unmatched-events.csv"))
events <- precise |> filter(!is.na(TS_PCODE)) |>
  mutate(month=as.Date(format(event_date,"%Y-%m-01")),
         civilian=as.integer(event_type=="Violence against civilians"),
         label_match=tolower(admin3)==tolower(polygon_TS))
audit <- tibble(step=c("Source records","Unique event IDs","Myanmar in date range","Three armed event types",
  "Valid coordinates","Precision 1 or 2","Inside township polygons","Violence against civilians",
  "Precise location (code 1)","Multiple polygon hits","Township-label disagreements","Valid input polygons"),
  records=c(nrow(raw),n_distinct(raw$event_id_cnty),nrow(period),nrow(armed),nrow(coord),nrow(precise),
    nrow(events),sum(events$civilian),sum(events$geo_precision==1),sum(nhits>1),
    sum(!events$label_match,na.rm=TRUE),valid_before))
write_csv(audit,file.path(base_dir,"results/data-audit.csv"))
write_csv(period |> count(event_type,disorder_type,name="records"),file.path(base_dir,"results/event-types.csv"))
write_csv(armed |> count(geo_precision,name="records"),file.path(base_dir,"results/location-precision.csv"))
write_csv(precise |> mutate(matched=!is.na(TS_PCODE)) |> count(matched,geo_precision,event_type,name="records"),
          file.path(base_dir,"results/spatial-match-audit.csv"))

## ---- th2-cube
months <- seq(as.Date("2021-01-01"),as.Date("2025-09-01"),by="month")
counted <- events |> group_by(TS_PCODE,month) |> summarise(armed_events=n(),
  civilian_events=sum(civilian),fatalities=sum(fatalities),precise_events=sum(geo_precision==1),.groups="drop")
cube <- expand_grid(month=months,TS_PCODE=townships$TS_PCODE) |>
  left_join(counted,by=c("TS_PCODE","month")) |>
  mutate(across(c(armed_events,civilian_events,fatalities,precise_events),~replace_na(.x,0L))) |>
  left_join(st_drop_geometry(townships),by="TS_PCODE") |> arrange(month,TS_PCODE)
stopifnot(nrow(cube)==330L*57L,!anyDuplicated(cube[c("TS_PCODE","month")]),
          sum(cube$armed_events)==nrow(events),sum(cube$civilian_events)==sum(events$civilian),
          all(cube$civilian_events<=cube$armed_events),!anyNA(cube))
totals <- cube |> group_by(TS_PCODE) |> summarise(across(c(armed_events,civilian_events,fatalities,precise_events),sum),.groups="drop")
townships <- townships |> left_join(totals,by="TS_PCODE")
write_csv(cube,file.path(base_dir,"results/township-month-cube.csv"))
write_csv(st_drop_geometry(townships),file.path(base_dir,"results/township-totals.csv"))
write_csv(cube |> group_by(month) |> summarise(across(c(armed_events,civilian_events,fatalities),sum)),
          file.path(base_dir,"results/monthly-totals.csv"))
saveRDS(list(townships=townships,events=events,cube=cube,audit=audit),file.path(base_dir,"data/derived/prepared.rds"))
message("Prepared ",nrow(events)," armed events; ",nrow(cube)," complete township-month cells.")
