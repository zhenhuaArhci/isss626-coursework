# Run from the website project root. Labelled sections also appear in the report.
## ----thx-packages
library(sf)
library(dplyr)
library(readr)
library(ggplot2)
library(spatstat.geom)
library(spatstat.explore)
library(spatstat.random)
theme_set(theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = 'bold', colour = '#203754'),
        panel.grid.minor = element_blank(), plot.background = element_rect(fill='white',colour=NA)))
ex_dir <- 'take home exercise1'
fig_dir <- file.path(ex_dir, 'figures')
out_dir <- file.path(ex_dir, 'data/derived')
dir.create(out_dir, recursive=TRUE, showWarnings=FALSE)
save_plot <- function(name, plot, width=10, height=7) {
  ggsave(file.path(fig_dir,paste0(name,'.png')),plot,width=width,height=height,dpi=180,bg='white')
}

## ----thx-import
raw <- read_csv(file.path(ex_dir,'data/raw/thai_road_accident_2019_2022.csv'),
  col_types=cols(incident_datetime=col_character(), report_datetime=col_character()),
  show_col_types=FALSE)
boundaries <- st_read(file.path(ex_dir,'data/raw/geoBoundaries-THA-ADM1.geojson'),quiet=TRUE)
stopifnot(nrow(problems(raw)) == 0)
raw$incident_date <- as.Date(substr(raw$incident_datetime,1,10))
stopifnot(!anyNA(raw$incident_date))

## ----thx-quality
d2022 <- filter(raw, incident_date >= as.Date('2022-01-01'), incident_date < as.Date('2023-01-01'))
unique_records <- distinct(d2022)
# A duplicated ID with conflicting attributes requires investigation, not an arbitrary drop.
stopifnot(!anyNA(unique_records$acc_code), !anyDuplicated(unique_records$acc_code))
valid <- unique_records |> filter(is.finite(longitude),is.finite(latitude),
  between(longitude,-180,180),between(latitude,-90,90))
plausible <- valid |> filter(between(longitude,97,106),between(latitude,5,21))
audit <- data.frame(stage=c('Original records, all years','Incident year 2022',
  'After removing identical rows','Valid global coordinates','Plausible Thailand coordinates'),
  retained=c(nrow(raw),nrow(d2022),nrow(unique_records),nrow(valid),nrow(plausible)))

## ----thx-window
iso6 <- c('TH-10','TH-12','TH-13','TH-11','TH-74','TH-73')
provinces <- boundaries[match(iso6,boundaries$shapeISO),] |>
  st_make_valid() |> st_transform(32647)
provinces$province <- sub(' Province$','',provinces$shapeName)
stopifnot(nrow(provinces)==6, all(st_is_valid(provinces)))
study_window <- st_union(provinces)
points_all <- st_as_sf(plausible,coords=c('longitude','latitude'),crs=4326,remove=FALSE) |> st_transform(32647)
hits <- st_intersects(points_all,provinces)
points <- points_all[lengths(hits)>0,]
# Shared-boundary points use the first province in the declared ISO order.
points$spatial_province <- provinces$province[vapply(hits[lengths(hits)>0],`[`,integer(1),1)]
points$fatal <- points$number_of_fatalities > 0
stopifnot(!anyNA(points$fatal),all(points$number_of_fatalities>=0),
  !anyNA(points$number_of_injuries),all(points$number_of_injuries>=0))
audit <- rbind(audit,data.frame(stage='Within six-province study window',retained=nrow(points)))
audit$removed_from_previous <- c(NA,-diff(audit$retained))
xy <- st_coordinates(points)
loc_key <- paste(points$longitude,points$latitude,sep=',')
points$location_id <- match(loc_key,unique(loc_key))
quality <- list(n_total=nrow(raw),n_2022=nrow(d2022),n=nrow(points),
  area_km2=as.numeric(st_area(study_window))/1e6,
  unique_locations=length(unique(loc_key)),repeated_extra=sum(duplicated(loc_key)),
  repeated_sites=sum(table(loc_key)>1),max_at_site=max(table(loc_key)),
  fatalities=sum(points$number_of_fatalities),fatal_events=sum(points$fatal),
  injuries=sum(points$number_of_injuries),
  province_mismatch=sum(points$province_en!=points$spatial_province),
  labelled_six=sum(d2022$province_en %in% provinces$province),
  labelled_six_invalid=sum(unique_records$province_en %in% provinces$province &
    (!is.finite(unique_records$latitude)|!is.finite(unique_records$longitude))),
  shared_boundary=sum(lengths(hits)>1))
missing_summary <- unique_records |>
  filter(!is.finite(longitude)|!is.finite(latitude)) |>
  count(province_en,agency,sort=TRUE)
label_audit <- data.frame(
  category=c('Label in study area; located inside','Label in study area; located outside',
             'Label outside study area; located inside','Label in study area; missing coordinates'),
  events=c(sum(points$province_en %in% provinces$province),
    sum(plausible$province_en %in% provinces$province)-sum(points$province_en %in% provinces$province),
    sum(!points$province_en %in% provinces$province),quality$labelled_six_invalid))
province_summary <- points |> st_drop_geometry() |> group_by(spatial_province) |>
  summarise(events=n(),fatal_events=sum(fatal),fatalities=sum(number_of_fatalities),
    injuries=sum(number_of_injuries),.groups='drop')
province_summary$area_km2 <- as.numeric(st_area(provinces[match(province_summary$spatial_province,provinces$province),]))/1e6
province_summary$events_per_km2 <- province_summary$events/province_summary$area_km2
agency_summary <- points |> st_drop_geometry() |> count(agency,sort=TRUE)
write_csv(audit,file.path(out_dir,'audit.csv'))
write_csv(province_summary,file.path(out_dir,'province-summary.csv'))
write_csv(agency_summary,file.path(out_dir,'agency-summary.csv'))
write_csv(missing_summary,file.path(out_dir,'missing-coordinate-summary.csv'))
write_csv(label_audit,file.path(out_dir,'label-audit.csv'))

## ----thx-study-map
labels <- st_point_on_surface(provinces)
overview <- ggplot() + geom_sf(data=provinces,fill='#eef2f5',colour='#718093',linewidth=.4) +
  geom_sf(data=points,aes(colour=fatal),size=.65,alpha=.6) +
  scale_colour_manual(values=c('FALSE'='#217c91','TRUE'='#ce3b3b'),
    labels=c('Non-fatal recorded accident','At least one recorded death'),name=NULL) +
  geom_sf_text(data=labels,aes(label=province),size=3.2,fontface='bold',check_overlap=TRUE) +
  labs(title='Recorded accidents follow metropolitan corridors',
    subtitle=paste(format(nrow(points),big.mark=','),'events within the six-province boundary, 2022'),
    caption='Kaggle: Thai road accidents | geoBoundaries ADM1 (2017) | WGS 84 / UTM 47N; graticule in degrees') +
  theme(legend.position='bottom',axis.title=element_blank())
save_plot('study-area',overview)
print(audit); print(quality); print(province_summary); print(agency_summary)
