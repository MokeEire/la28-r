library(here)
library(tidyverse)
library(sf)
library(traveltimeR)
library(jsonlite)

# Load custom functions
source(here("R", "fct-traveltime.R"))

# Census tract shapefiles
nhgis_shp_file = here("data", "nhgis", "US_tract_2020.shp")

# Create list of tracts to exclude
exclude_tracts = tribble(
  ~TRACTCE, ~include,
  "599100", F, # Catalina Island
  "599000", F, # Catalina Island
  "930400", F,  # Angeles Forest
)

# Load the shapefile
nhgis_shp = read_sf(nhgis_shp_file) |> 
  # Filter to Los Angeles, CA
  filter(STATEFP == "06", COUNTYFP == "037") |>
  # Reproject CRS to WGS84
  st_transform(crs = "EPSG:4326") |> 
  # Create an opinionated subset of LA tracts
  # anti_join(exclude_tracts, by = "TRACTCE") |>
  # Remove tracts where the centroid is north of 34.35
  # filter(st_coordinates(st_centroid(geometry))[,2] < 34.35) |>
  select(GISJOIN, TRACTCE, GEOID, NAME, NAMELSAD, geometry) |> 
  left_join(exclude_tracts, by = "TRACTCE") |> 
  mutate(include = case_when(!is.na(include) ~ include,
                             st_coordinates(st_centroid(geometry))[,2] > 34.35 ~ F,
                             .default = T))

nhgis_la = nhgis_shp |> 
  filter(include) |> 
  select(-include)

nhgis_centroids = nhgis_la |> 
  # Find centroids of each tract
  mutate(centroid = st_centroid(geometry))


# Venues ------------------------------------------------------------------


venues = read_rds(here("data", "venues_complete.rds"))

venue_locations = venues |> 
  mutate(lat = st_coordinates(venue_geometry)[,2], 
         lng = st_coordinates(venue_geometry)[,1]) |> 
  st_drop_geometry() |>
  select(id = venue_location, lat, lng) |> 
  pmap(\(id, lat, lng) make_location(id = id, coords = list(lat = as.numeric(lat), lng = as.numeric(lng)))) |> 
  unlist(recursive=F)

tract_locations = nhgis_centroids |> 
  mutate(lat = st_coordinates(centroid)[,2], 
         lng = st_coordinates(centroid)[,1]) |> 
  st_drop_geometry() |>
  select(id = TRACTCE, lat, lng) |> 
  pmap(\(id, lat, lng) make_location(id = id, coords = list(lat = as.numeric(lat), lng = as.numeric(lng)))) |> 
  unlist(recursive=F)

locations_list = list_flatten(list(venue_locations, tract_locations))

tract_ids = map(tract_locations, "id")
  
travel_times = time_filter_to_df(venues = venue_locations, tract_ids = tract_ids, locations = locations_list, 0)

travel_times |> 
  select(id, venue, arrival_time, transportation, time_mins) |> 
  pivot_wider(id_cols = c(id, venue, arrival_time), names_from = transportation, values_from = c(time_mins), id_expand = T) |> 
  mutate(driving = driving*1.05, 
         public_transport = public_transport*.95) |>
  arrange(venue, id) |> 
  mutate(time_diff = public_transport - driving) |> 
  group_by(venue) |> 
  summarise(
    driving_avg = mean(driving, na.rm=T),
    transit_avg = mean(public_transport, na.rm=T),
    diff_avg = mean(time_diff, na.rm=T)
  )


# Output ------------------------------------------------------------------

# Save travel times file as: data/traveltime/travel_times_YYYY-MM-DD.rds

saveRDS(travel_times, here("data", "traveltime", str_c("travel_times_", today(),".rds")))
saveRDS(travel_times, here("data", "traveltime", str_c("travel_times_current.rds")))


