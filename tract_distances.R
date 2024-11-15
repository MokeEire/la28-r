library(jsonlite)
library(geojsonsf)
library(here)
library(tidyverse)
library(sf)

# Load Census data --------------------------------------------------------

tract_file = here("data", "2020_Census_Tracts.geojson")
tracts = geojson_sf(tract_file, crs = "EPSG:4326")

exclude_tracts = tribble(
  ~CT20,
  "599100",
  "599000",
  "930400"
)

tracts_df = tracts |> 
  anti_join(exclude_tracts, by = "CT20") |> 
  mutate(center = st_centroid(geometry),
         lat = st_coordinates(center)[,2]) |> 
  filter(st_coordinates(center)[,2] < 34.35)

tracts_df |> ggplot()+geom_sf(aes(geometry=geometry))+geom_sf(aes(geometry=center))
# Load Venues data --------------------------------------------------------

venues = read_rds(here("data", "la_venues_geo.rds"))
glimpse(venues)

(venues_sf = st_as_sf(venues, coords = c("longitude", "latitude"), crs="EPSG:4326", sf_column_name = "venue_geometry")) |> 
  glimpse()

# Calculate distance to venue for each tract
calculate_distances = function(venue, venue_geometry, tracts_df){
  # stops_within_dist = st_join(rail_stops_df, df, st_is_within_distance, left=F, dist = units::set_units(within_distance, "m"))
  # stops_list = list(stops_within_dist)
  # browser()
  # venue_geometry_only = select(df, venue, venue_geometry)
  venue_geometry_only = st_sf(
    venue = venue, 
    venue_geometry = st_sfc(venue_geometry), 
    crs="EPSG:4326"
  )
  browser()
  # Calculate distances between venue and stops
  distances = cross_join(venue_geometry_only, tracts_df) |> 
    mutate(distance = as.numeric(st_distance(venue_geometry, geometry, by_element=T))) |> 
    # Keep stops within distance or the minimum
    # filter(distance <= !!within_distance | distance == min(distance)) |> 
    select(-starts_with("venue")) |> 
    # For reasons I can't understand, this only removes venue geometry
    st_set_geometry(NULL)
  
  distances
  # Filter for the nearest stop
  # nearest_stop = stop_distances |> 
  #   filter(distance == min(distance))
  # 
  # # Add both sets of stops as list columns
  # mutate(df, 
  #        nearest_stop = list(nearest_stop),
  #        stops_within_1km = list(stop_distances))
}

tract_distances = venues_sf |> 
  cross_join(tracts_df) |> 
  mutate(distance = as.numeric(st_distance(venue_geometry, center, by_element=T)))

tract_distances |> 
  group_by(venue) |> 
  summarise(across(distance, list(mean = mean, sd = sd, min = min, max = max)))
