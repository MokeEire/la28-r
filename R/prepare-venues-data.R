library(here)
library(tidyverse)
library(tidygeocoder)
library(janitor)
library(googlesheets4)
library(sf)
library(tidytransit)
source(here("R", "fct-prep.R"))

# Load Data ---------------------------------------------------------------

venues_drive_link = "https://docs.google.com/spreadsheets/d/18_P4igWVdnWZpbXPb7ZHEw6vscrD8gNAeu0BfDXmE4k/edit?gid=0#gid=0"
venues = read_sheet(venues_drive_link, sheet = "Venues") |> 
  clean_names() |> 
  # Separate sources into list column
  mutate(sources = str_split(source_s, ";\\\n"))

# Geocode ----------------------------------------------------------------

venues_geo = venues |> 
  geocode(address, method = 'osm', lat = latitude , long = longitude)

la_venues_geo = venues_geo |> 
  filter(in_los_angeles)



venues_sf = st_as_sf(la_venues_geo, coords = c("longitude", "latitude"), crs="EPSG:4326", sf_column_name = "venue_geometry") |> 
  # combine events for the venue
  group_by(venue_simplified) |> 
  mutate(events = str_c(events, collapse = ", "),
         venues = str_c(venue, collapse = ", ")) |> 
  ungroup() |> 
  distinct(venue_simplified, venues, address, cluster, events, venue_geometry)

venues_sf |> 
  # combine events for the venue
  group_by(venue_simplified) |> 
  mutate(events = str_c(events, collapse = ", "),
         venues = str_c(venue, collapse = ", ")) |> 
  ungroup() |> 
  distinct(venue_simplified, venues, address, cluster, events, venue_geometry) |> glimpse()

# Combine GTFS Data -------------------------------------------------------

# Metro Rail
# Load rail stops
metro_gtfs = read_gtfs("https://gitlab.com/LACMTA/gtfs_rail/raw/master/gtfs_rail.zip")

(rail_sf = gtfs_as_sf(metro_gtfs)) |> 
  glimpse()

rail_stops = rail_sf$stops |> 
  filter(location_type == 0)

# Metrolink
# https://metrolinktrains.com/about/gtfs/
metrolink_gtfs = read_gtfs("https://metrolinktrains.com/globalassets/about/gtfs/gtfs.zip")

metrolink_sf = gtfs_as_sf(metrolink_gtfs)

stops_combined = list_rbind(
  list(rail_stops, metrolink_sf$stops)
)

venues_metro = venues_sf |> 
  mutate(metro_stop_distances = map2(venue_simplified, venue_geometry, \(venue, venue_geometry) calculate_stop_distances(venue, venue_geometry, stops_combined)),
         metro_nearest_stop = map(metro_stop_distances, \(stops) filter(stops, distance == min(distance))),
         metro_nearby_stops = map(metro_stop_distances, \(stops) filter(stops, distance < 1000)),
         metro_nearby_routes = map(metro_nearby_stops, \(stops) add_routes(stops, metro_gtfs)))

# Output ------------------------------------------------------------------

write_csv(la_venues_geo, here("data", "la_venues_geo.csv"))
write_rds(la_venues_geo, here("data", "la_venues_geo.rds"))

# To RDS
write_rds(venues_metro, here("data", "venues_complete.rds"))

# To JSON
jsonlite::toJSON(venues_metro) |> 
  write_lines(here("data", "venues_complete.json"))
