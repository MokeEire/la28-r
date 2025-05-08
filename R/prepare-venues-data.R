library(here)
library(tidyverse)
library(tidygeocoder)
library(janitor)
library(googlesheets4)
library(sf)
library(tidytransit)
source(here("R", "fct-prep.R"))

# Load Data ---------------------------------------------------------------

venues_drive_link = "https://docs.google.com/spreadsheets/d/18_P4igWVdnWZpbXPb7ZHEw6vscrD8gNAeu0BfDXmE4k"

venues = read_sheet(venues_drive_link, sheet = "Venues") |> 
  clean_names() |> 
  # Filter out non-LA venues
  filter(in_los_angeles) |> 
  # Separate sources into list column
  mutate(sources = str_split(source_s, ";\\\n"))

# Geocode ----------------------------------------------------------------

# I'm using arcGIS because the osm geocoder could not find Universal Studios
venues_geo = venues |> 
  geocode(address = address, method = 'arcgis', lat = latitude , long = longitude)# |> 
  # Universal Studios won't geocode

venues_sf = st_as_sf(venues_geo, coords = c("longitude", "latitude"), crs="EPSG:4326", sf_column_name = "venue_geometry")

venues_sf_combined = venues_sf |> 
  # combine events and venue names for the venue
  group_by(venue_location) |> 
  mutate(events = str_c(events, collapse = ", "),
         venues = str_c(venue_name, collapse = ", ")) |> 
  ungroup() |> 
  # Reduce data to Venue Location level
  distinct(venue_location, venues, cluster, events, venue_geometry) |> 
  # Combine venues in same location with different addresses/coordinates e.g. Exposition Park/LA Memorial Coliseum
  distinct(venue_location, venues, cluster, events, .keep_all = T)


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

venues_metro = venues_sf_combined |> 
  mutate(
    # Calculate distance to metro stops
    metro_stop_distances = map2(venue_location, venue_geometry, \(venue, venue_geometry) calculate_stop_distances(venue, venue_geometry, stops_combined)),
    # Get the nearest metro stop
    metro_nearest_stop = map(metro_stop_distances, \(stops) filter(stops, distance == min(distance))),
    # Get stops within 1km
    metro_nearby_stops = map(metro_stop_distances, \(stops) filter(stops, distance < 1000 | distance == min(distance))),
    # Get the metro routes for nearby stops
    metro_nearby_routes = map(metro_nearby_stops, \(stops) add_routes(stops, metro_gtfs))
  ) |> 
  select(-metro_stop_distances)

# Output ------------------------------------------------------------------

write_csv(venues_geo, here("data", "la_venues_geo.csv"))
write_rds(venues_geo, here("data", "la_venues_geo.rds"))

# To RDS
write_rds(venues_sf_combined, here("data", "venues_complete.rds"))

# To JSON
jsonlite::toJSON(venues_sf_combined) |> 
  write_lines(here("data", "venues_complete.json"))
