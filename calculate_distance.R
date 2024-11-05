# Calculate distance between venues and rail stops

library(here)
library(tidyverse)
library(sf)
library(tidytransit)

# Load Data ---------------------------------------------------------------

# Load venues

(venues = read_csv(here("data", "la_venues_geo.csv"))) |> 
  glimpse()

(venues_sf = st_as_sf(venues, coords = c("longitude", "latitude"), crs="EPSG:4326")) |> 
  glimpse()


# Load rail stops
la_rail = read_gtfs("https://gitlab.com/LACMTA/gtfs_rail/raw/master/gtfs_rail.zip")

(la_rail_sf = gtfs_as_sf(la_metro)) |> 
  glimpse()

rail_stops = la_rail_sf$stops

rail_stops_only = stops |> 
  filter(location_type == 0)

# Load bus stops
la_bus = read_gtfs("https://gitlab.com/LACMTA/gtfs_bus/raw/master/gtfs_bus.zip")

(la_bus_sf = gtfs_as_sf(la_bus)) |> 
  glimpse()

bus_stops = la_bus_sf$stops

# Plot Data ---------------------------------------------------------------
venues_sf |> 
  ggplot() +
  geom_sf() +
  geom_sf_label(aes(label = venue)) +
  theme_void()

venues_sf |> 
  ggplot() +
  geom_sf() +
  geom_sf_label(aes(label = venue)) +
  theme_void()

# Calculate Distance ------------------------------------------------------

find_nearest_stop = function(venue, stops_df){
  # Calc distance between venue and all stops
  distances = st_distance(venue$geometry, stops_df$geometry)
  smallest_distance_index = which.min(distances)
  
  nearest_stop = stops_df[smallest_distance_index, ] |> 
    rename_with(.fn = ~str_c("stop_", .), .cols = -starts_with("stop_")) |> 
    mutate(stop_distance_m = as.numeric(distances[smallest_distance_index]))
  
  # Join routes
  # routes = distinct(routes_df, stop_id, route_code)
  # browser()
  # nearest_stop_routes = nearest_stop |> 
  #   left_join(routes, by = "stop_id") |> 
  #   distinct(all_of(colnames(nearest_stop)))
  
  bind_cols(venue, nearest_stop)
}

find_stop_within = function(venue_geometry, stops_df, stop_times, distance_m = 1000){
  # Calc distance between venue and all stops
  distances = st_distance(venue_geometry, stops_df$geometry)
  browser()
  stops_distances = stops_df |> 
    mutate(stop_distance_m = as.numeric(distances))
  
  stops_within = filter(stops_distances, distance_m >= stop_distance_m)
  
  routes = distinct(stop_times, stop_id, route_code)
  routes_within = stops_within |> left_join(routes, by = "stop_id")
  
  # venue |> 
  #   mutate(stops = list(routes_within))
  routes_within
}

st_join(venues_sf, bus_stops, suffix = c("_venue", "_stop"), st_is_within_distance, dist = units::set_units(1000, "m")) |> View()

# Split venues into list of rows
venues_split = venues_sf |> 
  split(venues_sf$venue)

venues_nearest_stop = venues_split |> 
  map(\(venue) find_nearest_stop(venue, stops_df = stops_only)) |> 
  list_rbind()

venues_split[1] |> 
  map(\(venue) find_stop_within(venue, stops_df = bus_stops)) |> 
  list_rbind()
