# Calculate distance between venues and rail stops

library(here)
library(tidyverse)
library(sf)
library(tidytransit)

# Load Data ---------------------------------------------------------------

# Load venues

(venues = read_csv(here("data", "la_venues_geo.csv"))) |>
  glimpse()

(venues_sf = st_as_sf(venues, coords = c("longitude", "latitude"), crs="EPSG:4326", sf_column_name = "venue_geometry")) |> 
  glimpse()


# Load rail stops
la_rail = read_gtfs("https://gitlab.com/LACMTA/gtfs_rail/raw/master/gtfs_rail.zip")

(la_rail_sf = gtfs_as_sf(la_rail)) |> 
  glimpse()

rail_stops = la_rail_sf$stops

rail_stops_only = rail_stops |> 
  filter(location_type == 0)

# Load bus stops
la_bus = read_gtfs("https://gitlab.com/LACMTA/gtfs_bus/raw/master/gtfs_bus.zip")

(la_bus_sf = gtfs_as_sf(la_bus)) |> 
  glimpse()

bus_stops = la_bus_sf$stops

# Calculate Distance ------------------------------------------------------

# For each row in venues,
# I want to calculate the distance between the venue and the stop
# Option #1: Join the stops to the venue and calculate st_distance using two columns

# Find nearest stop
# 1. Cross join rail stops
# 2. Calculate distance
# 3. Keep the stop with the smallest distance
venues_all_stops = venues_sf |> 
  # Cross join rail stops
  cross_join(rail_stops_only) |> 
  # Calculate distance
  mutate(distance = as.numeric(st_distance(venue_geometry, geometry, by_element = T)))

# Can use st_join but we don't get nearest stop
venues_sf |> 
  st_join(rail_stops_only, st_is_within_distance, dist = units::set_units(1000, "m")) |> View()

# What about st_nearest_points?
venues_sf |> 
  st_join(rail_stops_only, st_nearest_feature) |> View()

# Problem: st_join does not return both geometries

# Find nearest stops + nearby (<1km) stops
nearby_stops = venues_all_stops |> 
  group_by(venue) |> 
  filter(distance == min(distance) | distance < 1000) |> View()


venues_nearest_stop = venues_all_stops |> 
  # Group by venue and keep min distance
  group_by(venue) |> 
  filter(distance == min(distance, na.rm=T)) |> 
  ungroup()

# Find stops within 1000m
venues_sf |> 
  st_join(rail_stops_only, st_is_within_distance, dist = units::set_units(1000, "m"))



#' Add Nearby Stops
#' 
#' Add a list column of stops within a specified distance of each venue and 
#' a list column with only the nearest stop information
#'
#' @param df 
#' @param rail_stops_df 
#' @param within_distance 
#'
#' @return
#' @export
#'
#' @examples
add_nearby_stops = function(df, rail_stops_df, within_distance = 1000){
  # stops_within_dist = st_join(rail_stops_df, df, st_is_within_distance, left=F, dist = units::set_units(within_distance, "m"))
  # stops_list = list(stops_within_dist)
  
  venue_geometry_only = select(df, venue, venue_geometry)
  
  # Calculate distances between venue and stops
  stop_distances = cross_join(venue_geometry_only, rail_stops_df) |> 
    mutate(distance = as.numeric(st_distance(venue_geometry, geometry, by_element=T))) |> 
    # Keep stops within distance or the minimum
    filter(distance <= !!within_distance | distance == min(distance)) |> 
    select(-starts_with("venue")) |> 
    # For reasons I can't understand, this only removes venue geometry
    st_set_geometry(NULL)
  
  # Filter for the nearest stop
  nearest_stop = stop_distances |> 
    filter(distance == min(distance))
  
  # Add both sets of stops as list columns
  mutate(df, 
         nearest_stop = list(nearest_stop),
         stops_within_1km = list(stop_distances))
}

add_routes = function(df, stop_times){
  routes = distinct(stop_times, stop_id, route_code)
  
  stops_within = unnest(df, stops_within_1km)
  browser()
  routes_within = stops_within |> left_join(routes, by = "stop_id")
}

# Split venues into list of rows
venues_split = venues_sf |> 
  split(venues_sf$venue)

venues_split |> 
  map(\(venue) as_tibble(add_nearby_stops(venue, rail_stops_only))) |> 
  map(\(venue) as_tibble(add_routes(venue, la_rail$stop_times))) |> 
  list_rbind()




