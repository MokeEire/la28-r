# Calculate distance between venues and rail stops

library(here)
library(tidyverse)
library(sf)
library(tidytransit)

# Load Data ---------------------------------------------------------------

# Load venues

# (venues = read_csv(here("data", "la_venues_geo.csv"))) |>
#   glimpse()
venues = read_rds(here("data", "la_venues_geo.rds"))
glimpse(venues)

(venues_sf = st_as_sf(venues, coords = c("longitude", "latitude"), crs="EPSG:4326", sf_column_name = "venue_geometry")) |> 
  glimpse()

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

# Metro Bus
# Load bus stops
# bus_gtfs = read_gtfs("https://gitlab.com/LACMTA/gtfs_bus/raw/master/gtfs_bus.zip")
# 
# (bus_sf = gtfs_as_sf(bus_gtfs)) |> 
#   glimpse()
# 
# bus_stops = bus_sf$stops


# Calculate Distance ------------------------------------------------------

# For each row in venues,
# I want to calculate the distance between the venue and the stop
# Option #1: Join the stops to the venue and calculate st_distance using two columns


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
calculate_stop_distances = function(venue, venue_geometry, rail_stops_df, within_distance = 1000){
  # stops_within_dist = st_join(rail_stops_df, df, st_is_within_distance, left=F, dist = units::set_units(within_distance, "m"))
  # stops_list = list(stops_within_dist)
  # browser()
  # venue_geometry_only = select(df, venue, venue_geometry)
  venue_geometry_only = st_sf(
    venue = venue, 
    venue_geometry = st_sfc(venue_geometry), 
    crs="EPSG:4326"
  )
  
  # Calculate distances between venue and stops
  stop_distances = cross_join(venue_geometry_only, rail_stops_df) |> 
    mutate(distance = as.numeric(st_distance(venue_geometry, geometry, by_element=T))) |> 
    # Keep stops within distance or the minimum
    # filter(distance <= !!within_distance | distance == min(distance)) |> 
    select(-starts_with("venue")) |> 
    # For reasons I can't understand, this only removes venue geometry
    st_set_geometry(NULL)
  
  stop_distances
  # Filter for the nearest stop
  # nearest_stop = stop_distances |> 
  #   filter(distance == min(distance))
  # 
  # # Add both sets of stops as list columns
  # mutate(df, 
  #        nearest_stop = list(nearest_stop),
  #        stops_within_1km = list(stop_distances))
}

# For each nearby stop, what routes are available?
add_routes = function(df, gtfs_data){
  routes = distinct(gtfs_data$stop_times, stop_id, route_code)
  # browser()
  # stops_within = unnest(df, stops_within_1km)
  
  routes_within = df |> 
    left_join(routes, by = "stop_id") |> 
    inner_join(gtfs_data$routes, by = c("route_code" = "route_long_name")) |> 
    distinct(route_code, route_id, route_type, route_color, route_text_color, route_url)
  
  # mutate(df, routes = list(routes_within))
  routes_within
}

# Split venues into list of rows
venues_split = venues_sf |> 
  split(venues_sf$venue)



venues_metro = venues_sf |> 
  mutate(metro_stop_distances = map2(venue, venue_geometry, \(venue, venue_geometry) calculate_stop_distances(venue, venue_geometry, stops_combined)),
         metro_nearest_stop = map(metro_stop_distances, \(stops) filter(stops, distance == min(distance))),
         metro_nearby_stops = map(metro_stop_distances, \(stops) filter(stops, distance < 1000)),
         metro_nearby_routes = map(metro_nearby_stops, \(stops) add_routes(stops, metro_gtfs)))


venues_complete = venues_split |> 
  # map(\(venue) as_tibble(add_nearby_stops(venue, rail_stops_only))) |> 
  # map(\(venue) as_tibble(add_routes(venue, metro_gtfs))) |> 
  # list_rbind() |> 
  mutate(metro_routes = map(stops_within_1km, \(stops) add_routes(stops, metro_gtfs)))



# Write -----------------------------------------------------------

# To RDS
write_rds(venues_metro, here("data", "venues_complete.rds"))

# To JSON
jsonlite::toJSON(venues_complete) |> 
  write_lines(here("data", "venues_complete.json"))


# Rough Work --------------------------------------------------------------

# Find nearest stop
# 1. Cross join rail stops
# 2. Calculate distance
# 3. Keep the stop with the smallest distance
# venues_all_stops = venues_sf |> 
#   # Cross join rail stops
#   cross_join(rail_stops_only) |> 
#   # Calculate distance
#   mutate(distance = as.numeric(st_distance(venue_geometry, geometry, by_element = T)))
# 
# # Can use st_join but we don't get nearest stop
# venues_sf |> 
#   st_join(rail_stops_only, st_is_within_distance, dist = units::set_units(1000, "m"))
# 
# # What about st_nearest_points?
# venues_sf |> 
#   st_join(rail_stops_only, st_nearest_feature)

# Problem: st_join does not return both geometries
