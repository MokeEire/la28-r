#' Calculate stop distances
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
  venue_geometry_only = sf::st_sf(
    venue = venue, 
    venue_geometry = sf::st_sfc(venue_geometry), 
    crs="EPSG:4326"
  )
  
  # Calculate distances between venue and stops
  stop_distances = dplyr::cross_join(venue_geometry_only, rail_stops_df) |> 
    dplyr::mutate(distance = as.numeric(sf::st_distance(venue_geometry, geometry, by_element=T))) |> 
    # Keep stops within distance or the minimum
    # filter(distance <= !!within_distance | distance == min(distance)) |> 
    dplyr::select(-dplyr::starts_with("venue")) |> 
    # For reasons I can't understand, this only removes venue geometry
    sf::st_set_geometry(NULL)
  
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

#' Add Routes
#'
#' @param df 
#' @param gtfs_data 
#'
#' @return
#' @export
#'
#' @examples
add_routes = function(df, gtfs_data){
  routes = dplyr::distinct(gtfs_data$stop_times, stop_id, route_code)
  # stops_within = unnest(df, stops_within_1km)
  
  routes_within = df |> 
    dplyr::left_join(routes, by = "stop_id") |> 
    dplyr::inner_join(gtfs_data$routes, by = c("route_code" = "route_long_name")) |> 
    dplyr::distinct(route_code, route_id, route_type, route_color, route_text_color, route_url)
  
  # mutate(df, routes = list(routes_within))
  routes_within
}