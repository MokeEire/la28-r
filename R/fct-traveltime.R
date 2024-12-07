#' Title
#'
#' @param arrival 
#'
#' @return
#' @export
#'
#' @examples
time_map_fast_sf = function(arrival){
  arrival_df = list_rbind(arrival)
  # Call the API
  time_map_result = time_map_fast(
    arrival_many_to_one = arrival,
    format = "geo+json"
  )
  
  result_shapes = time_map_result$contentParsed$results[[1]]$shapes
  
  result_shell = modify(result_shapes, "shell") |> 
    modify_depth(.depth = 2, as_tibble) |> 
    map(list_rbind) |> 
    list_rbind(names_to = "group")
  
  result_points = sf::st_as_sf(x = result_shell,
                               coords = c("lng", "lat"),
                               crs = "+proj=longlat +datum=WGS84") #"EPSG:4326"
  
  result_points |> 
    dplyr::mutate(ID=dplyr::row_number()) |> 
    dplyr::group_by(group) |> 
    dplyr::arrange(ID) |> 
    summarise(
      INT = dplyr::first(ID),
      geometry = st_cast(st_combine(geometry), "POLYGON"), 
      .groups = "drop"
    ) |> 
    # dplyr::summarize(INT = dplyr::first(ID), do_union = FALSE) |> 
    #   sf::st_cast("POLYGON") |> 
    dplyr::select(-INT) |> 
    mutate(venue = arrival_df$id,
           travel_time = arrival_df$travel_time,
           arrival_time_period = arrival_df$arrival_time_period,
           transportation = arrival_df$transportation$type,
           .before = "group")
}

show_time_of_day = function(date) stamp("8am on Tuesday", orders = "%I%p %A")(date)


time_map_sf = function(arrival){
  Sys.sleep(1)
  arrival_df = list_rbind(arrival) |> 
    unnest_wider(transportation, names_sep = "_")
  # Call the API
  time_map_result = time_map(
    arrival_searches = arrival,
    format = "geo+json"
  )
  
  result_shapes = time_map_result$contentParsed$results[[1]]$shapes
  
  result_shell = modify(result_shapes, "shell") |> 
    modify_depth(.depth = 2, as_tibble) |> 
    map(list_rbind) |> 
    list_rbind(names_to = "group")
  
  result_points = sf::st_as_sf(x = result_shell,
                               coords = c("lng", "lat"),
                               crs = "EPSG:4326") #"+proj=longlat +datum=WGS84"
  
  result_sum = result_points |> 
    dplyr::mutate(ID=dplyr::row_number()) |> 
    dplyr::group_by(group) |> 
    dplyr::arrange(ID) |> 
    summarise(
      INT = dplyr::first(ID),
      geometry = st_cast(st_combine(geometry), "POLYGON"), 
      .groups = "drop"
    ) |> 
    # dplyr::summarize(INT = dplyr::first(ID), do_union = FALSE) |> 
    #   sf::st_cast("POLYGON") |> 
    dplyr::select(-INT)
  
  if(arrival_df$transportation_type == "public_transport"){
    mutate(result_sum,
           id = arrival_df$id,
           venue = str_extract(arrival_df$id, ".+(?=_)"),
           travel_time = arrival_df$travel_time,
           arrival_time = arrival_df$arrival_time,
           transportation = arrival_df$transportation_type,
           walking_time = arrival_df$transportation_walking_time,
           pt_change_delay = arrival_df$transportation_pt_change_delay,
           .before = "group")
  } else {
    mutate(result_sum,
           id = arrival_df$id,
           venue = str_extract(arrival_df$id, ".+(?=_)"),
           travel_time = arrival_df$travel_time,
           arrival_time = arrival_df$arrival_time,
           transportation = arrival_df$transportation_type,
           walking_time = NA_real_,
           pt_change_delay = NA_real_,
           .before = "group")
  }
  
}

# map isochrones function
map_isochrones = function(data){
  ggplot(data)+
    # Map of LA tracts
    geom_sf(data = nhgis_la_shp_subset, aes(geometry = geometry, fill = POPULATION), colour = NA)+
    # Public transport Isochrone
    geom_sf(aes(geometry = geometry), fill = "red", alpha = 0.5)+
    geom_sf(aes(geometry = venue_geometry), colour = "black", size = 1.5)+
    # Venue point
    # geom_sf(aes(geometry = venue_geometry), colour = "black")+
    scale_fill_continuous(low = "white", high = "blue")+
    labs(title = unique(data$venue))+
    facet_grid(rows = vars(arrival_time), cols = vars(transportation), 
               labeller = labeller(transportation = str_to_title))+
    theme_void()
}

routes_to_df = function(route_arrival_search, route_locations){
  result = routes(
    # departure_searches = departure_search,
    arrival_searches = route_arrival_search,
    locations = route_locations
  )
  
  parsed_result = result$contentParsed$results
  
  route_results = parsed_result |> 
    map(extract_location) |> 
    list_rbind() |> 
    mutate(venue = str_extract(search_id, regex(".+(?=\\s\\-)", dotall = TRUE)))
  
  
  # Flatten properties
  # parsed_result_properties = modify_in(parsed_result_locations, list(1, "properties"), \(props) list_flatten(props)) |> 
  #   # Keep only travel time and distance
  #   modify_in(list(1, "properties"), \(props) keep_at(props, c("travel_time", "distance"))) |> 
  #   map(list_flatten, name_spec = "{inner}") |> 
  #   map(as_tibble) |> 
  #   list_rbind() |> 
  #   rename(from_tract = id)
  
  # parsed_result_properties = modify_depth(parsed_result_locations, 1,  \(props) modify_in(props, "properties", list_flatten)) |> 
  #   modify_depth(1,  \(props) modify_in(props, "properties", keep_at, c("travel_time", "distance"))) |> 
  #   map(list_flatten, name_spec = "{inner}") |> 
  #   map(as_tibble) |> 
  #   list_rbind() |> 
  #   rename(from_tract = id)
  
  # parsed_result_properties = parsed_result_locations |> 
  #   map(\(route) modify_in(route, "properties", list_flatten)) |> 
  #   map(\(route) modify_in(route, "properties", keep_at, c("travel_time", "distance"))) |> 
  #   map(list_flatten, name_spec = "{inner}") |> 
  #   map(as_tibble) |> 
  #   list_rbind() |> 
  #   rename(from_tract = id)
  # 
  # parsed_result_locations
  # parsed_result |> 
  #   map(modify_in, "locations", extract_location) |> 
  #   map(modify_in, "unreachable", convert_unreached_locations) |> 
  #   #map(modify_in, "search_id", \(search) tibble(search_id = search)) |> 
  #   #map(reduce, bind_cols) |> 
  #   str(max.level = 6)
  
  route_search_vals = map(route_arrival_search, keep_at, c("arrival_location_id", "arrival_time", "transportation")) |> 
    list_rbind() |> 
    hoist(transportation, "type", "pt_change_delay") |> 
    rename(venue = arrival_location_id)
  
  # TODO: incorporate route info like time walking, time on transit, etc.
  
  route_results |> 
    left_join(route_search_vals, by = "venue") |> 
    select(search_id, type, arrival_time, pt_change_delay, tract, venue, distance, travel_time)

}

extract_location = function(search_list){

  locations = pluck(search_list, "locations")
  
  if(length(locations) > 0){
    
    locations_df = map(locations, extract_properties) |> 
      list_rbind()
    
    locations_df |> 
      mutate(search_id = search_list$search_id, 
             .before = "id") |> 
      rename(tract = id)
  } else {
    tibble(
      tract = character(),
      travel_time = integer(),
      distance = integer(),
      search_id = character(),
      venue = character(), 
      .rows = 0
    )
  }
  
  
    
    
}

extract_properties = function(location, keep_elements = c("travel_time", "distance")){
  location |> 
    modify_in("properties", list_flatten) |> 
    modify_in("properties", keep_at, keep_elements) |> 
    list_flatten(name_spec = "{inner}") |> 
    as_tibble()
}

convert_unreached_locations = function(unreached_locations){
  map(unreached_locations, \(loc) tibble(tract = loc, travel_time = NA_integer_, distance = NA_integer_)) |> 
    list_rbind()
}
