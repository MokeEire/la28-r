#' Return the date of the next weekday
#'
#' @param date 
#' @param time_in_hrs 
#' @param format_as_text 
#'
#' @returns
#' @export
#'
#' @examples
next_weekday = function(date = today(), time_in_hrs = 9, format_as_text = T){
  dow = wday(date, week_start = 1)
  
  if(dow == 5){
    new_date = date + days(3)
  } else if(dow == 6){
    new_date = date + days(2)
  } else {
    new_date = date + days(1)
  }
  
  new_date_tz = force_tz(new_date+hours(time_in_hrs), tzone = "America/Los_Angeles")
  
  if(format_as_text){
    format_ISO8601(new_date_tz, usetz=T)
  } else {
    new_date_tz
  }
  
}

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
           venue = str_extract(arrival_df$id, "([^_])+(?=_)"),
           travel_time = arrival_df$travel_time,
           arrival_time = arrival_df$arrival_time,
           transportation = arrival_df$transportation_type,
           walking_time = arrival_df$transportation_walking_time,
           pt_change_delay = arrival_df$transportation_pt_change_delay,
           .before = "group")
  } else {
    mutate(result_sum,
           id = arrival_df$id,
           venue = str_extract(arrival_df$id, "([^_])+(?=_)"),
           travel_time = arrival_df$travel_time,
           arrival_time = arrival_df$arrival_time,
           transportation = arrival_df$transportation_type,
           walking_time = NA_real_,
           pt_change_delay = NA_real_,
           .before = "group")
  }
  
}

get_routes = function(search, locations, sleep_time = 1){
  Sys.sleep(sleep_time)
  # browser()
  # tic(str_c("Get Routes: ", pluck(search, 1, "departure_location_ids", "departure_location_ids", 1), " to ", pluck(search, 1, "id")))
  arrival_df = list_rbind(search) |> 
    unnest_wider(transportation, names_sep = "_")
  
  # Call the API
  routes_result = routes(
    arrival_searches = search,
    locations = locations
  )
  # browser()
  result_df = routes_result$contentParsed$results[[1]]$locations |> 
    map_depth(1, map_if, is_list, list_flatten) |> 
    map_if(is_list, list_flatten, name_spec = "{inner}") |> 
    map(as_tibble) |> 
    list_rbind()



  # toc()
  if(arrival_df$transportation_type == "public_transport"){
    mutate(result_df,
           venue_id = arrival_df$id,
           venue = str_extract(arrival_df$id, "([^_])+(?=_)"),
           arrival_time = arrival_df$arrival_time,
           transportation = arrival_df$transportation_type,
           walking_time = arrival_df$transportation_walking_time,
           pt_change_delay = arrival_df$transportation_pt_change_delay)
  } else {
    mutate(result_df,
           venue_id = arrival_df$id,
           arrival_time = arrival_df$arrival_time,
           transportation = arrival_df$transportation_type,
           walking_time = NA_real_,
           pt_change_delay = NA_real_)
  }
  
}

time_filter_fast_to_df = function(search, locations, sleep_time = 1){
  Sys.sleep(sleep_time)
  # browser()
  tictoc::tic(str_c("Travel Times: ", pluck(search, 1, "id")))
  arrival_df = list_rbind(search) |> 
    unnest_wider(transportation, names_sep = "_")
  
  # Call the API
  routes_result = time_filter_fast(
    arrival_many_to_one = search,
    locations = locations
  )
  # browser()
  result_df = routes_result$contentParsed$results[[1]]$locations |> 
    map_depth(1, map_if, is_list, list_flatten) |> 
    map_if(is_list, list_flatten, name_spec = "{inner}") |> 
    map(as_tibble) |> 
    list_rbind()
  
  
  
  tictoc::toc()
  if(arrival_df$transportation_type == "public_transport"){
    mutate(result_df,
           venue_id = arrival_df$id,
           arrival_time_period = arrival_df$arrival_time_period,
           transportation = arrival_df$transportation_type,
           walking_time = arrival_df$transportation_walking_time,
           pt_change_delay = arrival_df$transportation_pt_change_delay)
  } else {
    mutate(result_df,
           venue_id = arrival_df$id,
           arrival_time_period = arrival_df$arrival_time_period,
           transportation = arrival_df$transportation_type,
           walking_time = NA_real_,
           pt_change_delay = NA_real_)
  }
  
}

time_filter_to_df = function(venues, tract_ids, locations, sleep_time = 1){
  Sys.sleep(sleep_time)
  # browser()
  tract_count = length(tract_ids)
  tract_ids1 = tract_ids[1:(tract_count/2)]
  tract_ids2 = tract_ids[((tract_count/2)+1):tract_count]
  
  time_requests = venues |> 
    select(-coords) |> 
    expand_grid(transportation_type = c("driving", "public_transport"), 
                tract_list = list(tract_ids1, tract_ids2)) |>
    mutate(tract_list_count = rep_len(1:2, n())) |> 
    pmap(\(id, transportation_type, tract_list, tract_list_count) make_search(id = str_c(id, " ", tract_list_count, " | ", transportation_type), 
                                                            departure_location_ids = tract_list,
                                                            arrival_location_id = id,
                                                            arrival_time = next_weekday(time_in_hrs = 10),
                                                            travel_time = 60*60*3, # 3 hrs
                                                            properties = list("travel_time", "distance"),
                                                            transportation = list(type = transportation_type, 
                                                                                  walking_time=15*60, 
                                                                                  cycling_time_to_station = 15*60,
                                                                                  pt_change_delay = 120,
                                                                                  parking_time = 600,
                                                                                  traffic_model = "pessimistic"),
                                                            snapping = list(threshold = 250)))
  
  map(time_requests, \(request) time_filter_fct(search = request, locations)) |> 
    list_rbind() |> 
    mutate(time_mins = travel_time/60,
           distance_mi = distance/1609, .before = "travel_time")
  
  
}

time_filter_fct = function(search, locations){
  tictoc::tic(str_c("Travel Times: ", pluck(search, 1, "id")))
  arrival_df = list_rbind(search) |> 
    unnest_wider(transportation, names_sep = "_")
  # browser()
  # Call the API
  routes_result = time_filter(
    arrival_searches = search,
    locations = locations
  )
  # browser()
  result_df = routes_result$contentParsed$results[[1]]$locations |> 
    map_depth(1, map_if, is_list, list_flatten) |> 
    map_if(is_list, list_flatten, name_spec = "{inner}") |> 
    map(as_tibble) |> 
    list_rbind()
  
  
  
  tictoc::toc()
  if(arrival_df$transportation_type == "public_transport"){
    mutate(result_df,
           venue_id = arrival_df$id,
           arrival_time = arrival_df$arrival_time,
           transportation = arrival_df$transportation_type,
           walking_time = arrival_df$transportation_walking_time,
           pt_change_delay = arrival_df$transportation_pt_change_delay)
  } else {
    mutate(result_df,
           venue_id = arrival_df$id,
           arrival_time = arrival_df$arrival_time,
           transportation = arrival_df$transportation_type,
           walking_time = NA_real_,
           pt_change_delay = NA_real_)
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
