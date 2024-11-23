library(jsonlite)
library(geojsonsf)
library(here)
library(tidyverse)
library(sf)
library(gmapsdistance)
library(tictoc)

# Load Census data --------------------------------------------------------

tract_file = here("data", "2020_Census_Tracts.geojson")
tracts = geojson_sf(tract_file)

exclude_tracts = tribble(
  ~CT20,
  "599100",
  "599000",
  "930400"
)

tracts_df = tracts |> 
  anti_join(exclude_tracts, by = "CT20") |> 
  mutate(center = st_centroid(geometry)) |> 
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
  mutate(distance = as.numeric(st_distance(venue_geometry, center, by_element=T))) |> 
  select(-ShapeSTArea, -ShapeSTLength, -OBJECTID)

tract_distances |> 
  group_by(venue) |> 
  summarise(across(distance, list(mean = mean, sd = sd, min = min, max = max)))


# Calculate travel times --------------------------------------------------

sample_venue = tract_distances |> 
  filter(venue == sample(venue, 1)) |>
  mutate(venue_coords = str_c(st_coordinates(venue_geometry)[,2], ", ", st_coordinates(venue_geometry)[,1]),
         tract_coords = str_c(round(st_coordinates(center)[,2], 5), ", ", round(st_coordinates(center)[,1], 5))) |> 
  # sample_n(640) |> 
  # relocate(venue_geometry, venue_coords, geometry, tract_coords, .before = address) |> 
  select(venue, venue_geometry, venue_coords, center, tract_coords, geometry,
         LABEL)


sample_venue |> 
  ggplot()+
  geom_sf(aes(geometry=geometry))+
  geom_sf(aes(geometry=center), colour="blue")+
  geom_sf(aes(geometry=venue_geometry), colour="red")

# Calculate travel time from tract to venue
# Tracts: 2339
# Venues: 19
# Modes of Transport: 2 (driving, transit)
# Traffic Models: 3 (optimistic, pessimistic, best_guess)
# Total Combinations: 2339 * 19 * 2 * 3 = 266,646

# tic("Time to calculate travel time for single venue")
# sample_times = gmapsdistance(
#   origin = sample_venue$tract_coords,
#   destination = sample_venue$venue_coords,
#   combinations = "pairwise",
#   mode = "driving",
#   dep_time = "18:00:00",
#   dep_date = "2024-11-18",
#   traffic_model = "best_guess",
#   key = Sys.getenv("GOOGLE_API_KEY")
# )
# toc()

# poss_gmaps = possibly(.f = gmapsdistance, otherwise = NA)

# sample_times_map = map2(sample_venue$tract_coords, sample_venue$venue_coords, \(origin, destination) poss_gmaps(
#   origin = origin,
#   destination = destination,
#   combinations = "pairwise",
#   mode = "driving",
#   dep_time = "18:00:00",
#   dep_date = "2024-11-18",
#   traffic_model = "best_guess",
#   key = Sys.getenv("GOOGLE_API_KEY")
# )
# )


# Test gmaps distance func ------------------------------------------------

travel_modes = c("driving", "transit")

sample_venue_multiple = tract_distances |> 
  filter(venue %in% sample(venue, 3)) |> 
  mutate(venue_coords = str_c(st_coordinates(venue_geometry)[,2], ", ", st_coordinates(venue_geometry)[,1]),
         tract_coords = str_c(round(st_coordinates(center)[,2], 5), ", ", round(st_coordinates(center)[,1], 5))) |> 
  # sample_n(640) |> 
  # relocate(venue_geometry, venue_coords, geometry, tract_coords, .before = address) |> 
  select(venue, venue_geometry, venue_coords, center, tract_coords,
         LABEL)

sample_venue_both_modes = map(travel_modes, \(mode) gmapsdistance(
  origin = sample_venue_multiple$tract_coords,
  destination = sample_venue_multiple$venue_coords,
  combinations = "pairwise",
  mode = mode,
  dep_time = "18:00:00",
  dep_date = "2024-12-18",
  # traffic_model = "best_guess",
  key = Sys.getenv("GOOGLE_API_KEY")
)) |> set_names(travel_modes)

sample_venue_both_modes_output = map(sample_venue_both_modes, list_cbind) |> 
  imap(\(df, i) mutate(df, mode = i)) |> 
  list_rbind() |> 
  as_tibble()

saveRDS(sample_venue_both_modes_output, here("data", "sample_venue_both_modes_output.rds"))

sample_venue_both_modes_output = readRDS(here("data", "sample_venue_both_modes_output.rds"))

sample_venue_tract_distances = gmapsdistance(
  origin = sample_venue$tract_coords,
  destination = sample_venue$venue_coords,
  combinations = "pairwise",
  mode = "driving",
  dep_time = "18:00:00",
  dep_date = "2024-12-18",
  traffic_model = "best_guess",
  key = Sys.getenv("GOOGLE_API_KEY")
)

sample_venue_tract_distances_transit = gmapsdistance(
  origin = sample_venue$tract_coords,
  destination = sample_venue$venue_coords,
  combinations = "pairwise",
  mode = "transit",
  dep_time = "18:00:00",
  dep_date = "2024-12-18",
  key = Sys.getenv("GOOGLE_API_KEY")
)

sample_venue_joined = sample_venue |> 
  inner_join(sample_venue_tract_distances$Distance, by = c("tract_coords" = "or", "venue_coords" = "de")) |> 
  inner_join(sample_venue_tract_distances$Time, by = c("tract_coords" = "or", "venue_coords" = "de")) |> 
  mutate(distance_km = Distance/1000,
         time_mins = Time/60)

sample_venue_joined |> 
  ggplot()+
  geom_sf(aes(geometry=geometry, fill=time_mins))+
  geom_sf(aes(geometry=venue_geometry), colour="black")+
  # scale_fill_continuous(low="green", high="red", breaks = c(0, 60, 120, 180, 240, 300, 360), #limits = c(0, 400),
  #                       rescale = scales::rescale_pal(range = c(0, 400)))+
  scale_fill_gradientn(colours = hcl.colors(14, "RdYlGn", rev = T),
                       limits = range(c(0,400)),
                       # valuesRdYlGn = c(0, seq(30, 360, length.out = 12), 400),
                       name = "Values")

sample_venue_joined |> 
  ggplot()+
  geom_histogram(aes(x=time_mins), colour = "white", bins=20)+
  theme_minimal()

sample_venue_joined_transit = sample_venue |> 
  inner_join(sample_venue_tract_distances_transit$Distance, by = c("tract_coords" = "or", "venue_coords" = "de")) |> 
  inner_join(sample_venue_tract_distances_transit$Time, by = c("tract_coords" = "or", "venue_coords" = "de")) |> 
  mutate(distance_km = Distance/1000,
         time_mins = Time/60)

sample_venue_joined_transit |> 
  mutate(time_mins = if_else(time_mins > 400, NA_real_, time_mins)) |> 
  ggplot()+
  geom_sf(aes(geometry=geometry, fill=time_mins))+
  geom_sf(aes(geometry=venue_geometry), colour="black")+
  scale_fill_gradientn(colours = hcl.colors(14, "RdYlGn", rev = T),
                       limits = range(c(0,400)),
                       # valuesRdYlGn = c(0, seq(30, 360, length.out = 12), 400),
                       name = "Values")

sample_venue_joined_transit |> 
  ggplot()+
  geom_histogram(aes(x=time_mins), colour = "white", bins=20)+
  theme_minimal()

ggplot()+
  geom_histogram(data = sample_venue_joined, aes(x=time_mins), fill = "blue", alpha = .8, colour = "white", binwidth = 10)+
  geom_histogram(data = mutate(sample_venue_joined_transit, time_mins = if_else(time_mins > 300, NA_real_, time_mins)), 
                 aes(x=time_mins), fill = "red", alpha = .7, colour = "white", binwidth = 10)+
  scale_x_continuous(breaks = c(0, 60, 120, 180, 240, 300),
                     minor_breaks = c(0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300))+
  theme_minimal()

sample_venue_joined_transit

source("gmaps_func.R")

tic(str_c("Time to calculate travel time for ", nrow(sample_venue), " tracts"))
sample_venue_distance = gmapsdistance_test_single(
  sample_venue,
  origin = tract_coords,
  destination = venue_coords,
  combinations = "pairwise",
  mode = "driving",
  dep_time = "18:00:00",
  dep_date = "2024-12-18",
  traffic_model = "best_guess",
  key = Sys.getenv("GOOGLE_API_KEY")
)
toc()

# Run for both driving and transit
traffic_models = c("optimistic", "pessimistic", "best_guess")

tic("Time to calculate multiple travel time models for single venue")
sample_times_models = map(traffic_models, \(model) gmapsdistance(
  origin = sample_venue$tract_coords, 
  destination = sample_venue$venue_coords, 
  combinations = "pairwise",
  mode = "driving",
  dep_time = "18:00:00",
  dep_date = "2024-11-18",
  traffic_model = model,
  key = Sys.getenv("GOOGLE_API_KEY")
))

sample_venue |> 
  mutate(driving_time = sample_times$Time$Time,
         driving_time_mins = driving_time/60,
         driving_distance = sample_times$Distance$Distance) |> View()

sample_venue |> 
  mutate(driving_time = sample_times$Time$Time,
         opt_driving_time = sample_times_models[[1]]$Time$Time,
         pes_driving_time = sample_times_models[[2]]$Time$Time,
         driving_time_mins = driving_time/60,
         opt_driving_time_mins = opt_driving_time/60,
         pes_driving_time_mins = pes_driving_time/60,
         driving_distance = sample_times$Distance$Distance) |> View()
