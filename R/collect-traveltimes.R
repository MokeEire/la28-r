library(here)
library(tidyverse)
library(sf)
library(traveltimeR)
library(jsonlite)

# Load custom functions
source(here("R", "fct-traveltime.R"))

# Census tract shapefiles
nhgis_shp_file = here("data", "nhgis", "US_tract_2020.shp")

# Create list of tracts to exclude
exclude_tracts = tribble(
  ~TRACTCE, ~include,
  "599100", F, # Catalina Island
  "599000", F, # Catalina Island
  "930400", F,  # Angeles Forest
)

# Load the shapefile
nhgis_shp = read_sf(nhgis_shp_file) |> 
  # Filter to Los Angeles, CA
  filter(STATEFP == "06", COUNTYFP == "037") |>
  # Reproject CRS to WGS84
  st_transform(crs = "EPSG:4326") |> 
  # Create an opinionated subset of LA tracts
  # anti_join(exclude_tracts, by = "TRACTCE") |>
  # Remove tracts where the centroid is north of 34.35
  # filter(st_coordinates(st_centroid(geometry))[,2] < 34.35) |>
  select(GISJOIN, TRACTCE, GEOID, NAME, NAMELSAD, geometry) |> 
  left_join(exclude_tracts, by = "TRACTCE") |> 
  mutate(include = case_when(!is.na(include) ~ include,
                             st_coordinates(st_centroid(geometry))[,2] > 34.35 ~ F,
                             .default = T))

nhgis_la = nhgis_shp |> 
  filter(include) |> 
  select(-include)

nhgis_centroids = nhgis_la |> 
  # Find centroids of each tract
  mutate(centroid = st_centroid(geometry))


# Venues ------------------------------------------------------------------


venues = read_rds(here("data", "venues_complete.rds"))

venue_locations = venues |> 
  mutate(lat = st_coordinates(venue_geometry)[,2], 
         lng = st_coordinates(venue_geometry)[,1]) |> 
  st_drop_geometry() |>
  select(id = venue_location, lat, lng) |> 
  pmap(\(id, lat, lng) make_location(id = id, coords = list(lat = as.numeric(lat), lng = as.numeric(lng)))) |> 
  unlist(recursive=F)

tract_locations = nhgis_centroids |> 
  mutate(lat = st_coordinates(centroid)[,2], 
         lng = st_coordinates(centroid)[,1]) |> 
  st_drop_geometry() |>
  select(id = TRACTCE, lat, lng) |> 
  pmap(\(id, lat, lng) make_location(id = id, coords = list(lat = as.numeric(lat), lng = as.numeric(lng)))) |> 
  unlist(recursive=F)

locations_list = list_flatten(list(venue_locations, tract_locations))

tract_ids = map(tract_locations, "id")
  
travel_times = time_filter_to_df(venues = venue_locations, tract_ids = tract_ids, locations = locations_list, 0)

travel_times |> 
  select(id, venue, arrival_time, transportation, time_mins) |> 
  pivot_wider(id_cols = c(id, venue, arrival_time), names_from = transportation, values_from = c(time_mins), id_expand = T) |> 
  mutate(driving = driving*1.05, 
         public_transport = public_transport*.95) |>
  arrange(venue, id) |> 
  mutate(time_diff = public_transport - driving) |> 
  group_by(venue) |> 
  summarise(
    driving_avg = mean(driving, na.rm=T),
    transit_avg = mean(public_transport, na.rm=T),
    diff_avg = mean(time_diff, na.rm=T)
  )


# Output ------------------------------------------------------------------

# Save travel times file as: data/traveltime/travel_times_YYYY-MM-DD.rds

saveRDS(travel_times, here("data", "traveltime", str_c("travel_times_", today(),".rds")))

# Plot

travel_times |> 
  select(id, venue, arrival_time, transportation, time_mins) |> 
  pivot_wider(id_cols = c(id, venue, arrival_time), names_from = transportation, values_from = c(time_mins), id_expand = T) |> 
  mutate(driving = driving*1.075, 
         public_transport = public_transport*.925) |>
  arrange(venue, id) |> 
  mutate(time_diff = public_transport - driving,
         time_diff_ratio = public_transport/driving) |> 
  group_by(venue) |> 
  mutate(mean_time_ratio = mean(time_diff_ratio, na.rm=T),
         mean_midpoint = mean(driving, na.rm=T) + mean(time_diff, na.rm=T)/2) |> 
  ungroup() |> 
  arrange(desc(mean_time_ratio)) |> 
  mutate(venue = fct_inorder(venue)) |> 
  pivot_longer(cols = c(driving, public_transport), names_to = "transport_mode", values_to = "time_mins") |> 
  group_by(venue, transport_mode)|> 
  mutate(mean_time = mean(time_mins, na.rm=T)) |> 
  ungroup() |> 
  ggplot(aes(x = time_mins)) +
  # Geoms: vertical mean lines, histograms, average difference ratio, 
  geom_vline(aes(xintercept = mean_time, colour = transport_mode),
             linetype = "dashed", linewidth = 1, alpha = 0.95, show.legend = F) +
  geom_histogram(aes(fill = transport_mode), position = "identity", alpha = 0.75, bins = 30, colour = "white") +
  geom_text(aes(x = mean_time, colour = transport_mode, label = scales::comma(mean_time, suffix = " mins", accuracy = 1), 
                hjust = if_else(transport_mode == "driving", 1, 0),
                nudge_x = if_else(transport_mode == "driving", -5, 5)), y = 550, size = 3,
             show.legend = F) +
  geom_text(aes(x = mean_midpoint, label = scales::comma(mean_time_ratio, suffix = "x", accuracy = .1)), y = 620,
            show.legend = F)+
  facet_wrap(~venue) +
  # Scales
  scale_fill_manual(values = c("driving" = "#5993D1", "public_transport" = "#f5a623"), breaks = c("Driving" = "driving", "Transit" = "public_transport")) +
  scale_colour_manual(values = c("driving" = "#5186BE", "public_transport" = "#EA940B")) +
  labs(title = "Travel Time Distribution by Transport Mode",
       x = "Travel Time (minutes)",
       y = "Count",
       fill = "Transport Mode") +
  theme_minimal(base_size = 16)+
  theme(legend.position = "top")
