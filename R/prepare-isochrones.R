# Libraries ---------------------------------------------------------------

library(here)
library(tidyverse)
library(sf)
library(traveltimeR)
library(jsonlite)
library(tictoc)
source(here("R", "fct-traveltime.R"))

# Read in data ------------------------------------------------------------

## Venues -----------------------------------------------------------------

venues = read_rds(here("data", "venues_complete.rds"))

glimpse(venues)

## Population data (NHGIS) -----------------------------------------------

# 2020 population by census tract from: https://www.nhgis.org/
# Thought I should try this instead: https://tech.popdata.org/ipumsr/index.html
nhgis_file = here("data", "nhgis", "nhgis0001_ds258_2020_tract.csv")

nhgis = read_csv(nhgis_file)

# Filter for LA County
nhgis_la = nhgis |> 
  filter(STATE == "California", COUNTY == "Los Angeles County") |> 
  # Scale land area from square meters to square kilometers and square miles
  mutate(AREALAND_SQKM = AREALAND/1e6,
         AREALAND_SQMI = AREALAND_SQKM/2.59,
         .after = "AREALAND") |> 
  select(GISJOIN, YEAR, TRACTA, POPULATION = U7H001, AREALAND_SQKM, AREALAND_SQMI)

# Census tract shapefiles
nhgis_shp_file = here("data", "nhgis", "US_tract_2020.shp")

# Reproject CRS to WGS84
nhgis_shp = st_read(nhgis_shp_file) |> 
  st_transform(crs = "EPSG:4326")

# Join tract population and GIS data
nhgis_la_shp = inner_join(nhgis_la, nhgis_shp, by = "GISJOIN")

# Create list of tracts to exclude
exclude_tracts = tribble(
  ~TRACTCE,
  "599100",
  "599000",
  "930400"
)

# Create an opinionated subset of LA tracts: Basically south of the Angeles forest
nhgis_la_shp_subset = nhgis_la_shp |> 
  anti_join(exclude_tracts, by = "TRACTCE") |> 
  # Remove tracts where the centroid is north of 34.35
  filter(st_coordinates(st_centroid(geometry))[,2] < 34.35) |> 
  st_as_sf()

glimpse(nhgis_la_shp_subset)



# Create API Requests -----------------------------------------------------

# Calculate lat/lon and put them in a list column to match format for API calls
venues_iso = venues |> 
  mutate(lat = st_coordinates(venue_geometry)[,2], 
         lng = st_coordinates(venue_geometry)[,1],
         coords = map2(lat, lng, \(lat, lng) list(lat = lat, lng = lng))) |>
  select(id = venue_location, coords) |> 
  st_drop_geometry()

# Create a combination of venues and travel times for isochrone requests
travel_times = 60*c(30, 60, 90, 120)

iso_request_df = venues_iso |> 
  expand_grid(travel_time = 60*c(30, 60, 90, 120),
              time_of_day = c(9, 11, 13))

glimpse(iso_request_df)


## Transit -----------------------------------------------------------------

# Create a search object for each venue: public transport
venues_iso_search = iso_request_df |> 
  pmap(\(id, coords, travel_time, time_of_day) make_search(id = str_c(id, "_", travel_time), 
                                              travel_time = travel_time, 
                                              arrival_time = next_weekday(time_in_hrs = time_of_day),#"2024-12-18T09:00:00-08:00", 
                                              coords = coords, 
                                              transportation = list(type = "public_transport", 
                                                                    walking_time=15*60, 
                                                                    pt_change_delay = 120),
                                              level_of_detail = list(scale_type = "simple", level = "medium")))

tic("Make API Requests: Transit")
# Perform searches
isochrones_transit_results = map(venues_iso_search, time_map_sf)
toc()

# Combine into a single data frame
isochrones_transit_df = list_rbind(isochrones_transit_results, names_to = "id")


## Driving -----------------------------------------------------------------

venues_iso_search_drive = iso_request_df |> 
  pmap(\(id, coords, travel_time, time_of_day) make_search(id = str_c(id, "_", travel_time), 
                                                           travel_time = travel_time, 
                                                           arrival_time = next_weekday(time_in_hrs = time_of_day), 
                                                           coords = coords, 
                                                           transportation = list(type = "driving"),
                                                           level_of_detail = list(scale_type = "simple", level = "medium")))

tic("Make API Requests: Driving")
# Perform searches
isochrones_drive_results = map(venues_iso_search_drive, time_map_sf)
toc()
# Combine into a single data frame
isochrones_drive_df = list_rbind(isochrones_drive_results, names_to = "id")

## Alternatively: Load isochrone from files -------------------------------
# isochrones_transit_df = readRDS(here("data", str_c("isochrones_transit", "2024-12-16", ".Rds")))
# isochrones_drive_df = readRDS(here("data", str_c("isochrones_drive", "2024-12-16", ".Rds")))

tic("Combine Isochrones + Venues")
# Combine Isochrones + Venues ---------------------------------------------

isochrones_transit_venues = isochrones_transit_df |> 
  # Join with venues to get the venue data
  inner_join(venues, by = c("venue" = "venue_location")) |> 
  # Create travel time factor var
  mutate(travel_time_mins = factor(travel_time/60))|> 
  arrange(desc(travel_time))

isochrones_drive_venues = isochrones_drive_df |> 
  # Join with venues to get the venue data
  inner_join(venues, by = c("venue" = "venue_location")) |> 
  # Create travel time factor var
  mutate(travel_time_mins = factor(travel_time/60))|> 
  arrange(desc(travel_time))

toc()

# Join Population Data ----------------------------------------------------

tic("Join Population Data")
## Transit -----------------------------------------------------------------

# Combine multiple polygons per Venue/Time into one MultiPolygon
isochrones_transit_combined = isochrones_transit_venues |> 
  # Convert multiple individual polygons to single multipolygon
  group_by(id, venue, venues, travel_time, travel_time_mins, 
           arrival_time, transportation) |>
  summarise(geometry = st_union(geometry), .groups = "drop") |>
  st_as_sf()


# Spatial join isochrones to census tracts where they intersect
isochrones_transit_pop = isochrones_transit_combined |> 
  st_join(y=nhgis_la_shp_subset, join = st_intersects)

# Calculate the % of total population within each isochrone
total_population = sum(nhgis_la_shp_subset$POPULATION)

isochrones_transit_pop_pct = isochrones_transit_pop |> 
  st_drop_geometry() |> 
  group_by(venue, venues, travel_time, travel_time_mins, arrival_time, transportation) |>
  summarise(pop = sum(POPULATION),
            pop_pct = pop/total_population, .groups = "drop")

# Join pop % data with isochrones
isochrones_transit_output = left_join(isochrones_transit_combined,
                                      isochrones_transit_pop_pct,
                                 by = c("venue", "venues", "travel_time", "travel_time_mins", "transportation", "arrival_time"))

## Driving -----------------------------------------------------------------

# Combine multiple polygons per Venue/Time into one MultiPolygon
isochrones_drive_combined = isochrones_drive_venues |> 
  # Convert multiple individual polygons to single multipolygon
  group_by(id, venue, venues, travel_time, travel_time_mins, 
           arrival_time, transportation, walking_time, pt_change_delay) |>
  summarise(geometry = st_union(geometry), .groups = "drop") |>
  st_as_sf()


# Spatial join isochrones to census tracts where they intersect
isochrones_drive_pop = isochrones_drive_combined |> 
  st_join(y=nhgis_la_shp_subset, join = st_intersects)


isochrones_drive_pop_pct = isochrones_drive_pop |> 
  st_drop_geometry() |> 
  group_by(venue, venues, travel_time, travel_time_mins, arrival_time, transportation, walking_time, pt_change_delay) |>
  summarise(pop = sum(POPULATION),
            pop_pct = pop/total_population, .groups = "drop")

# Join pop % data with isochrones
isochrones_drive_output = left_join(isochrones_drive_combined,
                                      isochrones_drive_pop_pct,
                                      by = c("venue", "venues", "travel_time", "travel_time_mins", "transportation", "walking_time", "pt_change_delay", "arrival_time"))

toc()

# Write Data --------------------------------------------------------------

tic("Writing Data")
st_write(isochrones_transit_output, 
         here("output", str_c("isochronesTransit", today(), ".geojson.json")), 
         driver = "GeoJSON", append = F)

st_write(isochrones_drive_output, 
         here("output", str_c("isochronesDrive", today(), ".geojson.json")), 
         driver = "GeoJSON", append = F)

isochrones_df = list_rbind(list(isochrones_transit_output, isochrones_drive_output))
saveRDS(isochrones_df, here("output", str_c("isochrones", today(), ".Rds")))

toc()