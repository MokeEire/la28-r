# Libraries ---------------------------------------------------------------

library(here)
library(tidyverse)
library(sf)
library(traveltimeR)
library(jsonlite)


# Read in data ------------------------------------------------------------

## Venues -----------------------------------------------------------------

venues = read_rds(here("data", "venues_complete.rds"))

glimpse(venues)

# Some of the venues are at the same address e.g. different stadia in Carson.
# I can reduce the API calls I need to make by only using the distinct addresses.

# Distinct addresses for venues
venues_distinct_addresses = venues |> 
  # combine events for the venue
  group_by(venue_simplified) |> 
  mutate(events = str_c(events, collapse = ", "),
         venues = str_c(venue, collapse = ", ")) |> 
  ungroup() |> 
  distinct(venue_simplified, address, cluster, events, venue_geometry, .keep_all = T)

glimpse(venues_distinct_addresses)

## Population data (NHGIS) -----------------------------------------------

# 2020 population by census tract from: https://www.nhgis.org/
nhgis_file = here("data", "nhgis", "nhgis0001_ds258_2020_tract.csv")

nhgis = read_csv(nhgis_file)

# Filter for LA County
nhgis_la = nhgis |> 
  filter(STATE == "California", COUNTY == "Los Angeles County") |> 
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

# Create an opinionated subset of LA tracts
nhgis_la_shp_subset = nhgis_la_shp |> 
  anti_join(exclude_tracts, by = "TRACTCE") |> 
  # Remove tracts where the centroid is north of 34.35
  filter(st_coordinates(st_centroid(geometry))[,2] < 34.35) |> 
  st_as_sf()

glimpse(nhgis_la_shp_subset)



# Create API Requests -----------------------------------------------------

# Calculate lat/lon and put them in a list column
venues_iso = venues_distinct_addresses |> 
  mutate(lat = st_coordinates(venue_geometry)[,2], 
         lng = st_coordinates(venue_geometry)[,1],
         coords = map2(lat, lng, \(lat, lng) list(lat = lat, lng = lng))) |>
  select(id = venue_simplified, coords) |> 
  st_drop_geometry()

# Create a combination of venues and travel times for isochrone requests
iso_request_df = venues_iso |> 
  expand_grid(travel_time = 60*c(30, 60, 90, 120))

glimpse(iso_request_df)

# Create a search object for each venue: public transport
venues_iso_search = iso_request_df |> 
  pmap(\(id, coords, travel_time) make_search(id = str_c(id, "_", travel_time), 
                                              travel_time = travel_time, 
                                              arrival_time = "2024-12-18T09:00:00-08:00", 
                                              coords = coords, 
                                              transportation = list(type = "public_transport", 
                                                                    walking_time=20*60, 
                                                                    pt_change_delay = 120),
                                              level_of_detail = list(scale_type = "simple", level = "medium")))

# Perform searches
isochrones_transit_results = map(venues_iso_search, time_map_sf)

isochrones_transit_df = list_rbind(isochrones_transit_results, names_to = "id")

venue_addresses = venues_distinct_addresses |> 
  st_drop_geometry() |> 
  select(venue, venue_simplified, address)


# Combine Isochrones + Venues ---------------------------------------------

# ISSUE: Initially isochrones were created for each venue, but I realized 
# multiple venues shared almost identical locations e.g. Carson venues
# The first join adds the simplified venue  name
# The second join brings in other venue info
venue_addresses = venues_distinct_addresses |> 
  st_drop_geometry() |> 
  select(venue, venue_simplified, address)

# 1. Join on venues
isochrones_transit_venues = isochrones_transit_df |> 
  # filter(venue == sample(venue, 1)) |> 
  inner_join(venue_addresses, by = "venue") |> 
  distinct(id, venue_simplified, travel_time, arrival_time, transportation, walking_time, pt_change_delay, group, geometry) |> 
  inner_join(venues_distinct_addresses, by = "venue_simplified") |> 
  # select(-venue.x) |> 
  # rename(venue = venue.y) |> 
  mutate(travel_time_mins = factor(travel_time/60))|> 
  arrange(desc(travel_time))


# Join Population Data ----------------------------------------------------


# Combine multiple polygons per Venue/Time into one MultiPolygon
isochrones_union = isochrones_transit_venues |> 
  mutate(travel_time_mins = travel_time/60, .after = "travel_time") |> 
  # Convert multiple individual polygons to single multipolygon
  group_by(id, venue_simplified, venues, travel_time, travel_time_mins, 
           arrival_time, transportation, walking_time, pt_change_delay) |>
  summarise(geometry = st_union(geometry), .groups = "drop") |>
  st_as_sf()

# Spatial join isochrones to census tracts where they intersect
isochrones_pop_joined = isochrones_union |> 
  st_join(y=nhgis_la_shp_subset, join = st_intersects)

# Calculate the population within each isochrone
isochrones_pop_pct = isochrones_pop_joined |> 
  st_drop_geometry() |> 
  group_by(venue_simplified, travel_time, travel_time_mins, arrival_time, transportation, walking_time, pt_change_delay) |>
  summarise(pop = sum(POPULATION),
            pop_pct = pop/sum(nhgis_la_shp_subset$POPULATION), .groups = "drop")


# Write Data --------------------------------------------------------------

isochrones_pop_pct |> 
  select(venue_simplified, travel_time, arrival_time, pop, pop_pct) |> 
  right_join(isochrones_union, by = c("venue_simplified", "travel_time", "arrival_time")) |> 
  st_write(here("data", "isochronesTransit.geojson.json"), driver = "GeoJSON", append = F)
