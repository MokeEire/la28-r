# Geocode Venues

library(here)
library(tidyverse)
library(tidygeocoder)
library(janitor)

# Load Data ---------------------------------------------------------------

venues = read_csv(here("data", "venues.csv")) |> 
  clean_names()

glimpse(venues)

# Geocode ----------------------------------------------------------------
# View example here: https://jessecambon.github.io/tidygeocoder/

(venues_geo = venues |> 
  geocode(address, method = 'osm', lat = latitude , long = longitude)) |> 
  glimpse()

la_venues_geo = venues_geo |> 
  filter(in_los_angeles)

# Plot to check --------------------------------------------------------------------

la_venues_geo |> 
  ggplot(aes(x = longitude, y = latitude)) +
  borders("county", 
          xlim = c(min(la_venues_geo$longitude, na.rm=T)-.01, max(la_venues_geo$longitude, na.rm=T)+.01), 
          ylim = c(min(la_venues_geo$latitude, na.rm=T)-.01, max(la_venues_geo$latitude, na.rm=T)+.01))+
  geom_point() +
  theme_void()


# Output ------------------------------------------------------------------

write_csv(la_venues_geo, here("data", "la_venues_geo.csv"))
