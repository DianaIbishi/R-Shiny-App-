# Install packages -------------

install.packages("ggmap")
library(ggmap)

install.packages('osmdata')
library(osmdata)

# First get the map of Kosovo with leaflet (allows interactive maps)

library(leaflet)

# Coordinates for Kosovo's approximate center
kosovo_lat <- 42.6026
kosovo_lng <- 20.9028

# Create the map
Kosovo_map <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap tiles
  setView(lng = kosovo_lng, lat = kosovo_lat, zoom = 8) 


# New add an outline to highlight the country

kosovo_geojson <- "https://github.com/datasets/geo-boundaries-world-110m/countries/kosovo"

leaflet() %>%
  addTiles() %>%  # Add default map tiles
  addGeoJSON(kosovo_geojson, 
             color = "black", 
             weight = 10, 
             fillOpacity = 5) %>% 
  setView(lng = kosovo_lng, 
          lat = kosovo_lat, 
          zoom = 8) 













