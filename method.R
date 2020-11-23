library(tidyverse)
library(sf)
library(leaflet)
library(inlmisc)
library(pins)
library(config)

# user inpu
lat <- as.numeric(37.8984)
lng <- as.numeric(-79.90876)
stationType <- c('Riverine', 'Lacustrine', 'Estuary')[1] # choose 1
 
# make a spatial object from lat/long
point <- tibble(Name = 'User Station', Latitude = lat, Longitude = lng) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), 
           remove = F, # don't remove these lat/lon cols from df
           crs = 4326) # add projection, needs to be geographic for now bc entering lat/lng

# Pull down spatial data on server
conn <- config::get("connectionSettings") # get configuration settings

vahu6 <- st_as_sf(pin_get("ejones/vahu6", board = "rsconnect"))

# appropriate WQS layer
if(stationType %in% c('Riverine', 'Lacustrine')){
  WQS <- st_as_sf(pin_get(paste0('ejones/', tolower(stationType)), board = 'rsconnect'))
} else {
  WQS <- st_as_sf(pin_get('ejones/estuarinePolys', board = 'rsconnect'))
  WQSEL <- st_as_sf(pin_get('ejones/estuarineLines', board = 'rsconnect'))
}

subbasinToVAHU6 <- read_csv('data/subbasinToVAHU6conversion.csv') 

# Highlight HUC station falls into
stationHUC <- st_intersection(point, vahu6) %>%
  left_join(dplyr::select(subbasinToVAHU6, VAHU6, Basin, BASIN_CODE, Basin_Code))

# use snapping tools to get closest WQS for user
wqs_HUC <- st_intersection(point, WQS)

vafrm_HUC <- vafrm99_05[stationHUC,]
