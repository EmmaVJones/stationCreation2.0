library(tidyverse)
library(sf)
library(leaflet)
library(inlmisc)
library(pins)
library(config)

# user inpu
lat <- as.numeric(37.78861)
lng <- as.numeric(-80.00083)
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
  source('snappingFunctions/snapWQS.R')
  # source('preprocessingModules/WQS_lakePoly.R')
} else {
  WQS <- st_as_sf(pin_get('ejones/estuarinePolys', board = 'rsconnect'))
  WQSEL <- st_as_sf(pin_get('ejones/estuarineLines', board = 'rsconnect'))
}

subbasinToVAHU6 <- read_csv('data/subbasinToVAHU6conversion.csv') 

# Highlight HUC station falls into
stationHUC <- st_intersection(point, vahu6) %>%
  left_join(dplyr::select(subbasinToVAHU6, VAHU6, Basin, BASIN_CODE, Basin_Code))

# use snapping tools to get closest WQS for user
time1 <- Sys.time()
WQSout <- snapAndOrganizeWQS(point, 'Name', 
                             st_intersection(st_zm(st_transform(filter(vahu6, VAHU6 %in% stationHUC$VAHU6), st_crs(WQS))), st_zm(WQS)), # only send necessary WQS to function for snapping
                             bufferDistances = seq(20,80,by=20),  # buffering by 20m from 20 - 80 meters
                             tibble(StationID = NA, WQS_ID = NA, `Buffer Distance` = NA))
Sys.time() - time1

time2 <- Sys.time()
WQSsegments <- st_intersection(st_buffer(point %>% st_transform(st_crs(WQS)), 1000),
                               st_zm(WQS)) %>%
  st_transform(102003)

Sys.time() - time2
time3 <- Sys.time()
WQSout2 <- snap_Point_to_Feature(point %>% st_transform(102003), 'Name',
                      WQSsegments, # only send necessary WQS to function for snapping
                      bufferDistances = seq(20,80,by=20))
Sys.time() - time3

vafrm_HUC <- vafrm99_05[stationHUC,]
