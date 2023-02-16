###************************###
### Functions for HOTspots ###
###************************###
# Project aim: tracking antimicrobial resistance in the top end
#
# Author: Alys Young
#
# Collaborators: Saras Windecker, Nick Golding
#
# Date: 6-March-2021
#
# Script aim: Functions used in the HOTspots shiny app

##***********##
## 1. Set up ## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##***********##

# if(!require(ggplot2)) install.packages(ggplot2)
# if(!require(dplyr)) install.packages(dplyr)
# 
# library(ggplot2)
# library(dplyr)

##************##
## 2. ggplots ## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##************##

##************##
## 3. Leaflet ## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##************##


leaflet_empty <- function(data_shp, zoom_min=2, leg_values = 0:100, leg_title = "% resistance", leg_locate = "bottomright", col_pal = pal_num){
  
  data_bbox <- summary(data_shp)$bbox
  
  data_shp %>% # using the shapefile data
    leaflet() %>% # create a leaflet map
    fitBounds(lng1 = data_bbox[1], lat1 = data_bbox[2], lng2 = data_bbox[3], lat2 = data_bbox[4]) %>% # the starting position of the map
    addTiles(options = providerTileOptions(minZoom = zoom_min)) %>% # sets the furtherst that can be zoomed out
    addLegend(leg_locate, pal = col_pal, values = ~c(leg_values), 
              title = leg_title, # legend title
              opacity = 1)
}

##*****************##
## 4. Manipulation ## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##*****************##


