############################Plot bamboo distribution maps##################################
#
#
#This script plots the web maps of the potential distribution of the different bamboo species in Peru
#Any questions or improvements can be emailed to Ximena Tagle: xtagle@iiap.gob.pe
#
#
####

###SET WDIR
#setwd("~/Bamboo-SDM/") #Guanabana
setwd("G:/My Drive/CIB/3. Resultados/") #Xime PC


processing_directory <- "G:/My Drive/CIB/3. Resultados/Output" #"~/Bamboo-SDM/"
predictions_dir <- paste0(processing_directory, "/Mapa_SDM_Oct20/")

###LOAD LIBRARIES
library("sf")
library("mapview")
library("RColorBrewer")
# library("stars")
library(raster)
library(dplyr)
library(leafem)
library(leaflet)
library(png)
library(raster)
library(stringr)


###LOAD RASTERS
#Function to load tifs
load_data <- function(sp, directory) {
  projection_stack <- directory %>% 
    list.files(recursive = TRUE, pattern = "tif$", full.names = TRUE) %>% 
    str_subset(sp) %>% 
    stack()
  return(projection_stack)
}

###Load all the predictions
all_sps <- load_data("", paste0(predictions_dir, "/Species/"))
all_genus <- load_data("", paste0(predictions_dir, "/Genus/"))
names(all_genus)

###If loading only some rasters  
#Aulonemia<- raster("./Output/Mapa_SDM_prel_Ago17/Bambu_1maps/Aulonemia_hirtula.tif")
#Chusquea<- read_stars("./Output/Mapa_SDM_prel_Ago17/Bambu_1maps/Chusquea_sp.tif")
#Guadua<- raster("./Output/Mapa_SDM_prel_Nov19/Guadua_sp.tif")


###Data exploration
summary(all_sps)
summary(all_sps[[40]])

#summary(Guadua)
#summary(Aulonemia)

###Select the probability values higher than 10%
all_sps[all_sps<=100] <- NA # Select only values higher than 100 (10%)
all_genus[all_genus<=100] <- NA 

#Aulonemia[Aulonemia<=100] <- NA 
#Guadua<- Guadua[Guadua<=100] <- NA 
#Stack layers
#all_S<-stack(Aulonemia, Guadua)


###LOAD POINTS
P_bamboo<-st_read("./Scripts/Bamboo-SDM/Data/Shapefiles/Bambu_filtered261020_genus.gpkg")
#P_bamboo<-st_read("./Data/Shapefiles/Bambu_filtered181020_genus.shp")

###LOAD MEMBRETE
membrete <- paste0(predictions_dir, "/membrete.png")


###PLOT MAP
#View only bamboo records
mapView(P_bamboo, legend=T, #add points
        map.types=c("Stamen.Terrain", "CartoDB.Positron", "Esri.WorldImagery", "OpenStreetMap", "OpenTopoMap")) #add basemaps


bambu_species <- P_bamboo %>% 
  mutate(
    longitude = st_coordinates(.)[, 1],
    latitude = st_coordinates(.)[, 2]
  ) %>% 
  st_set_geometry(NULL)

# Rescale values to 0-1
my_map <- all_sps[[1]] / 1000 
# Aggregate map for easier loading 
my_map_agg <- aggregate(my_map, fact = 10)

# 
bambu_species_df <- split(bambu_species, bambu_species$Species)
basemap <- leaflet() %>% addTiles(group = "OpenStreetMap") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
  addProviderTiles(providers$Stamen.Terrain, group = "Terrain") 
  
  

### Create df so it can be visualized per species

# In case that you want just to show points instead of 
# point clusters, remove the clusterOptions line
names(bambu_species_df) %>%
  purrr::walk( function(df) {
    basemap <<- basemap %>%
      addCircleMarkers(data=bambu_species_df[[df]],
                 lng=~longitude, lat=~latitude,
                 weight = 2,
                 radius = 4,
                 label=~as.character(Species),
                 #popup=~as.character(Species),
                 popup= popupTable(bambu_species_df[[df]][2:3]),
                 group = df,
                 clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
                 labelOptions = labelOptions(noHide = F,
                                             direction = 'auto'))
  })


# Select species occurences on map (By default G. angustifolia)
basemap %>% 
  #addProviderTiles("OpenStreetMap") %>% 
  addRasterImage(my_map_agg,
                 # colors = pal,
                 group = "Raster",
                 opacity = 0.8) %>%
  leaflet.extras::addResetMapButton()  %>% 
  addScaleBar(position = "bottomright") %>% 
  addLogo(membrete, src = "local",
           position = "bottomleft",
           offset.x = 5,
           offset.y = 5,
           width = 380,
           height = 160) %>%
  addLayersControl(
    baseGroups = c("OpenStreetMap", "World Imagery", "Terrain"),
    overlayGroups = c("Raster",names(bambu_species_df)),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup(stringr::str_remove(names(bambu_species_df), "Guadua angustifolia"))



#################
# Map all points and raster
leaflet::leaflet(data = bambu_species) %>% 
  #addProviderTiles("OpenStreetMap") %>% 
  addCircleMarkers(
    radius = 10, # size of the dots
    fillOpacity = .7, # alpha of the dots
    stroke = FALSE, # no outline
    # popup = ~htmlEscape(name),
    group = "Points",
    clusterOptions = markerClusterOptions(),
    #popup = ~htmltools::htmlEscape(Species)) %>% 
    popup= popupTable(bambu_species[2:4]))%>% 
  addRasterImage(my_map_agg, 
                 # colors = pal, 
                 group = "Raster",
                 opacity = 0.8) %>%
  addRasterImage(my_map_agg, 
                 # colors = pal, 
                 group = "Raster",
                 opacity = 0.8) %>%
  leaflet.extras::addResetMapButton()  %>% 
  addScaleBar(position = "bottomright") %>% 
  addLogo(paste0(predictions_dir, "/membrete.png"), src = "local",
          position = "bottomleft",
          offset.x = 5,
          offset.y = 5,
          width = 380,
          height = 160) %>% 
  leaflet::addLayersControl(overlayGroups = c("Raster", "Points"),
                            position = "topright",
                            options = layersControlOptions(collapsed = FALSE)) 

