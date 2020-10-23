##################### Script to explore the bamboo data ########################

###set WD
setwd("D:/Google Drive/CIB/")


###Load 
library(dplyr)
library(sf)
library(stringr)
library(broom)
library(readr)
library(mapview)

### Read csv
bambD<-read.csv("./BDD/BDDAgo.csv", header=T)

###Inspect data
str(bambD)    #See structure
head(bambD)   #See first 6

length(bambD) #Number of variables read
nchar(bambD$Latitud) #determine if they have coordinates

###Plot points
species_location <- bambD %>% 
  filter(complete.cases(Latitud)) %>%  #filter the ones with coordinates
  transmute(Species = Latin,
            Latitude = Latitud,
            Longitude = Longitud,
            Source = Source)

head(species_location)


LocP <- st_as_sf(species_location, coords = c("Longitude", "Latitude"), crs = 4326)
mapview(LocP, zcol = "Species") #visualization

#dir.create("./Output/Mapa_SDM_prel_Ago17")  #create new folder
st_write(LocP,"./Output/Mapa_SDM_prel_Ago17/BLocation.shp") #Export as shp

###Summarize location points by species and source
sp_lisTS <- table(LocP$Species, LocP$Source)
write.csv(sp_lisTS, "./Output/Mapa_SDM_prel_Ago17/Bamb_summary.csv") #export summary


###Summarize location points by species
sp_listT <- tidy(table(LocP$Species))

sp_listT <- sp_listT %>% 
  arrange(desc(Freq)) %>% 
  transmute(Species = Var1, Count = Freq)

write.csv(sp_listT, "./Output/Mapa_SDM_prel_Ago17/Bamb_sumTotal.csv") #export summary





