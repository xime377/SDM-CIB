############################RETRIEVE DATA FROM GBIF##################################
#
#
#This script retrieves additional GBIF data for the bamboo database
#Any questions or improvements can be emailed to Ximena Tagle: xtagle@iiap.gob.pe
#
#
# Things done by the script:
# - Read and clear bamboo data from the CIB database
# - Get additional data from GBIF for species that are modeled 
# - Merge the datasets and rarify occurence points to a 1 kilometer grid
####

###SET WDIR
setwd("G:/My Drive/CIB/3. Resultados/Scripts") #Xime PC



## Get packages
pacman::p_load(spocc, sf, mapview, lubridate, dplyr, readxl, raster)



bambu_species_raw <- read_excel("BDD_27OCT19.xlsx", sheet = 5)


#### Clean up names
bambu_species_raw <- bambu_species_raw %>% 
  mutate(
    Species = case_when(
      Latin == "Bambusa vulgaris var. vittata" | Latin == "Bambusa vulgaris var. vulgaris" ~ "Bambusa vulgaris",
      Latin == "Phyllostachis aurea" ~ "Phyllostachys aurea",
      TRUE ~ as.character(Latin)
    )
  )


#### Select species to model
#######
bambu_species_to_model <- c("Aulonemia david-smithii",
                            "Aulonemia haenkei",
                            "Aulonemia hirtula",
                            "Aulonemia humillima",
                            "Aulonemia longiaristata",
                            "Aulonemia parviflora",
                            "Aulonemia queko",
                            "Aulonemia rubraligulata",
                            "Aulonemia sp.",
                            "Aulonemia yanachagensis",
                            "Bambusa oldhamii",
                            "Bambusa sp.",
                            "Bambusa vulgaris",
                            "Chusquea acuminata",
                            "Chusquea acuminatissima",
                            "Chusquea angusta",
                            "Chusquea aristata",
                            "Chusquea aspera",
                            "Chusquea barbata",
                            "Chusquea decolorata",
                            "Chusquea delicatula",
                            "Chusquea depauperata",
                            "Chusquea elata",
                            "Chusquea exasperata",
                            "Chusquea falcata",
                            "Chusquea gamarrae",
                            "Chusquea huantensis",
                            "Chusquea intipaqariy",
                            "Chusquea longipedicellata",
                            "Chusquea neurophylla",
                            "Chusquea peruviana",
                            "Chusquea picta",
                            "Chusquea polyclados",
                            "Chusquea rugoloana",
                            "Chusquea scandens",
                            "Chusquea smithii",
                            "Chusquea sp.",
                            "Chusquea spicata",
                            "",
                            "",
                            "",
                            "Chusquea uniflora",
                            "Cryptochloa sp.",
                            "Cryptochloa unispiculata",
                            "",
                            "",
                            "Guadua angustifolia",
                            "",
                            "Guadua sp.",
                            "",
                            "Guadua takahashiae",
                            "Guadua weberbaueri",
                            "",
                            "",
                            "",
                            "Olyra standleyi",
                            "",
                            "",
                            "",
                            "",
                            "",
                            "",
                            "",
                            "",
                            "",
                            "",
                            "",
                            "",
                            "",
                            "Pharus virescens",
                            "Phyllostachys aurea",
                            "",
                            "",
                            "Rhipidocladum harmonicum",
                            "",
                            "Rhipidocladum racemiflorum",
                            "")



Olyraf_key <- name_backbone(name = "Olyra fasciculata")$speciesKey
Olyraf_df <- occ_search(taxonKey = Olyraf_key, return = "data", hasCoordinate = T)
Olyraf<- cbind(Olyraf_df$name,Olyraf_df$decimalLongitude,Olyraf_df$decimalLatitude, Olyraf_df$stateProvince, Olyraf_df$country, Olyraf_df$bibliographicCitation)
#
Olyral_key <- name_backbone(name = "Olyra latifolia")$speciesKey
Olyral_df <- occ_search(taxonKey = Olyral_key, return = "data", hasCoordinate = T)
Olyral<- cbind(Olyral_df$name,Olyral_df$decimalLongitude,Olyral_df$decimalLatitude, Olyral_df$stateProvince, Olyral_df$country, Olyral_df$bibliographicCitation)

##
Bambu<- data.frame(rbind(Aulonemiaq, Bambusalo, Bambusavu, Chusquab, Chusquad, Chusquae, Chusquap, Chusquas,
                    Chusquasm, Chusquau,Cryptochloau, Dendrocalamas, Guaduaw, Guadual, Guaduat,
                    Olyras, Pharusv, Phyllostachys, Rhipidocladumh, Rhipidocladumr, Olyraf, Olyral)) #without Guaduaa and Neurolepsiss
names(Bambu)<-c("latin","lon","lat","Prov", "Country", "link")

head(Bambu)
tail(Bambu)

Bambu<-Bambu[(Bambu$Country=="Peru"),] #select only Peru records


#remove NAs
row_index <-- which(is.na(Bambu))
Bambu <- Bambu[row_index,]

bambu_df<-data.frame(Bambu)
coordinates(Bambu) <- ~ lon+lat

proj4string(Bambu) <- CRS("+proj=longlat +ellps=WGS84") #Projects shape

data(wrld_simpl)
plot(wrld_simpl, xlim=c(-80,-60), ylim=c(-18,0), axes=TRUE, 
     col='light yellow')
points(Bambu$lon, Bambu$lat, col='red', cex=0.75)

write.csv(Bambu,"Bambu_GBIF.csv")


