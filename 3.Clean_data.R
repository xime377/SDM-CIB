############################RARIFY OCCURENCES##################################
#
#
#This script rarifies occurence points to a 1 kilometer grid
#Any questions or improvements can be emailed to Ximena Tagle: xtagle@iiap.gob.pe
#
#
####

###SET WDIR
setwd("~/Bamboo-SDM/") #Guanabana
#setwd("G:/My Drive/CIB/3. Resultados/BDD")

## Get packages
pacman::p_load(spocc, sf, mapview, lubridate, dplyr, readxl, raster, sf)

###Load info
bambu_species_raw <- read_excel("BDD_26OCT2020.xlsx", sheet = 2)


##List of species to model

bambu_species_to_model <- c("Arthrostylidium simpliciusculum",
                            "Aulonemia david-smithii",
                            "Aulonemia haenkei",
                            "Aulonemia hirtula",
                            "Aulonemia longiaristata",
                            "Aulonemia queko",
                            "Bambusa vulgaris",
                            "Chusquea barbata",
                            "Chusquea scandens",
                            "Chusquea uniflora",
                            "Cryptochloa unispiculata",
                            "Guadua angustifolia",
                            "Guadua glomerata",
                            "Guadua lynnclarkiae",
                            "Guadua macrospiculata",
                            "Guadua sarcocarpa",
                            "Guadua superba",
                            "Guadua takahashiae",
                            "Guadua weberbaueri",
                            "Olyra standleyi",
                            "Phyllostachys aurea",
                            "Rhipidocladum harmonicum",
                            "Rhipidocladum racemiflorum",
                            "Aulonemia humillima",
                            "Aulonemia parviflora",
                            "Aulonemia rubraligulata",
                            "Aulonemia yanachagensis",
                            "Bambusa oldhamii",
                            "Bambusa tuldoides",
                            "Chusquea acuminata",
                            "Chusquea acuminatissima",
                            "Chusquea angusta",
                            "Chusquea aristata",
                            "Chusquea aspera",
                            "Chusquea decolorata",
                            "Chusquea delicatula",
                            "Chusquea depauperata",
                            "Chusquea elata",
                            "Chusquea exasperata",
                            "Chusquea falcata",
                            "Chusquea gamarrae",
                            "Chusquea huantensis",
                            "Chusquea intipaqarity",
                            "Chusquea longipedicellata",
                            "Chusquea neurophylla",
                            "Chusquea peruviana",
                            "Chusquea picta",
                            "Chusquea polyclados",
                            "Chusquea rugoloana",
                            "Chusquea smithii",
                            "Chusquea spicata",
                            "Chusquea stuebelii",
                            "Chusquea tarmensis",
                            "Chusquea tovarii",
                            "Dendrocalamus asper",
                            "Gigantochloa apus",
                            "Merostachys brevispica",
                            "Olyra latifolia",
                            "Pariana bicolor",
                            "Pariana campestris",
                            "Pariana concinna",
                            "Pariana gracilis",
                            "Pariana interrupta",
                            "Pariana radiciflora",
                            "Pariana sociata",
                            "Pariana stenolemma",
                            "Pariana swallenii",
                            "Pariana ulei",
                            "Pariana velutina",
                            "Rhipidocladum arenicola",
                            "Rhipidocladum maxonii")

#### Make spatial
bambu_species_sf <- bambu_species_raw %>% 
  filter(complete.cases(Latitud)) %>% # Get data with coordinates
  filter(Latin %in% bambu_species_to_model) %>% # Get only species of interest
  transmute(ID = ID,
            Species = Latin, # Subset and rename columns
            Genus = Genus,
            Latitude = Latitud,
            Longitude = Longitud,
            Ref = Fuente,
            Source = Source_type) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) # Convert into simple feature

bambu_species_sp <- as(bambu_species_sf, "Spatial") #Convert into sp


#### Get species list
bambu_species_list <- bambu_species_raw %>% 
  as.data.frame() %>% 
  dplyr::select(Latin) %>% 
  unique() %>% 
  arrange(Latin)


###

table(bambu_species_sf$Species)

bambu_species_empty <- bambu_species_sp[FALSE, ]


#### Rarify point data so there is a single occurrence for each cell

# Load raster file that should be used as a mask
raster_mask <- raster("./Data/Predictors/dem_resampled.tif")
raster_grid <- as(raster_mask, "SpatialGrid")

crs(raster_grid) <- proj4string(bambu_species_sp)


for (species in unique(bambu_species_sp$Species))
{
  bambu_species_single <- bambu_species_sp[bambu_species_sp$Species == species, ]
  
  
  bambu_species_single$grid <- over(bambu_species_single, raster_grid)
  gridlist <- split(bambu_species_single, bambu_species_single$grid)
  # Take one point per grid
  samples <- lapply(gridlist, function(x) x[sample(1:nrow(x), 1, FALSE),])
  # Bind those rows back together in a new data frame
  sampledgrid <- do.call(rbind, samples)
  bambu_species_empty <- rbind(bambu_species_empty, sampledgrid)
}


bambu_species_cleared <- st_as_sf(bambu_species_empty)


#Export as a geopackage
st_write(bambu_species_cleared, "./Data/Shapefiles/Bambu_filtered261020_genus.gpkg")
