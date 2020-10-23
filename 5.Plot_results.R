############################PLOT RESULTS##################################
#
#
#This script extracts and plots projection results from the modeling stage
#Any questions or improvements can be emailed to Ximena Tagle: xtagle@iiap.gob.pe
#
#
####


# Load libraries
pacman::p_load(raster, tidyverse, mapview)



# Set directory that has biomod results
processing_directory <- "~/Bamboo-SDM/" #"G:/My Drive/CIB/3. Resultados/Scripts/Bamboo-SDM"


biomod_dir <- paste0(processing_directory, "/Results/biomod")


# Pass a species name or a genus name, and raster stack that contains 
# that string in the name will be returned
load_data <- function(sp, directory) {
  projection_stack <- directory %>% 
    list.files(recursive = TRUE, pattern = "img$", full.names = TRUE) %>% 
    str_subset(sp) %>% 
    stack()
  
  return(projection_stack)
}

# Peru border (to be used for cropping)
peru <- processing_directory %>% 
  paste0("/Data/Shapefiles/GADM_2.8_PER_adm0.rds") %>% 
  read_rds()


### Load everything
all_sps <- load_data("", biomod_dir)

#plot(all)

# Check what is within the raster stack
# !! Outputs from ensemble models are:
# - EMmeanByTSS -- Mean value of multiple models
# - EMwmeanByTSS -- Mean value of multiple models weighted by TSS value (better models weigh more)
# - ensemble.x -- One of the models that go into the ensemble
# - .x -- individual model projections

### Mask weighted mean projection to the boundary of Peru

#Select all the weighted mean layers from the stack
all_EMwmeanByTSS <- raster::subset(all_sps, grep('EMwmeanByTSS_merged', names(all_sps), value = T))
names(all_EMwmeanByTSS)

#Mask
all_peru_wmean <- mask(all_EMwmeanByTSS, peru)
#plot(all_peru_wmean)

#Export
for (i in 1:length(all_peru_wmean))   
{
  writeRaster(all_peru_wmean[[i]],paste0("../predictions_wmean/",
                                substr(names(all_peru_wmean[[i]]),7,43),".tif"),
              options="COMPRESS=NONE", overwrite=T)  
}


#################### Check the model assessment and variable importance

csvs_folder <- paste0(processing_directory, "/Results/csv")

singel_model_assessment <- csvs_folder %>% 
  list.files(recursive = TRUE, pattern = "Model_assessment.*.csv", full.names = TRUE) %>% 
  map(read_csv) %>% 
  reduce(rbind)


ensemble_assessment <- csvs_folder %>% 
  list.files(recursive = TRUE, pattern = "Ensemble_model_assessment.*.csv", full.names = TRUE) %>% 
  map(read_csv) %>% 
  reduce(rbind)

variable_importance <- csvs_folder %>% 
  list.files(recursive = TRUE, pattern = "varimp.*.csv", full.names = TRUE) %>% 
  map(read_csv) %>% 
  reduce(bind_rows)




#################################################### Loading only some species/genus  #####

####
# Load data 
Ahaenkei <- load_data("Aulonemia.haenkei", biomod_dir)
plot(Ahaenkei)

Cpicta <- load_data("Chusquea.picta", biomod_dir)
plot(Ahaenkei)

Phyllostachys <- load_data("Phyllostachys", biomod_dir)
names(Phyllostachys)
  

# Load data related with Bambusa genus
bambusa <- load_data("Bambusa", biomod_dir)
plot(bambusa)
names(bambusa)


#Calcs
Guadua_mean <- mean(Guadua)
Guadua_median <- raster::calc(Guadua, fun = median)

plot(Guadua_mean)
plot(Guadua_median)

# Mask weighted mean projection to the boundary of Peru
Guadua_peru_wmean <- mask(Guadua[[2]], peru)
plot(Guadua_peru_wmean)

writeRaster(Guadua_peru_wmean, "Guadua_peru_wmean.tif")


Phyllostacys_peru_wmean <- mask(Phyllostachys[[14]], peru)
writeRaster(Phyllostacys_peru_wmean, "../predictions_wmean/Genus/Phyllostacys_EMwmeanByTSS_merged.tif")

############### 
Cpicta_mean <- mean(Cpicta)
Cpicta_median <- raster::calc(Cpicta, fun = median)

plot(Cpicta_mean)
plot(Cpicta_median)

# Mask weighted mean projection to the boundary of Peru
Cpicta_peru_wmean <- mask(Cpicta[[2]], peru)

plot(Cpicta_peru_wmean)

writeRaster(Cpicta_peru_wmean, "Cpicta_peru_wmean.tif")