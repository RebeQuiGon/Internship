#Libraries
library(sp)
library(raster)
library(rgdal)
library(sf)

####MAP WHOLE DANMARK###########################################################################

mainDir<-"C:/Users/rqg/Documents/Nedsivningsevne_DK"

if(!dir.exists(file.path(mainDir, "results/Hele_DK"))){ dir.create(file.path(mainDir, "results/Hele_DK")) }

#Infiltration shapefile
models_list<- list.files(file.path(mainDir, "results/Nedsivningsevne_shp"), pattern="shp$", full.names = TRUE)
models_tiles <- lapply(models_list, shapefile) #transform list to polygon list
DK_map <- do.call(bind, models_tiles) #merge the tiles into 1 file

writeOGR(DK_map, file.path(mainDir, "results/Hele_DK"), "Nedsivningsevne", driver = "ESRI Shapefile",
         check_exists=T, overwrite_layer=T)


#Uncertainty
models_list<- list.files(file.path(mainDir, "results/Usikkerhed_shp"), pattern="shp$", full.names = TRUE)
models_tiles <- lapply(models_list, shapefile) #transform list to polygon list
DK_map <- do.call(bind, models_tiles) #merge the tiles into 1 file

writeOGR(DK_map, file.path(mainDir, "results/Hele_DK"), "Usikkerhed", driver = "ESRI Shapefile",
         check_exists=T, overwrite_layer=T)


#Uncertainty simplified
models_list<- list.files(file.path(mainDir, "results/Simplificeret_shp"), pattern="shp$", full.names = TRUE)
models_tiles <- lapply(models_list, shapefile) #transform list to polygon list
DK_map <- do.call(bind, models_tiles) #merge the tiles into 1 file

writeOGR(DK_map, file.path(mainDir, "results/Hele_DK"), "Usik_simplificeret", driver = "ESRI Shapefile",
         check_exists=T, overwrite_layer=T)

#Hydraulic conductivity raster
f <- list.files(file.path(mainDir, "results/Hydraulisk_kvaerdier"), pattern="tif$", full.names = T)

new_list = list()

#load rasters into list
for (i in seq_along(f)){
  new_list[[i]] = raster(f[[i]], band=2)
}


#Give them a NULL name and merge
new_list$fun<-mean
mos <- do.call(mosaic, new_list)

writeRaster(mos, file.path(mainDir,"/results/Hele_DK/Hydraulisk_k.tif"), "GTiff", overwrite=T)



#Infiltration potential raster
f <- list.files(file.path(mainDir, "results/Nedsivningsevne"), pattern="tif$", full.names = T)

new_list = list()

#load rasters into list
for (i in seq_along(f)){
  new_list[[i]] = raster(f[[i]])
}


#Give them a NULL name and merge
new_list$fun<-mean
mos <- do.call(mosaic, new_list)

writeRaster(mos, file.path(mainDir,"/results/Hele_DK/Nedsivningsevne_raster.tif"), "GTiff", overwrite=T)



#### Overview ####
library(gdalUtils)

#Make an ovr for the infiltration potential and hydraulic conductivity maps
gdaladdo(filename = file.path(mainDir, "results/Hele_DK/Nedsivningsevne_raster.tif"), 
         levels = c(2,4,8,16,32,64), r = "mode", ro=T)

gdaladdo(filename = file.path(mainDir, "results/Hele_DK/Hydraulisk_k.tif"), 
         levels = c(2,4,8,16,32,64), r = "mode", ro=T)



