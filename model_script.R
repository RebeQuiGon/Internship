#OBS! Update mainDir with the folder where the project is located

#Load libraries
library(sp)
library(raster)
library(rgdal)
library(sf)
library(SpaDES)
library(xlsx)
library(data.table)


#############################################################################################
#Create the main directories
mainDir<-"C:/Users/rqg/Documents/Nedsivningsevne_DK"

modelDir<- dirname(rstudioapi::getSourceEditorContext()$path)
nameDir<-sapply(strsplit(modelDir, split='/', fixed=TRUE), tail, 1)


#Create a folder to save "checkpoints" and results

if(!dir.exists(file.path(mainDir, "results"))){ dir.create(file.path(mainDir, "results")) }
if(!dir.exists(file.path(paste0(mainDir,"/checkpoints/",nameDir)))){ dir.create(file.path(paste0(mainDir,"/checkpoints/",nameDir))) }

############DATA PREPARATION########################################################################
####Clay thickness###############################################################

####Load the data####
#1. Load the legend with the GeoEnhedID and the names
legend<-list.files(file.path(modelDir), pattern=".xlsx", full.names = T)
legend<-read.xlsx(legend, 1, header=T)

#2. Get the number of those layers that have "Ler" or "ler" in their name
ler_layers<-legend[legend$Navn %like% "ler",]
Ler_layers<-legend[legend$Navn %like% "Ler",]
ler_table<-rbind(ler_layers,Ler_layers)

Geoenhed_ler<- ler_table$GeoEnhedID

#3. Using the numbers, create a list that contains the whole name of the layers
ler_names <- sapply(paste0(modelDir, "/geo-enhed-", Geoenhed_ler,".tif"), min)

#4. Load the files 
ler_top<-stack(ler_names, bands=1)
ler_bund<-stack(ler_names, bands=2)

#Repeat for the sand layers
Sand_layers<-legend[legend$Navn %like% "Sand",]
sand_layers<-legend[legend$Navn %like% "sand",]
sand_table<-rbind(Sand_layers,sand_layers)

Geoenhed_sand<- sand_table$GeoEnhedID
sand_names <- sapply(paste0(modelDir, "/geo-enhed-", Geoenhed_sand,".tif"), min)

sand_top<-stack(sand_names, bands=1)
sand_bund<-stack(sand_names, bands=2)


####Sand layers####
#Saet crs
crs(sand_top) <- CRS('+init=EPSG:25832')
crs(sand_bund) <- CRS('+init=EPSG:25832')
crs(ler_top) <- CRS('+init=EPSG:25832')
crs(ler_bund) <- CRS('+init=EPSG:25832')

#Set origin sand
t=length(sand_top@layers)

for (x in 1:t) {
  origin(sand_top[[x]])<-0
  origin(sand_top)<-0
}
for (x in 1:t) {
  origin(sand_bund[[x]])<-0
  origin(sand_bund)<-0
}

#Set origin clay
t=length(ler_top@layers)

for (x in 1:t) {
  origin(ler_top[[x]])<-0
  origin(ler_top)<-0
}

for (x in 1:t) {
  origin(ler_bund[[x]])<-0
  origin(ler_bund)<-0
}

#1. Calculate sand thickness
sand_thickness<- stack(sand_top-sand_bund)

#2. Exclude sand thicker than 1m 
sand_1m<-stack(sand_thickness>=1)

#3. Reclasiffy 0 to NA
#set an empty rasterstack to save the data
n<-nlayers(sand_top) #number of rasters in the stack
r<-raster(extent(sand_top[[1]]), crs= '+init=EPSG:25832') #extent & crs
res(r)=res(sand_top[[1]]) #resolution
values(r)<- NA  #set values

sand_NA <- stack(replicate(n, r)) #replicate the empty raster into the stack

##3.1. Reclassify 0 to NA
reclass_df <- matrix(c(0, NA,
                       1, 1),
                     ncol=2, byrow = T)

for(i in 1:n){
  sand_NA[[i]]=reclassify(sand_1m[[i]],reclass_df)
}

#4. Calculate the top depht of each sand layer
sand_dybt<-stack(sand_NA*sand_top)

#5. Overlay the layers keeping the highest value
b <- brick(sand_dybt)

sand_max<-stackApply(b, indices = rep(1, nlayers(b)), fun = max)

#6. Reclassify NA as -999 (computational issues)
reclass_sand <- matrix(c(NA,-999),
                       ncol=2, byrow = T)

sand_alle <- reclassify(sand_max, reclass_sand)

#Checkpoint
writeRaster(sand_alle, file.path(paste0(mainDir,"/checkpoints/",nameDir,"/",nameDir,"_sand.tif")), "GTiff", overwrite=TRUE)


####Clay thickness map####
#1. Calculate clay thickness
ler_tyk<-stack(ler_top-ler_bund)

#2. Find clay over sand (get a bitmap) 
ler_over<-stack(ler_top>=sand_alle)

#3. Clip the clay to the bitmap
ler_tyk_over<-stack(ler_over*ler_tyk)

#4. Sum up the clay layers, get total clay thickness 
ler_b<-brick(ler_tyk_over)
lertykkelse<-stackApply(ler_b, indices = rep(1, nlayers(ler_b)), fun = sum)

#5. Clip the map to the model's area
mask_list<- list.files(file.path(modelDir, "modelomraade"), pattern="shp$", full.names = TRUE)
mask<- readOGR(mask_list)

ler_alle <- crop(lertykkelse, extent(mask))
ler_alle<- mask(ler_alle, mask)

#Checkpoint
writeRaster(ler_alle, file.path(paste0(mainDir,"/checkpoints/",nameDir,"/",nameDir,"_lertykkelse.tif")), "GTiff", overwrite=TRUE)


####Rasterize soil types (jordartskort) & morphological map###################################
#Get data
jordart_list<- list.files(file.path(mainDir, "Input/Jordartskort"), pattern="shp$", full.names = TRUE)
morfkort_list<- list.files(file.path(mainDir, "Input/Morfologisk_kort"), pattern="shp$", full.names = TRUE)

jord<- st_read(jordart_list)
morfologisk<-st_read(morfkort_list)

#Get legends
jordtsym_list<-list.files(file.path(mainDir, "Input/Legend/Jord_nedsiv"), pattern=".xlsx", full.names = T)
jordtsym<-read.xlsx(jordtsym_list, 1, header=T, encoding = 'UTF-8')

morf_list<-list.files(file.path(mainDir, "Input/Legend/Morf_nedsiv"), pattern=".xlsx", full.names = T)
morf_range<-read.xlsx(morf_list, 1, header=T, encoding = 'UTF-8')

#1. Fix the geometries
jord_ret<-st_make_valid(jord)
morf_ret<-st_make_valid(morfologisk)

#2. Combine maps and legends after the TSYM column
jord_ranged <- merge(jord_ret, jordtsym, by="tsym", all=FALSE)  # merge by row names

column<-names(morf_range)
names(morf_range)[names(morf_range) == column[1]] <- "Landskabse"

morf_ranged<- merge(morf_ret, morf_range, by="Landskabse", all.x=T, all.y=F)

#3. Make a mask to clip the layers using the clay thickness map
crs(ler_alle)<-CRS('+init=EPSG:25832')
coord<- matrix(c(extent(ler_alle)))

##Coords: (x_min, y_min), (x_max, y_min), (x_max, y_max), (x_min, y_max), (x_min, y_min)
pol<- st_sfc(st_polygon(list(cbind(c(coord[[1]]@xmin, coord[[1]]@xmax, coord[[1]]@xmax, coord[[1]]@xmin, coord[[1]]@xmin),
                                   c(coord[[1]]@ymin, coord[[1]]@ymin, coord[[1]]@ymax, coord[[1]]@ymax, coord[[1]]@ymin)))))
mask2<- st_sf(pol, crs = 25832) #set crs of the mask

jord_model<-st_intersection(jord_ranged,mask2) #intersect jordsartskort with the mask
morf_model<-st_intersection(morf_ranged,mask2) #intersect the morfologisk kort with the mask

#4. checkpoint, just in case
st_write(jord_model, file.path(mainDir,"checkpoints", nameDir), paste0(nameDir,"_jord.shp"),
         driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=T, append = F)
st_write(morf_model, file.path(mainDir,"checkpoints", nameDir), paste0(nameDir,"_morf.shp"), 
         driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=T, append = F)

#5. Rasterize
r<-raster(extent(ler_alle), crs= '+init=EPSG:25832') ##make an empty raster as clay thickness
res(r) = res(ler_alle) #pixel size

jordv<-jord_model$Vaerdi #vector with the values that will be transferred to the raster
jordart_raster <- rasterize(jord_model,r, jordv) #rasterize shp to raster

jordv2<-jord_model$Band1
jord_ID<- rasterize(jord_model,r,jordv2) #this one is for the potential map

morfv<-morf_model$Vaerdi #vector with values to be transferred to the raster
morf_raster<-rasterize(morf_model,r, morfv) #rasterize shp to raster

#6. Clip to model's extent
jordart <- crop(jordart_raster, extent(mask))
jordart<- mask(jordart, mask)

jordID <- crop(jord_ID, extent(mask))
jordID<- mask(jordID, mask)

morf_final <- crop(morf_raster, extent(mask))
morf_final<- mask(morf_final, mask)

writeRaster(jordart, file.path(paste0(mainDir,"/checkpoints/",nameDir,"/",nameDir,"_jordart.tif")), "GTiff", overwrite=TRUE)
writeRaster(jordID, file.path(paste0(mainDir,"/checkpoints/",nameDir,"/",nameDir,"_jordID.tif")), "GTiff", overwrite=TRUE)
writeRaster(morf_final, file.path(paste0(mainDir,"/checkpoints/",nameDir,"/",nameDir,"_morf.tif")), "GTiff", overwrite=TRUE)


####Groundwater Raster resampling###########################################################
#Get raster & set crs
tngvs_list<- list.files(file.path(mainDir, "Input/Grundvand"), pattern="tif$", full.names = TRUE)

tngvs<-raster(tngvs_list)

crs(tngvs)<- CRS('+init=EPSG:25832')

#1. Resample to ler_classified extent and resolution
# (resolution 25x25, dimensions 993, 1365, 1355445  (nrow, ncol, ncell))

gvand<- resample(tngvs, ler_alle, method="ngb")

gvand<-crop(gvand,extent(mask))
gvand<-mask(gvand, mask)

writeRaster(gvand, file.path(paste0(mainDir,"/checkpoints/",nameDir,"/",nameDir,"_grundvand.tif")), "GTiff", overwrite=TRUE)


######INFILTRATION POTENTIAL MAP ################################################################
####Hydraulic conductivity map###############################################
#1. Reclassify clay thickness to hydraulic conductivity in moraines
reclass_mor <- matrix(c(-1, 1, 5,
                        1, 2, 4,
                        2, 3, 3,
                        3, Inf, 2),
                      ncol=3, byrow = T)

ler_mor <- reclassify(ler_alle, reclass_mor)

#2. Reclassify soil types to infiltration potential based on their TSYM ID
reclass_morrains<- matrix(c(0,42, 0,
                            42,43, 1,
                            43, Inf, 0),
                          ncol=3, byrow=T) 
morrains<-reclassify(jordID,reclass_morrains) #bitmap for moraines (ML)

reclass_rest<- matrix(c(0,42, 1,
                        42,43, 0,
                        43, Inf, 1),
                      ncol=3, byrow=T)
non_morrain<- reclassify(jordID, reclass_rest) #bitmap for the other types

morrains_ler<-morrains*ler_mor #potential for clay in moraines
jord_rest<-non_morrain*jordart #potential for other soil types
jordart_all<-morrains_ler+jord_rest #potential for all soil types


#Reclassify to hydraulic conductivity
kvaerdi<- matrix(c(6, 1e-2,
                   5, 1e-4, 
                   4, 1e-5, 
                   3, 1e-7, 
                   2, 1e-8, 
                   1, 1e-10), 
                 ncol=2, byrow=T)

hydraulisk_k <- reclassify(jordart_all, kvaerdi, include.lowest=F)


#NA til 0 (computation issues)
NA_reclass<- matrix(c(NA, 0),
                    ncol=2, byrow = T)

jordart_all<- reclassify(jordart_all, NA_reclass)


#2. Checkpoints
writeRaster(morrains_ler, file.path(paste0(mainDir,"/checkpoints/",nameDir,"/",nameDir,"_morrain_ler.tif")), "GTiff", overwrite=TRUE)
writeRaster(jord_rest, file.path(paste0(mainDir,"/checkpoints/",nameDir,"/",nameDir,"_non_morrain.tif")), "GTiff", overwrite=TRUE)
writeRaster(jordart_all, file.path(paste0(mainDir,"/checkpoints/",nameDir,"/",nameDir,"_jordart_all.tif")), "GTiff", overwrite=TRUE)

#3. Combine in a 5 band raster and save

if(!dir.exists(file.path(mainDir, "results/Hydraulisk_kvaerdier"))){ dir.create(file.path(mainDir, "results/Hydraulisk_kvaerdier")) }
if(!dir.exists(file.path(mainDir, "results/Hydraulisk_multiband"))){ dir.create(file.path(mainDir, "results/Hydraulisk_multiband")) }

hydraulisk_multi<-stack(ler_mor, jordID, jordart, jordart_all, hydraulisk_k)
hydraulisk_2band<-stack(jordart_all, hydraulisk_k)

writeRaster(hydraulisk_multi, file.path(paste0(mainDir,"/results/hydraulisk_multiband/",nameDir,"_hydraulisk_multi.tif")), "GTiff", overwrite=T)
writeRaster(hydraulisk_2band,file.path(paste0(mainDir,"/results/hydraulisk_kvaerdier/",nameDir,"_hydraulisk_2band.tif")), "GTiff", overwrite=TRUE )

####Infiltration potential map########################################################
#1. Reclassify clay to infiltration potential
reclass_ler <- matrix(c(-1, 1, 6,
                        1, 5, 5,
                        5, 10, 4,
                        10, 15, 3,
                        15, Inf, 2),
                      ncol=3, byrow = T)

ler_classified <- reclassify(ler_alle, reclass_ler)

#2. Reclassify groundwater to infiltration potential
reclass_gv<-matrix(c(-1, 0.5, 6,
                     0.5, Inf, 1,
                     NA, NA, 0),
                   ncol=3, byrow=T)

grundvand<- reclassify(gvand, reclass_gv)

#3. Combine soil types, clay and groundwater, keeping the lowest value of the three
b1 <- brick(ler_classified,jordart_all,grundvand)

nedsivningsevne<- stackApply(b1, indices = rep(1, nlayers(b1)), fun = min)

NA_back<- matrix(c(0, NA),
                 ncol=2, byrow = T) #Reclassify 0 to NA, visualization purposes

nedsivningsevne<-reclassify(nedsivningsevne, NA_back)

#4. Stack resulting rasterfiles together (4 band raster)
nedsivningsevne_multi<- stack(jordart_all,ler_classified,grundvand,nedsivningsevne)

if(!dir.exists(file.path(mainDir, "results/Nedsivningsevne_multiband"))){ dir.create(file.path(mainDir, "results/Nedsivningsevne_multiband")) }
if(!dir.exists(file.path(mainDir, "results/Nedsivningsevne"))){ dir.create(file.path(mainDir, "results/Nedsivningsevne")) }

writeRaster(nedsivningsevne_multi, file.path(paste0(mainDir,"/results/Nedsivningsevne_multiband/",nameDir,"_nedsiv_multi.tif")), "GTiff", overwrite=T)
writeRaster(nedsivningsevne, file.path(paste0(mainDir,"/results/Nedsivningsevne/",nameDir,"_nedsivningsevne.tif")), "GTiff", overwrite=T)


####UNCERTAINTY MAP##############################################################################
#Groundwater uncertainty
##Reclassify morphology and groundwater
reclass_morf<-matrix(c(NA, -1),
                     ncol=2, byrow=T) #computational issues, will give 0 if overlayed with the groundwater

reclass_usik<-matrix(c(-1, 0.39, 0,
                       0.39, 0.6, 1,
                       0.6, Inf, 0),
                     ncol=3, byrow=T)

gvand_usikk<- reclassify(gvand, reclass_usik)
morf_usikk<- reclassify(morf_final, reclass_morf)

##Sum up gvand +  morfologi
usikkerhed<-gvand_usikk+morf_usikk

##Reclassify -1 values to 0
reclass_usik<-matrix(c(-1, 0),
                     ncol=2, byrow=T)
usikkerhed<-reclassify(usikkerhed, reclass_usik)


if(!dir.exists(file.path(mainDir, "results/Usikkerhed"))){ dir.create(file.path(mainDir, "results/Usikkerhed")) }

writeRaster(usikkerhed, file.path(paste0(mainDir,"/results/Usikkerhed/",nameDir,"_usikkerhed.tif")), "GTiff", overwrite=T)


####POLYGON MAP###################################################################################
rm(list=ls())

mainDir<-"C:/Users/rqg/Documents/Nedsivningsevne_DK"

modelDir<- dirname(rstudioapi::getSourceEditorContext()$path)
nameDir<-sapply(strsplit(modelDir, split='/', fixed=TRUE), tail, 1)

#Get data
nedsivningsevne<- raster(file.path(paste0(mainDir,"/results/Nedsivningsevne/",nameDir,"_nedsivningsevne.tif")))
usikkerhed <- raster(file.path(paste0(mainDir, "/results/Usikkerhed/",nameDir,"_usikkerhed.tif")))

#Create folders for the tiles
if(!dir.exists(file.path(mainDir, "checkpoints/shapes"))){ dir.create(file.path(mainDir, "checkpoints/shapes")) }
if(!dir.exists(file.path(mainDir, "checkpoints/shapes/nedsivningsevne"))){ dir.create(file.path(mainDir, "checkpoints/shapes/nedsivningsevne")) }
if(!dir.exists(file.path(mainDir, "checkpoints/shapes/usikkerhed"))){ dir.create(file.path(mainDir, "checkpoints/shapes/usikkerhed")) }
if(!dir.exists(file.path(mainDir, "checkpoints/shapes/simplificeret"))){ dir.create(file.path(mainDir, "checkpoints/shapes/simplificeret")) }


####Infiltration potential####
##2.1. Split into tiles and remove older tiles
nx<-3
ny<-3

f <- list.files(file.path(mainDir, "checkpoints/split/nedsivningsevne"), include.dirs = F, full.names = T, recursive = T)
file.remove(f)

neds<- splitRaster(nedsivningsevne, nx, ny, path=file.path(mainDir,"checkpoints/split/nedsivningsevne"))

#Delete unnecessary files
f <- list.files(file.path(mainDir, "checkpoints/shapes/nedsivningsevne"), include.dirs = F, full.names = T, recursive = T)
file.remove(f)

#Exclude tiles with only NA values
f <- list.files(file.path(mainDir, "checkpoints/split/nedsivningsevne"), 
                pattern="grd$", full.names = T)

new_list = list()
i= list()

for (x in seq_along(f)){ 
  new_list[[x]] = raster(f[[x]])
  i[[x]] <- cellStats(is.na(new_list[[x]]), sum) #get number of NA cells
  i[[x]] <- i[[x]]/ncell(new_list[[x]]) #divide by total cell number
}                                       #if all cells are NA, x = 1

df<-as.data.frame(unlist(i))
names_tiles<- as.data.frame(f)

df<- cbind(df, names_tiles)

column<-names(df)
names(df)[names(df) == column[1]] <- "V1"
names(df)[names(df) == column[2]] <- "V2"

no_na<-df[df$V1 < 1,] #<1 means tiles have values, not just NA 
no_na_names<-no_na$V2 #get file paths for no NA tiles

neds<-list()
for (x in seq_along(no_na_names)){
  neds[[x]] <- raster(no_na_names[[x]]) #load tiles in list
}

#Now poligonize only those rasters with values in them (otherwise we get an error)
t<-length(neds)

for (m in 1:t){
  pol <- rasterToPolygons(neds[[m]], fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=TRUE)
  names<-paste0("vector_tile_", m)
  proj4string(pol) <- CRS("+init=EPSG:25832")
  print(paste0("Way to go! ", m, " tiles transformed!"))
  writeOGR(pol, file.path(mainDir, "checkpoints/shapes/nedsivningsevne"), 
           names, driver="ESRI Shapefile", delete_dsn=TRUE) #gem shp filer i "shape" mappe
}

#Bind the tiles of each map to "reform" them
neds_list<- list.files(file.path(mainDir, "checkpoints/shapes/nedsivningsevne"), 
                       pattern="shp$", full.names = TRUE)
neds_tiles <- lapply(neds_list, shapefile) #transform list to polygon list
neds_map <- do.call(bind, neds_tiles) #merge the tiles into 1 file
column<- names(neds_map)
names(neds_map)[names(neds_map) == column] <- "nedsivning"

#Save results
if(!dir.exists(file.path(mainDir, "results/Nedsivningsevne_shp"))){ dir.create(file.path(mainDir, "results/Nedsivningsevne_shp")) }

writeOGR(neds_map, file.path(mainDir, "results/Nedsivningsevne_shp"), 
         paste0(nameDir,"_nedsivningsevne"), driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=T)



####Uncertainty map####
## Split into tiles and remove older tiles
nx<-3
ny<-3

f <- list.files(file.path(mainDir, "checkpoints/split/usikkerhed"), include.dirs = F, full.names = T, recursive = T)
file.remove(f)

usik<- splitRaster(usikkerhed, nx, ny, path = file.path(mainDir, "checkpoints/split/usikkerhed"))

#Remove old sph files
f <- list.files(file.path(mainDir, "checkpoints/shapes/usikkerhed"), include.dirs = F, full.names = T, recursive = T)
file.remove(f)

#Exclude rasters with only NA values
f <- list.files(file.path(mainDir, "checkpoints/split/usikkerhed"), pattern="grd$", full.names = T)

new_list = list()
i= list()

for (x in seq_along(f)){
  new_list[[x]] = raster(f[[x]])
  i[[x]] <- cellStats(is.na(new_list[[x]]), sum)
  i[[x]] <- i[[x]]/ncell(new_list[[x]])
}

df<-as.data.frame(unlist(i))
names_tiles<- as.data.frame(f)

df<- cbind(df, names_tiles)

column<-names(df)
names(df)[names(df) == column[1]] <- "V1"
names(df)[names(df) == column[2]] <- "V2"

no_na<-df[df$V1 < 1,]
no_na_names<-no_na$V2

usik<-list()
for (x in seq_along(no_na_names)){
  usik[[x]] <- raster(no_na_names[[x]])
}

#Now poligonize only those rasters with values in them (otherwise we get an error)
t<-length(usik)

for (m in 1:t){
  pol <- rasterToPolygons(usik[[m]], fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=TRUE)
  names<-paste0("vector_tile_", m)
  proj4string(pol) <- CRS("+init=EPSG:25832")
  print(paste0("Way to go! ", m, " tiles transformed!"))
  writeOGR(pol, file.path(mainDir, "checkpoints/shapes/usikkerhed"), names, driver="ESRI Shapefile") #gem shp filer i "shape" mappe
}

##Bind the tiles back together
usik_list<- list.files(file.path(mainDir, "checkpoints/shapes/usikkerhed"), pattern="shp$", full.names = TRUE)
usik_tiles <- lapply(usik_list, shapefile) #transform list to polygon list
usik_map <- do.call(bind, usik_tiles) #merge the tiles into 1 file
column <- names(usik_map)
names(usik_map)[names(usik_map) == column] <- "usikkerhed"

##Usikkerhedskort: add extra column with "high/medium/low" 
usik_map$skala <- ifelse(usik_map$usikkerhed >= 0 & usik_map$usikkerhed <= 2, 'low',
                         ifelse(usik_map$usikkerhed >=3 & usik_map$usikkerhed <=4, 'medium',
                                ifelse(usik_map$usikkerhed >=5, 'high', 'something else')
                         )
)

if(!dir.exists(file.path(mainDir, "results/Usikkerhed_shp"))){ dir.create(file.path(mainDir, "results/Usikkerhed_shp")) }

#Save result
writeOGR(usik_map, file.path(mainDir, "results/Usikkerhed_shp"), 
         paste0(nameDir,"_usikkerhed"), driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=T, )



####Uncertainty simplified ####
##Simplify the map, better rendering
reclass_usik<-matrix(c(-1, 0, 1,
                       0, 3, NA,
                       3, 7, 1),
                     ncol=3, byrow=T)

simplificeret<-reclassify(usikkerhed, reclass_usik)

#Clean folder with older tiles and split again
f <- list.files(file.path(mainDir, "checkpoints/split/simplificeret"), include.dirs = F, full.names = T, recursive = T)
file.remove(f)

nx<-3
ny<-3 

simple_tiles<- splitRaster(simplificeret, nx, ny, path=file.path(mainDir,"checkpoints/split/simplificeret"))

#Clean the folder with the shp tiles
f <- list.files(file.path(mainDir, "checkpoints/shapes/simplificeret"), include.dirs = F, full.names = T, recursive = T)
file.remove(f)

#Exclude rasters with only NA values
f <- list.files(file.path(mainDir, "checkpoints/split/simplificeret"), pattern="grd$", full.names = T)

new_list = list()
i= list()

for (x in seq_along(f)){
  new_list[[x]] = raster(f[[x]])
  i[[x]] <- cellStats(is.na(new_list[[x]]), sum)
  i[[x]] <- i[[x]]/ncell(new_list[[x]])
}

df<-as.data.frame(unlist(i))
names_tiles<- as.data.frame(f)

df<- cbind(df, names_tiles)

column<-names(df)
names(df)[names(df) == column[1]] <- "V1"
names(df)[names(df) == column[2]] <- "V2"

no_na<-df[df$V1 < 1,]
no_na_names<-no_na$V2

simplificeret_tiles<-list()
for (x in seq_along(no_na_names)){
  simplificeret_tiles[[x]] <- raster(no_na_names[[x]])
}

#Now poligonize only those rasters with values in them (otherwise we get an error)
t<-length(simplificeret_tiles)

for (m in 1:t){
  pol <- rasterToPolygons(simplificeret_tiles[[m]], fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=TRUE)
  names<-paste0("vector_tile_", m)
  proj4string(pol) <- CRS("+init=EPSG:25832")
  print(paste0("Way to go! ", m, " tiles transformed!"))
  writeOGR(pol, file.path(mainDir, "checkpoints/shapes/simplificeret"), names, driver="ESRI Shapefile") #gem shp filer i "shape" mappe
}

##Bind the tiles back together
simple_list<- list.files(file.path(mainDir, "checkpoints/shapes/simplificeret"), pattern="shp$", full.names = TRUE)
simp_tiles <- lapply(simple_list, shapefile) #transform list to polygon list
simple_map <- do.call(bind, simp_tiles) #merge the tiles into 1 file
column<- names(simple_map)
names(simple_map)[names(simple_map) == column] <- "usikkerhed"

#Gem resultat
if(!dir.exists(file.path(mainDir, "results/Simplificeret_shp"))){ dir.create(file.path(mainDir, "results/Simplificeret_shp")) }

writeOGR(simple_map, file.path(mainDir, "results/Simplificeret_shp"), 
         paste0(nameDir,"_simplificeret"), driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=T)



###############




