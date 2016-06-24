## A set of functions to...
## This script will sort through a directory of raster files 
## to determine what files are within a particular study AOI extent
## Author: Leah A. Wasser
## Last Modified: 4 May 2016

### Required packages ####
# install.packages(c("raster","rgdal","rgeos"))
library(raster)
library(rgdal)
library(rgeos)

# specify the working DIR

# setwd("~/Documents/data/1_spectrometerData/Teakettle")
setwd("H:\\1_Data-Institute-subsets\\")

## Check Extent Function ####
# this function below checks to see if a raster falls within a spatial extent
# inputs: raster file to check, clipShp (spatial )
checkExtent <- function(aRaster, clipShp){
  # create polygon extent assign CRS to extent 
  rasterExtPoly <- as(extent(aRaster), "SpatialPolygons")
  crs(rasterExtPoly) <-  crs(aRaster)
  
  # check to see if the polygons overlap
  # return a boolean (1= the raster contains pixels within the extent, 0 it doesn't)
  return(gIntersects(clipShp, rasterExtPoly))
}


## This function checks to see if a raster falls within the extent
## if it's in the extent, it then records the name of the file

rastInClip <- function(rasterFile, clipShp) {
  # rasterFile <- "/Volumes/data_dr/teakettle/DTM/2013_TEAK_1_326000_4103000_DTM.tif"
  # rasterFile <- rasterList[221]
  recordRaster <- NA
  aRaster <- raster(rasterFile)
  if (checkExtent(aRaster, clipShp)) {
    recordRaster <- rasterFile
  }
  return(recordRaster)
}


## Get Aop Tiles ####
# This fun takes a list of file names (gtif), a clipping extent and an output
# gtif name to create a mosaic and write a gtif. 
get_AOP_tiles <- function(tileDir, clipExtent, outFileName){
  message(paste0("Now running ", tileDir[1]))
  # get list of files from the server
  rasterList <- list.files(tileDir, full.names = TRUE, pattern = "\\.tif$")
  
  # create a boolean list of rasters in the extent window == 1
  finalList <- unlist(lapply(rasterList, rastInClip, clipShp = clipExtent))
  # remove NA and get the final list of rasters to mosaic
  finalList <- finalList[!is.na(finalList)]
  
  # take the list of rasters, and create a mosaic
  rast.list <- list()
  for(i in 1:length(finalList)) { rast.list[i] <- stack(finalList[i]) }
  
  # mosaic rasters
  # note removed () from the function max call for windows
  rast.list$fun <- max
  rast.mosaic <- do.call(mosaic, rast.list)
  # plot(rast.mosaic)
  
  # crop final mosaic to clip extent
  rast.mosaic <- crop(rast.mosaic, clipExtent)
  
  # write geotiff
  writeRaster(rast.mosaic,
              filename=outFileName,
              format="GTiff",
              options="COMPRESS=LZW",
              overwrite = TRUE,
              NAflag = -9999,
              datatype="FLT4S") # ensure it's writing to 32 bit
  # for those who want to plot the final raster to check it out
  # return(rast.mosaic)
}

### run code ####

#dataDir <- file.path("P:","Distros","1.3a+WaveForm_V1.1a",
#                     "1.3a","D17",
#                     "SJER","2013","SJER_L3",
#                     "SJER_Lidar")



# ##### OSBS Clip
# # the name of the site
# site <- "OSBS"
# domain <- "D3"
# fullDomain <- "D03-Florida"
# level <- "L3"
# dataType <- "lidar"
# level <- paste0(site,"_L3")
# year <- "2014"
# productType <- paste0(site,"_Lidar")

# ##### JERC Clip
# # the name of the site
# site <- "JERC"
# domain <- "D3"
# fullDomain <- "D03-Florida"
# level <- "L3"
# dataType <- "lidar"
# level <- paste0(site,"_L3")
# year <- "2014"
# productType <- paste0(site,"_Lidar")
# clipFile <- "JERC_crop"

##### HARV Clip
# the name of the site
# site <- "HARV"
# domain <- "D01"
# fullDomain <- "D01-Massachusetts"
# level <- "L3"
# dataType <- "lidar"
# level <- paste0(site,"_L3")
# year <- "2014"
# productType <- paste0(site,"_Lidar")

##### SJER Clip
# # the name of the site
# site <- "SJER"
# domain <- "D17"
# fullDomain <- "D03-Florida"
# level <- "L3"
# #level <- "L2"
# year <- "2013"

##### TEAK Clip
# the name of the site
# site <- "TEAK"
# domain <- "D17"
# fullDomain <- paste0(domain,"-California")
# level <- "L3"
# year <- "2013"

##### SOAP Clip
# the name of the site
site <- "SOAP"
domain <- "D17"
fullDomain <- paste0(domain,"-California")
level <- "L3"
year <- "2013"

## variables that won't change
dataType <- "lidar"
level <- paste0(site,"_", level)
productType <- paste0(site,"_Lidar")


#### Distro Path Location
# 

#### Distro Path Location
# 
driveLetter <- "P:"
theDir <- "Distros"
distroVersion <- "1.4a_No_Waveform"


####
dataDir <- file.path(driveLetter, theDir, distroVersion, domain,
                     site, year, level, productType)


# clip file name
clipFile <- paste0(site,"_crop")
# specify the path to the clip file
clipFilePath <- file.path("H:","1_Data-Institute-subsets","NEONdata",fullDomain, site, "vector_data")


# where you want to save the outputs
# outputDir <- "/Users/lwasser/Documents/data/1_data-institute-2016/Teakettle/may1_subset/subset/lidar"
outputDir <- file.path("H:","1_Data-Institute-subsets", fullDomain, site, year, tolower(dataType))

# automate parsing through all lidar subdirs to grab data
tileDir <- list.dirs(dataDir,
                     full.names = TRUE, 
                     recursive = FALSE)

# just for SJER - comment out after this run

#dataDir <- "X:/flight/FullSite/2013/FullSite/D17/2013_SJER_1/L3/DiscreteLidar/CanopyHeightModel_gtif"

#outputDir <- file.path("H:","1_Teakettle", SJER)
#tileDir <- dataDir

# create a list of output filenames
outNames <- paste0("H:/1_Data-Institute-subsets/NEONdata/",fullDomain,"/",site, 
                   "/",year,"/",dataType,"/", 
                   site, "_", dataType, 
                   basename(tileDir),".tif")



# import shapefile to spatial polygon
clipExtent <- readOGR(clipFilePath, clipFile)

## write output mosaics ####
# the code below calls a function that grabs all of the gtifs within a clipping
# extent from each directory specified in the list. it should then
# write an output gtif

#mapply(get_AOP_tiles, tileDir, 
#       clipExtent = clipExtent, 
#       outFileName = outNames)

# mapply is still acting odd so using a for loop

for(i in 1:length(tileDir)){
  get_AOP_tiles(tileDir = tileDir[i], outFileName = outNames[i], clipExtent=clipExtent)
}

# rast.mosaic.round <- trunc(rast.mosaic, prec=4)

#writeRaster(rast.mosaic.round,
#            filename="rounded.tif",
#            format="GTiff",
#            options="COMPRESS=LZW",
#            overwrite = TRUE,
#            NAflag = -9999,
#            datatype="FLT4S")
