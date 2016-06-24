## This script will sort through a directory of raster files 
## to determine what files are within a particular study AOI extent

# install.packages(c("raster","rgdal","rgeos"))
library(raster)
library(rgdal)
library(rgeos)

# specify the dir that you wish to find rasters
# dataDir <- "/Volumes/data_dr/teakettle/lidar/CHM"
# dataDir <- "P:\\Distros\\1.3a+WaveForm_V1.1a\\1.3a\\D17\\TEAK\\2013\\TEAK_L1\\TEAK_Camera\\Images"

# DTM Dir
dataDir <- "P:\\Distros\\1.3a+WaveForm_V1.1a\\1.3a\\D17\\TEAK\\2013\\TEAK_L3\\TEAK_Lidar\\DTM"
# specify the output file name
outFile <- "Teak_lidarDTM.tif"

# DSM Dir
# dataDir <- "P:\\Distros\\1.3a+WaveForm_V1.1a\\1.3a\\D17\\TEAK\\2013\\TEAK_L3\\TEAK_Lidar\\DSM"
# specify the output file name
# outFile <- "Teak_lidarDSM.tif"


# dataDir <- "P:\\Distros\\1.3a+WaveForm_V1.1a\\1.3a\\D17\\TEAK\\2013\\TEAK_L3\\TEAK_Lidar\\aspect"
# specify the output file name
# outFile <- "Teak_lidarAspect.tif"

# dataDir <- "P:\\Distros\\1.3a+WaveForm_V1.1a\\1.3a\\D17\\TEAK\\2013\\TEAK_L3\\TEAK_Lidar\\slope"
# specify the output file name
# outFile <- "Teak_lidarSlope.tif"

# fixed CHM data
# dataDir <- "Y:\\users\\tgoulden\\TEAK\\L3\\DiscreteLidar\\CanopyHeightModelGtif"
#Y:\users\tgoulden\TEAK\L3\DiscreteLidar\CanopyHeightModel



# specify the clipping file name
# clipFile <- "teakettle_clip"
clipFile <- "teakettleClip_2"
#specify the path to the clip file
clipFilePath <- "H:\\1_Teakettle"
# specify the working DIR

# setwd("~/Documents/data/1_spectrometerData/Teakettle")
setwd("H:\\1_Teakettle\\new_subset\\lidar")

# import shapefile
clippingExtent <- readOGR(clipFilePath, clipFile)

# the function below checks to see if a raster falls within an clipping extent
checkExtent <- function(raster, clipShp){
  # create polygon extent assign CRS to extent 
  rasterExtPoly <- as(extent(raster), "SpatialPolygons")
  crs(rasterExtPoly) <-  crs(clippingExtent)
  
  # check to see if the polygons overlap
  # return a boolean
  return(gIntersects(clippingExtent,rasterExtPoly))
}s

get_AOP_tiles <- function(tileDir, clipExtent, outFileName){
  
}

# get list of files from the server
rasterList <- list.files(dataDir, full.names = TRUE, pattern = "\\.tif$")

# the function below checks to see if a raster falls within an clipping extent
checkExtent <- function(raster, clipShp){
  # create polygon extent assign CRS to extent 
  rasterExtPoly <- as(extent(raster), "SpatialPolygons")
  crs(rasterExtPoly) <-  crs(clippingExtent)
  
  # check to see if the polygons overlap
  # return a boolean
  return(gIntersects(clippingExtent,rasterExtPoly))
}


rastInClip <- function(rasterFile) {
  # rasterFile <- "/Volumes/data_dr/teakettle/DTM/2013_TEAK_1_326000_4103000_DTM.tif"
  # rasterFile <- rasterList[221]
  recordRaster <- NA
  aRaster <- raster(rasterFile)
  if (checkExtent(aRaster)) {
    recordRaster <- rasterFile
  }
  return(recordRaster)
}

# create a list of only rasters in the extent window
finalList <- unlist(lapply(rasterList, rastInClip))

# remove NA and get the final list of rasters to mosaic!
finalList <- finalList[!is.na(finalList)]

# take the list of rasters, and create a mosaic
rast.list <- list()
for(i in 1:length(finalList)) { rast.list[i] <- stack(finalList[i]) }

# mosaic rasters
# note removed () from the function max call
rast.list$fun <- max
rast.mosaic <- do.call(mosaic, rast.list)
# plot(rast.mosaic)

# crop final mosaic
rast.mosaic <- crop(rast.mosaic, clippingExtent)

# write geotiff
writeRaster(rast.mosaic,
            filename=outFile,
            format="GTiff",
            options="COMPRESS=LZW",
            overwrite = TRUE,
            NAflag = -9999)
