
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

rastInClip <- function(rasterFile,clipShp) {
  # rasterFile <- "/Volumes/data_dr/teakettle/DTM/2013_TEAK_1_326000_4103000_DTM.tif"
  # rasterFile <- rasterList[221]
  # recordRaster <- NA
  suppressWarnings(aRaster <- raster(rasterFile))
  # for now skip rotated lines
  if(rotated(aRaster)){
    print("raster rotation detected - skipping")
    # remove rotation - because this doesn't currently work, commenting it out
    # aRaster2 <- rectify(aRaster, res=1, method="ngb")
    # crs(aRaster2) <- crs(clipShp)
    # return(recordRaster)
  #}
  }else{
  if(compareCRS(aRaster,clipShp)==FALSE){
    #reproject if the data aren't in the same projection
    print("fixing projection of clip extent to match raster data")
    clipShp <- spTransform(clipShp, crs(aRaster))
  } 
    
  if (checkExtent(aRaster, clipShp)) {
    # recordRaster <- rasterFile
    # crop raster
    aRaster <- crop(aRaster, clipShp)
    return(aRaster)
  }
   # return(recordRaster)  
  }
}


## Get Aop Tiles ####
# This fun takes a list of file names (gtif), a clipping extent and an output
# gtif name to create a mosaic and write a gtif. 
# updating this to accept a list of rasters rather than a dir to allow for 
# multiple products in a dir.
get_AOP_tiles <- function(rasterList, clipExtent, outFileName){
  message(paste0("Now running ", basename(rasterList[1])))
  # get list of files from the server
  # rasterList <- list.files(tileDir, full.names = TRUE, pattern = "\\.tif$")
  
  # create a boolean list of rasters in the extent window == 1
  finalList <- lapply(rasterList, rastInClip, clipShp = clipExtent)
  # remove NA and get the final list of rasters to mosaic
  finalList <- finalList[!sapply(finalList,is.null)]
  
  # # create list of rasters
  # rast.list <- list()
  # for(i in 1:length(finalList)) { rast.list[i] <- stack(finalList[i]) }
  # for(i in 1:length(new)) { rast.list[i] <- stack(new[i]) }
  
  # 
  # crop each raster in list - not sure if it's faster to crop now or after mosaicking
  # rast.list <- lapply(rast.list, crop, clipExtent)
  # mosaic rasters
  # note removed () from the function max call for windows
  finalList$fun <- max
  finalList$tolerance <- .5
  rast.mosaic <- do.call(mosaic, finalList)
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
