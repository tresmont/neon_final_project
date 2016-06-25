####################
# functions to extract individual points/pixels from raster stack
library(dplyr)
source("scripts/bufferPoints.R" )
# function to subset the inventory data by plotid
# expects the shape from a single plot; point or poly
getPlotData <- function(plotShp) {
  if(length(plotShp) > 1){
    stop("Requires a single poly or point")
  }
  # extract a single point we think is a tree 
  subPlotData <- inventory %>% filter(plotid == as.character(plotShp$id))
  return(subPlotData)
}

# change the inventory data to a shape
coords_2_shape <- function(subPlotData, proj4Str){
  x <- subPlotData$easting
  y <-  subPlotData$northing
  # plotAttr <- dplyr::select(subPlotData, -c(easting, northing))
  coords <- as.data.frame(cbind(x,y))
  
  stemPoints <- SpatialPointsDataFrame(coords, 
                                       proj4string = CRS(proj4Str),
                                       data = data.frame(subPlotData))
  return(stemPoints)
}

# calculate a buffer using the ground-measured max canopy diameter
bufferMaxCanopy <- function(site, stemPoints) {
  maxCanopy <- stemPoints$maxcanopydiam
  plotId <- stemPoints$plotid[1]
  # print(plotId)
  bufferPoly(site = site, stemPoints, type = "plot", plotID = plotId, radius = maxCanopy)
}


extractRasterPixel <- function(inRasterSubset, subPlotData) {
  if(missing(inRasterSubset) | missing(subPlotData)) {
    stop("missing required data")
  }
  
}