####################
# functions to extract individual points/pixels from raster stack

# function to subset the inventory data by plotid
# expects the shape from a single plot; point or poly
getPlotData <- function(plotShp) {
  if(length(plotShp > 1)){
    stop("Requires a single poly or point")
  }
  # extract a single point we think is a tree 
  subPlotData <- inventory %>% filter(plotid == plotShp$id)
  return(subPlotData)
}

# change the inventory data to a shape
coords_2_shape <- function(subPlotData){
  x <- subPlotData$easting
  y <-  subPlotData$northing
  
}

extractRasterPixel <- function(inRasterSubset, subPlotData) {
  if(missing(inRasterSubset) | missing(subPlotData)) {
    stop("missing required data")
  }
  
}