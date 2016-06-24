###############################
# script modified from http://neondataskills.org/HDF5/Plot-Hyperspectral-Pixel-Spectral-Profile-In-R/

#first call required libraries
library(rhdf5)
library(plyr)
library(ggplot2)
library(neonAOP)


## ----open-H5-file--------------------------------------------------------

#Define the file name to be opened
# f <- 'SJER_140123_chip.h5'

mapInfo_2_proj <- function(f, type=c("proj", "epsg")){
  if(missing(f)){
    stop("File name is required")
  }
  if(missing(type)){
    stop("Type of proj is required")
  }
  mapInfo <- h5read(f,
                    "map info",
                    read.attributes = TRUE)
  # split the mapInfo object on the ","
  mapInfo <- unlist(strsplit(mapInfo, ","))
  
  # produce the required output
  type <- match.arg(type, c("proj", "epsg"))
  
  if(type == "proj"){
    outproj <- sprintf("+proj=%s +zone=%s +datum=%s +units=%s +no_defs +ellps=%s +towgs", tolower(mapInfo[1]), #projection
                    mapInfo[8], #zone
                    mapInfo[10], #datum
                    tolower(unlist(strsplit(mapInfo[11], "="))[2]), #units
                    mapInfo[10]) #ellipsoid
  }
  if(type == "epsg"){
    outproj <-  as.numeric(paste0(326, mapInfo[8]))  
  }
  return(outproj)
}

getWavelengths <- function(fileName){
  if(missing(fileName)){
    stop("File name is required")
  }
  
  # read in the wavelength information from the HDF5 file
  wavelengths<- h5read(fileName, "wavelength")
  
  # convert wavelength to nanometers (nm)
  wavelengths <- wavelengths * 1000

  # return the object
  return(wavelengths)
}

allBands <- function(fileName) {
  if(missing(fileName)){
    stop("File name is required")
  }
  
  # get the length of bands from wavelengths
  wavelengths <- getWavelengths(fileName)
  
  # get all the bands
  bands <- c(1:length(wavelengths))
  
  # create a projection string
  epsg <- mapInfo_2_EPSG(fileName)
  
  # get stack
  all.bands.stack <- create_stack(fileName, 
                                bands,
                                epsg)

  # get spectra for each band
  # spectra <- extract_av_refl(all.bands.stack, 
  #                          aFun = mean)
  # spectra <- as.data.frame(spectra)
  # return(spectra)
}



###############################################
# Code below modified from https://github.com/lwasser/neon-create-aop-subset

####################### Check Extent Function ###########################

# this function checks to see if a raster falls within a spatial extent
# inputs: 
# h5.extent: the spatial extent of the h5 file
# clipShp: a spatial polygon object
checkExtent <- function(h5.extent, clipShp){
  # create polygon extent assign CRS to extent 
  h5.extent.sp <- as(h5.extent, "SpatialPolygons")
  
  # note this is ASSUMING both the extent and the h5 file are in the same CRS
  crs(rasterExtPoly) <-  crs(clip.polygon)
  
  # check to see if the polygons overlap
  # return a boolean (1= the raster contains pixels within the extent, 0 it doesn't)
  return(gIntersects(h5.extent.sp, clip.polygon))
}

################ Write Extent Shapefiles Function ########################

# Inputs:
# f: a path to an h5 file that you'd like an extent for.
# shpDir: path to the output directory where you want to store the data
# projf4Str: the proj4 formated string of the CRS that the H5 file is in.
# NOTE: proj4 NEEDS to be in the same proj as your h5 file
# write_shapefile_bound <- function(f, shpDir, proj4Str){
#   # create shapefileName
#   # output
#   h5.extent <- create_extent(f)
#   # create polygon extent assign CRS to extent 
#   h5.extent.sp <- as(h5.extent, "SpatialPolygons")
#   # create data.frame, add the name of the file to the shapefile
#   sp.df <- data.frame(id=basename(f))
#   sp.obj <- SpatialPolygonsDataFrame(h5.extent.sp, sp.df)
#   # assign CRS
#   crs(sp.obj) <- CRS(proj4Str)
#   # create shapefile output name
#   outName <- gsub(pattern = ".h5",
#                   x = basename(f),
#                   replacement = "")
#   writeOGR(sp.obj, 
#            shpDir, #path to export to
#            outName,
#            driver="ESRI Shapefile",
#            overwrite_layer = TRUE)
# }

##################### Run Export Polygon Boundary for Each Flightline ##############

# # export extent polygon for all flightlines
# proj4Str <- "+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs"
# shpDir <- paste0("exports/", site, "_flightLines")
# 
# sapply(h5.files, write_shapefile_bound,
#        proj4Str = proj4Str,
#        shpDir = shpDir)

#################### End Export Polygon Boundary for Each Flightline ###########




##################### Run Find Flightlines that Intersect With Spatial Extent ##############
#
# initalize counter and list object

# h5.files: a list of h5 files (full paths) to review
# clip.polygon: the clipping polygon spatial object that you want to find the h5 files that intersect it 

find_intersect_h5 <- function(h5.files, clip.polygon){
  recordRaster <- NA
  i <- 0
  # the loop below returns a LIST of the files that have overlapping extent
  for(afile in h5.files){
    # get extent of h5 file
    h5Extent <- create_extent(afile)
    # turn into polygon extent object
    h5.poly <- as(h5Extent, "SpatialPolygons")
    # this is assuming both are in the same CRS!
    crs(h5.poly) <-  crs(clip.polygon)
    
    # check to see if the polygons overlap
    if(gIntersects(h5.poly, clip.polygon)){
      i <- i+1
      recordRaster[i] <- afile
    } else {
      print("not in")
    }
  } 
  return(recordRaster)
}


# # plot spectra
# qplot(x=spectra$wavelength,
#       y=spectra$spectra,
#       xlab="Wavelength (nm)",
#       ylab="Reflectance",
#       main="Spectra for all pixels",
#       ylim = c(0, .35))


## ----extract-spectra-----------------------------------------------------

#extract Some Spectra from a single pixel
# aPixel<- h5read(f,"Reflectance",index=list(54,36,NULL))
# 
# #reshape the data and turn into dataframe
# b <- adply(aPixel,c(3))
# 
# #create clean data frame
# aPixeldf <- b[2]
# 
# 
# #add wavelength data to matrix
# aPixeldf$Wavelength <- wavelengths
# 
# head(aPixeldf)
# 
# 
# 
# ## ----pull-scale-factor---------------------------------------------------
# 
# #grab scale factor
# scaleFact <- reflInfo$`Scale Factor`
# 
# #add scaled data column to DF
# aPixeldf$scaled <- (aPixeldf$V1/scaleFact)
# 
# #make nice column names
# names(aPixeldf) <- c('Reflectance','Wavelength','ScaledReflectance')
# head(aPixeldf)
# 
# 
# ## ----plot-spectra--------------------------------------------------------
# 
# qplot(x=aPixeldf$Wavelength, 
#       y=aPixeldf$ScaledReflectance,
#       xlab="Wavelength (nm)",
#       ylab="Reflectance")
# 
# loadHDF <- function(fileName) {
#   if (missing(fileName)) {
#     stop("File name is required")
#   }
#   
#   #look at the HDF5 file structure 
#   h5ls(f,all=T) 
#   
#   
#   ## ----read-spatial-attributes---------------------------------------------
#   
#   #r get spatialInfo using the h5readAttributes function 
#   spInfo <- h5readAttributes(f,"spatialInfo")
#   # import spatial information from the map info in the hdf file
#   mapInfo <- h5read(f, 
#                     "map info",
#                     read.attributes = TRUE)
#   # print the info
#   mapInfo
#   
#   #r get attributes for the Reflectance dataset
#   reflInfo <- h5readAttributes(f,"Reflectance")
#   
#   ## ----read-band-wavelengths-----------------------------------------------
#   
#   #read in the wavelength information from the HDF5 file
#   wavelengths<- h5read(f,"wavelength")
#   #convert wavelength to nanometers (nm)
#   #NOTE: this is optional!
#   wavelengths <- wavelengths*1000
#   
# }
