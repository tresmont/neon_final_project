###
# this code takes a shapefile polygon extent and a directory containging
# a bunch of NEON H5 files and returns the flightlines that INTERSECT the 
# polygon boundary
###

# library(devtools)
# install from github
# install_github("lwasser/neon-aop-package/neonAOP")


# load libraries
library(sp)
library(rgeos)
library(rhdf5)
library(neonAOP)

########## Inputs #####

# shapefile path and name
# shape.path <- "NEONdata/D17-California/SJER/vector_data"
# clipFile <- "sjer_clip_extent"


########## Inputs #####

### TEAK Clip
#the name of the site
# site <- "TEAK"
# domain <- "D17"
# fullDomain <- "D17-California"
# year <- "2013"

#### OSBS Clip
# # the name of the site
# site <- "OSBS"
# domain <- "D03"
# fullDomain <- "D03-Florida"
# year <- "2014"

## SOAP Clip
# the name of the site
site <- "SOAP"
domain <- "D17"
fullDomain <- "D17-California"
year <- "2013"

### SJER clip
# site <- "SJER"
# domain <- "D17"
# fullDomain <- "D17-California"
# year <- "2013"

#### Identify where the shapefile is located on your hard drive
### NOTE: change these to the polygon that contains your crop extent

# define clip file name (this could be a plot)
clipFile <- paste0(site, "_crop")
# import shapefile
clipFilePath <- file.path("neonData", fullDomain, site, "vector_data")

##### Define your hard drive path. Mac's often use "volumes" for an external hard drive
##### Windows often use a letter "D:" etc
drivePath <- "Volumes"
driveName <- "My Passport"

########## Reflectance data variables based upon the hard drive structure
dataProduct <- "Reflectance"
dataType <- "Spectrometer"
level <- paste0(site,"_L1")
productType <- paste0(site,"_", dataType)



### Define the data directory where the h5 files are stored

dataDir <- file.path(drivePath, driveName,
                      domain,
                      site, year, level, productType, dataProduct)

# you only need this is you are using a mac
dataDir <- paste0("/", dataDir)
# get a list of all files in the dir
# if this variable returns no values, that means your dataDir is wrong OR
# the data dir doesn't have any information in it.
h5.files <- list.files(dataDir, pattern = '\\.h5$', full.names = TRUE)


#### Import shapefile / clipping spatial object ####
# note this could be replaced with a spatial polygon rather than importing a shapefile
clip.polygon <- readOGR(clipFilePath,
                        clipFile)

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
write_shapefile_bound <- function(f, shpDir, proj4Str){
  # create shapefileName
  # output
  h5.extent <- create_extent(f)
  # create polygon extent assign CRS to extent 
  h5.extent.sp <- as(h5.extent, "SpatialPolygons")
  # create data.frame, add the name of the file to the shapefile
  sp.df <- data.frame(id=basename(f))
  sp.obj <- SpatialPolygonsDataFrame(h5.extent.sp, sp.df)
  # assign CRS
  crs(sp.obj) <- CRS(proj4Str)
  # create shapefile output name
  outName <- gsub(pattern = ".h5",
                  x = basename(f),
                  replacement = "")
  writeOGR(sp.obj, 
           shpDir, #path to export to
           outName,
           driver="ESRI Shapefile",
           overwrite_layer = TRUE)
}

##################### Run Export Polygon Boundary for Each Flightline ##############

# export extent polygon for all flightlines
proj4Str <- "+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs"
shpDir <- paste0("exports/", site, "_flightLines")

sapply(h5.files, write_shapefile_bound,
       proj4Str = proj4Str,
       shpDir = shpDir)

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


## Loop through all of the H5 files and return a list of what's "in"
intersect_files <- find_intersect_h5(h5.files, 
                                     clip.polygon)


################### Next -- see if you can loop through polygons ####

# for this to work the shapefile has to have an "Id" field with numbers 1-x
soap.plots <- readOGR("exports/SOAP_subsets_poly/", "SOAP_subsets")

#clip.file <- soap.plots[soap.plots$Id == 1,]
final.files <- vector("list", length(soap.plots))

# This for loop generates a list of flightlines that intersect each polygon in a 
# shapefile. Note that the spatial object needs an Id field that is numbered 1-n
for(i in 1:length(soap.plots)){
  clip.file <- soap.plots[soap.plots$Id == i,]
  final.files[[i]] <- find_intersect_h5(h5.files, 
                                 clip.file)
  }


first.box <- soap.plots[soap.plots$Id == 1,]
first.box

summary(soap.plots$Id)


