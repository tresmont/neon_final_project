
# load r packages
library(raster)
library(rgdal)
library(rgeos)


setwd("H:/1_git/neon-create-aop-subset")

# import functions to find rasters in the clip extent
source("rasterFunctions.R")

#### INPUTS
# ##### SJER Clip
# # # the name of the site
# site <- "SJER"
# domain <- "D17"
# fullDomain <- "D17-California"
# year <- "2013"

##### TEAK Clip
# # the name of the site
# site <- "TEAK"
# domain <- "D17"
# fullDomain <- "D17-California"
# year <- "2013"

# ##### SOAP Clip
# # # the name of the site
# site <- "SOAP"
# domain <- "D17"
# fullDomain <- "D17-California"
# year <- "2013"

##### JERC Clip
# # the name of the site
#site <- "JERC"
#domain <- "D03"
#fullDomain <- paste0(domain,"-Florida")
#year <- "2014"

### OSBS Clip
site <- "OSBS"
domain <- "D03"
fullDomain <- paste0(domain,"-Florida")
year <- "2014"

##### HARV Clip
# the name of the site
# site <- "HARV"
# domain <- "D01"
# fullDomain <- paste0(domain,"-Massachusetts")
# year <- "2014"


#### Data Product Inputs (consistent across sites)
level <- "L2"
dataType <- "Spectrometer"
level <- paste0(site, "_",level)
productType <- paste0(site, "_",dataType)
dataProduct <- "veg_index"
# Clip Files
clipFile <- paste0(site,"_crop")


#### Distro Path Location
# 
driveLetter <- "P:"
theDir <- "Distros"
distroVersion <- "1.4a_No_Waveform"


####

# identify directory where data are located
tileDir <- file.path(driveLetter,theDir,distroVersion,domain,
                     site, year, level, productType, 
                     "Veg_Indices")

# get just the files that are geotiffs
veg.files <- list.files(tileDir, 
                        pattern = ".tif",
                        full.names = TRUE)

# get unique vi names
veg.files.base <- t(as.data.frame(strsplit(basename(veg.files),"_")))

# Grab unique product types
vi.types <- unique(veg.files.base[,ncol(veg.files.base)])



# create a list of output filenames
outputDir <- file.path("H:","1_Data-Institute-subsets", fullDomain, site, year, 
                       tolower(dataType),dataProduct)


# specify the path to the clip file
clipFilePath <- file.path("H:","1_Data-Institute-subsets/NEONdata",fullDomain, site, "vector_data")

# import shapefile to spatial polygon
clipExtent <- readOGR(clipFilePath, clipFile)

# create a list of output filenames
outNames <-  paste0("H:/1_Data-Institute-subsets/NEONdata/",fullDomain,"/",site, "/",year,"/",
                    tolower(dataType),"/", dataProduct,"/", paste0(site,"_",vi.types))

# get list of rasters to 


for(i in 1:length(vi.types)){
  rasterList <- list.files(tileDir, 
                  pattern = vi.types[i],
                  full.names = TRUE)
  get_AOP_tiles(rasterList = rasterList, outFileName = outNames[i], clipExtent=clipExtent)
}


#outNames <- paste0(clipFilePath,"/TEAK/2013/spectrometer/veg_index/", basename(veg.files))



# 
# vi.crop.data <- function(aRasterFile, outFileName, clipExtent=clipExtent){
#   print("just starting")
#   # open raster
#   aRaster <- raster(aRasterFile)
#   print("opened raster")
#   # crop raster to clip extent
#   vi.crop <- crop(aRaster, clipExtent)
#   print("cropped raster")
# 
#   # write geotiff
#   writeRaster(vi.crop,
#             filename=outFileName,
#             format="GTiff",
#             options="COMPRESS=LZW",
#             overwrite = TRUE,
#             NAflag = -9999)
# }
# 
# 
# crop all data
# mapply(vi.crop.data, 
#        aRasterFile=veg.files,
#        outFileName=outNames,
#        clipExtent=clipExtent)



# for some reason mapply isn't working so writing a for loop

# for(i in 1:length(veg.files)){
#     vi.crop.data(aRasterFile = veg.files[i],
#              clipExtent = clipExtent,
#              outFileName = outNames[i]
#               )}
