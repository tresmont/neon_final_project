#Function for ITC extraction from folder of raster
 
  
#get the libraries 
library(raster)
library(rhdf5)
library(rgdal)
library(neonAOP)
library(itcSegment)
library(sp)
library(tools)

ITCras2poly <- function(folder){

# takes the full path name of the folder and produces list of .tifs contained within
plot_file_lst <- list.files(folder, pattern = "*.tif", full.names = TRUE)

#names list for output function
plot_file_lst_names <- list.files(folder, pattern = "*.tif")

# converts path names to raster, returns list of raster
plot_ras_lst <- lapply(plot_file_lst, raster)

#pulls first raster for stupid epsg fix
first <- plot_ras_lst[[1]]
args <- first@crs@projargs
arg_lst <- unlist(strsplit(chm@crs@projargs, "="))
zone_arg <- arg_lst[3]
zone <- substr(zone_arg, 1, 2)
utm_zone <- paste("326", zone, sep ="")

#run itcIMG
out_itc_lst <- lapply(plot_ras_lst, itcIMG, epsg = utm_zone, searchWinSize = 5, TRESHSeed = 0.45, TRESHCrown = 0.55, DIST = 20, th = 1.5, ischm = TRUE)  

#Write out shapefiles
writePoly <- function(f){
  for (i in 1:length(f)){
            file <- out_itc_lst[[i]]
            name <- plot_file_lst_names[[i]]
            name_sans_ext <- file_path_sans_ext(name)
            
            writeOGR(file, 'output',  name_sans_ext  , 'ESRI Shapefile', overwrite_layer = TRUE, verbose = TRUE)
            
  }
}

}

