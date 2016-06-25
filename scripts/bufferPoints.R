# Function to create a buffered ploygon from points

library(sp)
library(rgdal)

bufferPoly <- function(site, type, plotID, ptShp, radius){
  if(missing(ptShp)){
    stop("input point polygon is required")
  }
  if(missing(radius)){
    #set the radius for the plots
    radius <- 20 #radius in meters
  } else {
    #set the radius for the plots
    radius <- radius #radius in meters
  }
  if(missing(type)){
    type <- "centroid"
  }
  
  ptShp_base <- basename(ptShp)
  ptShp_base_split <- strsplit(ptShp_base, "\\.")
  ptShp_base_no_etx <- ptShp_base_split[1]
  noextstr <- as.character(ptShp_base_no_etx[[1]][1])

  ptShp_dir <- dirname(ptShp)
  
  pts <- readOGR(ptShp_dir, noextstr)
  
  
  #define the plot boundaries based upon the plot radius. 
  #NOTE: this assumes that plots are oriented North and are not rotated. 
  #If the plots are rotated, you'd need to do additional math to find 
  #the corners.
  yPlus <- pts$northing+radius
  xPlus <- pts$easting+radius
  yMinus <- pts$northing-radius
  xMinus <- pts$easting-radius
  
  #Extract the plot ID information. NOTE: because we set
  #stringsAsFactor to false above, we can import the plot 
  #ID's using the code below. If we didn't do that, our ID's would 
  #come in as factors by default. 
  #We'd thus have to use the code ID=as.character(centroids$Plot_ID) 
<<<<<<< HEAD
  ID <- pts$Plot_ID
  prj_info <- pts@proj4string@projargs
=======
  if(type == "centroi"){
    ID <- ptShp$Plot_ID  
  } else{
    ID <- seq(1:length(ptShp))
  }
  
  prj_info <- ptShp@proj4string@projargs
>>>>>>> 0d290cab734ade03a610513f7b70333b0c0e1bc1
    
    
  #calculate polygon coordinates for each plot centroid. 
  square <- cbind(xMinus,yPlus, xPlus,yPlus, xPlus,yMinus, xMinus,yMinus,xMinus,yPlus,xMinus,yPlus)
  
  #create spatial polygons
  polys <- SpatialPolygons(mapply(function(poly, id) {
    xy <- matrix(poly, ncol=2, byrow=TRUE)
    Polygons(list(Polygon(xy)), ID=id)
  }, split(square, row(square)), ID),proj4string=CRS(as.character(prj_info)))
  
  
  # Create SpatialPolygonDataFrame -- this step is required to output multiple polygons.
  polys.df <- SpatialPolygonsDataFrame(polys, data.frame(id=ID, row.names=ID))
  
  #write out the data
  outfile <- paste(site, type, radius, sep = "_", collapse = '')

  if(!missing(plotID)){
    outfile <- paste(outfile, plotID, sep = "_", collapse = '')
  }
  print(outfile)
  # writeOGR(polys.df, 'output', outfile, 'ESRI Shapefile',
           # overwrite_layer=TRUE)
  
  return(polys.df)
  
}



