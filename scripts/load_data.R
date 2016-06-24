library(dplyr)


# function to get the site inventory
# created separately as a convenience function
loadSiteData <- function(site, type ){ #= c("inventory", "structure")) {
  if (missing(site)){
    stop("Site name is required")
  }
  
  # get the inventory data for the site
  sitePath <- file.path(site, "inventory")
  # type <- match.arg(tolower(type))
  if (type == "structure") {
    siteCSV <- list.files(sitePath, pattern = "*vegStr.csv")
  } else {
    siteCSV <- list.files(sitePath, pattern = "*1m2Data.csv")
  }
  # print(siteCSV)
  siteData <- tbl_df(read.csv(file.path(sitePath,siteCSV),
                              stringsAsFactors = FALSE))
  return(siteData)
}

# function to filter site data using a list of species
filterSpecies <- function(siteData, species) {
  if (missing(siteData)) {
    stop("Site data is required")
  }
  if (missing(species)) {
    stop("A list of species is required")
  }
  
  # Build a filter list of species
  speciesFilter <- paste0(species, collapse = "|")
  # apply the filter
  siteSpecies <- filter(siteData, grepl(speciesFilter, scientificName))
  
  if (nrow(siteSpecies) > 0) {
    return(siteSpecies)
  } else {
    stop("Species not present")
  }
  
}
# function to list the distinct species
listSpecies <- function(siteData) {
  if (missing(siteData)) {
    stop("Site data is required")
  }
  spList <- siteData %>% distinct(scientificName) %>% arrange(scientificName)
  return(spList$scientificName)
}

loadInvasive <- function() {
  # load a list of invasive species
  invasiveSpecies <- tbl_df(read.csv(file.path("data", "invasive_species_list.csv"),
                                     header = TRUE, stringsAsFactors = FALSE))
  return(invasiveSpecies$Species)
}

# function to compare species list to invasives
findInvasives <- function(speciesList, invasiveList) {
  if (missing(speciesList)) {
    stop("Species list is required")
  }
  if (missing(invasiveList)) {
    invasiveList <- loadInvasive()
  }
  # try to handle parsing the species data if it's a data frame
  if (is.data.frame(speciesList)) {
    speciesList <- listSpecies(speciesList)
  }
  if (is.data.frame(invasiveList)) {
    invasiveList <- invasiveList$Species
  }
  # Break the multiname scientific name into genus and species
  genus <- sapply(strsplit(speciesList, " "), "[", 1)
  species <- sapply(strsplit(speciesList, " "), "[", 2)
  
  # recombine into a single character
  spList <- data.frame(species=paste(genus,species, sep = " "))
  
  # intersect the to lists to find the matches
  m <- intersect(invasiveList, spList$species)
  return(m)
}

# # function to compare species list to invasives and return with coordinates
# findInvasivesCoords <- function(siteData, invasiveList) {
#   if (missing(siteData)) {
#     stop("Species list is required")
#   }
#   if (missing(invasiveList)) {
#     stop("Invasive list is required")
#   }
#   
#   # Build a filter list of species
#   speciesFilter <- paste0(invasiveList, collapse = "|")
#   
#   # apply the filter
#   siteSpecies <- filter(siteData, grepl(speciesFilter, scientificName))
#   
#   # return a data frame with the name and coords
#   outData <- data.frame(species = siteSpecies$scientificName, 
#                         lat = siteSpecies$decimalLatitude, 
#                         lon = siteSpecies$decimalLongitude,
#                         site = siteSpecies$siteID)
#   return(outData)
# }

