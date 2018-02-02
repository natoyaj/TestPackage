library(DATRAS)
library(rgdal)
library(rworldxtra)
library(mapplots)

dataDir <<- system.file("Data", package = "TestPackage")
dat<- readExchangeDir(dataDir)
#-------------------------------------------------------

#Extract the data frames from IBTS-data and merge them---
CA <- dat[["CA"]]
HL <- dat[["HL"]]
HH <- dat[["HH"]]

#Read shape file for roundfish areas
rfa <- readOGR(file.path(system.file("shapefiles", package = "TestPackage"), "Roundfish_shapefiles"))

#' Get stations with age measurement for the given species and length group
#' @param hh the HH table for the survey
#' @param ca the CA table for the survey
#' @param scientific name of species
#' @param lngtCM the lengthgroup as identified by column lngtCM in CA and HL
#' @return data frame formatted as hh
getStationsWithAge <- function(hh, ca, species, lngtCM){
  ages <- ca[ca$LngtCm==lngtCM & ca$Species==species & !is.na(ca$Age),]
  return(hh[hh$haul.id %in% unique(ages$haul.id),])
}

#' Get stations with length measurement for the given species and length group
#' @param hh the HH table for the survey
#' @param hl the HL table for the survey
#' @param scientific name of species
#' @param lngtCM the lengthgroup as identified by column lngtCM in CA and HL
#' @return data frame formatted as hh
getStationsWithLength <- function(hh, hl, species, lngtCM){
  lengths <- hl[hl$LngtCm==lngtCM & hl$Species==species,]
  return(hh[hh$haul.id %in% unique(lengths$haul.id),])
}

#' Tabulates the number of missing age samples in each length group and roundfish area.
#' @param hh HH table for the survey
#' @param ca CA table for the survey
#' @param hl HL table for the survey
#' @param scientific name of species
#' @return data frame with columns: LngtCM RFA count.age count.length missing
tabulateMissingAgeSamples <- function(hh=HH, ca=CA, hl=HL, species = "Gadus morhua"){
  lengths <- hl[hl$Species==species,]
  lengths <- merge(lengths, hh[,c("haul.id", "Roundfish")], by="haul.id", all.x=T)
  lengths <- aggregate(lengths$haul.id~lengths$LngtCm+lengths$Roundfish, FUN=function(x){length(unique(x))})
  names(lengths) <- c("LngtCM", "RFA", "count")

  ages <- ca[ca$Species==species & !is.na(ca$Age),]
  ages <- merge(ages, hh[,c("haul.id", "Roundfish")], by="haul.id", all.x=T)
  ages <- aggregate(ages$haul.id~ages$LngtCm+ages$Roundfish, FUN=function(x){length(unique(x))})
  names(ages) <- c("LngtCM", "RFA", "count")

  tab <- merge(ages, lengths, by=c("LngtCM", "RFA"), suffixes=c(".age",".length"), all=T)
  tab[is.na(tab)]<-0
  tab$missing <- tab$count.length - tab$count.age

  return(tab)
}

#' Plot stations color coded for missing age samples in specific length groups
#' @param hh HH table for the survey
#' @param ca CA table for the survey
#' @param hl HL table for the survey
#' @param scientific name of species
#' @param lngtCM the lengthgroup as identified by column lngtCM in CA and HL
#' @param polygons SpatialPolygonsDataFrame object for drawing on the map. Map be NULL.
plotStations <- function(hh=HH, ca=CA, hl=HL, species = "Gadus morhua", lengthGroup = 38, polygons=rfa){
  age <- getStationsWithAge(hh,ca,species,lengthGroup)
  length <- getStationsWithLength(hh,hl,species,lengthGroup)
  length <- length[!length$haul.id %in% unique(age$haul.id),]

  data(countriesHigh)
  map <- countriesHigh

  basemap(xlim=c(min(HH$lon), max(HH$lon)), ylim=c(min(HH$lat), max(HH$lat)), bg="white")
  plot(map, col="grey", add=T)
  if (!is.null(polygons)){
    plot(polygons, add=T)
  }
  points(age$lon, age$lat, col="green", pch=3)
  points(length$lon, length$lat, col="red", pch=3)

}

tab<-tabulateMissingAgeSamples()
tt<-tab[order(tab$missing, decreasing=T),]
tt[1:6,]

dev.new()
par(mfrow=c(2,2))
plotStations(lengthGroup = 16)
plotStations(lengthGroup = 25)
plotStations(lengthGroup = 27)
plotStations(lengthGroup = 14)
