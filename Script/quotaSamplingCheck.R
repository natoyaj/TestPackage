

#This code is run every time the package is installed

library(rgdal)
library(rworldxtra)
library(mapplots)
library(DATRAS)

dataDir <<- system.file("Data", package = "TestPackage")
dat <- readExchangeDir(dataDir)
#-------------------------------------------------------

#Read shape file for roundfish areas
rfa <-
  readOGR(file.path(
    system.file("shapefiles", package = "TestPackage"),
    "Roundfish_shapefiles"
  ))
ices <-
  readOGR(file.path(
    system.file("shapefiles", package = "TestPackage"),
    "ICES_AREA_shapefiles"
  ))
#Extract the data frames from IBTS-data and merge them---
CA <- dat[["CA"]]
HL <- dat[["HL"]]
HH <- dat[["HH"]]

#temporary fix for selecting years: we should create an automatic download from DATRAS
year=2015
quarter = 1
CA<-CA[CA$Year==year & CA$Quarter ==quarter,]
HL<-HL[HL$Year==year & HL$Quarter ==quarter,]
HH<-HH[HH$Year==year & HH$Quarter ==quarter,]

#Read shape file for roundfish areas
rfa <-
  readOGR(file.path(
    system.file("shapefiles", package = "TestPackage"),
    "Roundfish_shapefiles"
  ))
#rfa <- readOGR("Roundfish_shapefiles")


#' Get stations with age measurement for the given species and length group
#' @param hh the HH table for the survey
#' @param ca the CA table for the survey
#' @param scientific name of species
#' @param lngtCM the lengthgroup as identified by column lngtCM in CA and HL
#' @return data frame formatted as hh
getStationsWithAge <- function(hh, ca, species, lngtCM) {
  ages <-
    ca[ca$LngtCm == lngtCM & ca$Species == species & !is.na(ca$Age), ]
  return(hh[hh$haul.id %in% unique(ages$haul.id), ])
}

#' Get stations with length measurement for the given species and length group
#' @param hh the HH table for the survey
#' @param hl the HL table for the survey
#' @param scientific name of species
#' @param lngtCM the lengthgroup as identified by column lngtCM in CA and HL
#' @return data frame formatted as hh
getStationsWithLength <- function(hh, hl, species, lngtCM) {
  lengths <- hl[hl$LngtCm == lngtCM & hl$Species == species, ]
  return(hh[hh$haul.id %in% unique(lengths$haul.id), ])
}

#' computes a table of the number of age samples pr haul and length group
#' @param ca DATRAS CA table
#' @param hl DATRAS HL table
#' @param species scientific name of species, to be mathed with DATRAS Species
#' @param lengthresCM length resolution for length groups. Must exceed resolution of measurement, but this is not enforced.
#' @return
tabulate_ages_pr_length_pr_haul <- function(ca=CA, hl=HL, species="Gadus morhua", lengthresCM=1){
  ca <- ca[!is.na(ca$Species) & ca$Species==species,]
  hl <- hl[!is.na(hl$Species) & hl$Species==species,]

  maxl <- max(max(ca$LngtCm, na.rm=T), max(hl$LngtCm, na.rm=T))
  breaks <- seq(0,maxl+lengthresCM, lengthresCM)

  ca$lengthgroup <- cut(ca$LngtCm, breaks=breaks)
  hl$lengthgroup <- cut(hl$LngtCm, breaks=breaks)
  mm <- merge(hl, ca, by=c("haul.id", "lengthgroup"), all.x=T)
  drop <- sum(is.na(mm$CatCatchWgt) | mm$CatCatchWgt==0)
  mm <- mm[!is.na(mm$CatCatchWgt) & mm$CatCatchWgt>0,]

  warning(paste(drop, "Registrations dropped because CatCathWgt is missing or zero.", nrow(mm), "registrations left."))
  nages<-aggregate(list(agesamples=mm$Age), by=list(haul.id=mm$haul.id, lengthgroup=mm$lengthgroup), FUN=function(x){sum(!is.na(x))})
  if (any(is.na(nages$lengthgroup))){
    warning("NAs in lengthgroup")
  }
  return(nages)
}

#' Tabulates the number of missing age samples in each length group and roundfish area.
#' @param hh HH table for the survey
#' @param ca CA table for the survey
#' @param hl HL table for the survey
#' @param scientific name of species
#' @return data frame with columns: LngtCM RFA count.age count.length missing
tabulateMissingAgeSamples <-
  function(hh = HH,
           ca = CA,
           hl = HL,
           species = "Gadus morhua") {
    lengths <- hl[hl$Species == species, ]
    lengths <-
      merge(lengths, hh[, c("haul.id", "Roundfish")], by = "haul.id", all.x =
              T)
    lengths <-
      aggregate(
        lengths$haul.id ~ lengths$LngtCm + lengths$Roundfish,
        FUN = function(x) {
          length(unique(x))
        }
      )
    names(lengths) <- c("LngtCM", "RFA", "count")

    ages <- ca[ca$Species == species & !is.na(ca$Age), ]
    ages <-
      merge(ages, hh[, c("haul.id", "Roundfish")], by = "haul.id", all.x = T)
    ages <-
      aggregate(
        ages$haul.id ~ ages$LngtCm + ages$Roundfish,
        FUN = function(x) {
          length(unique(x))
        }
      )
    names(ages) <- c("LngtCM", "RFA", "count")

    tab <-
      merge(
        ages,
        lengths,
        by = c("LngtCM", "RFA"),
        suffixes = c(".age", ".length"),
        all = T
      )
    tab[is.na(tab)] <- 0
    tab$missing <- tab$count.length - tab$count.age

    return(tab)
  }

#' Plot stations color coded for missing age samples in specific length groups
#' @param hh HH table for the surve
#' @param nages table of age samples as returned by tabulate_ages_pr_length_pr_haul
#' @param polygons SpatialPolygonsDataFrame object for drawing on the map. Map be NULL.
#' @param labelcol column i SpatioalPolygon specifying labels for polugons. Not plotted if NULL
plotStations <-
  function(nages,
           hh = HH,
           polygons = rfa,
           labelcol="AreaName", main=NULL) {

      age <- hh[hh$haul.id %in% unique(nages[nages$agesamples>0,"haul.id"]),]
      length <- hh[hh$haul.id %in% unique(nages[nages$agesamples==0,"haul.id"]),]

    data(countriesHigh)
    map <- countriesHigh

    basemap(
      xlim = c(min(hh$lon), max(hh$lon)),
      ylim = c(min(hh$lat), max(hh$lat)),
      bg = "white"
    )
    plot(map, col = "grey", add = T)
    if (!is.null(polygons)) {
      plot(polygons, add = T)
      if (!is.null(labelcol)){
        text(coordinates(polygons), as.character(polygons@data[[labelcol]]))
      }
    }
    points(age$lon, age$lat, col = "blue", pch = 3)
    points(length$lon, length$lat, col = "red", pch = 3)

    if (is.null(main)){
      title(paste(species, unique(ca$Year)))
    }
    else{
      title(main)
    }
  }


tab <- tabulateMissingAgeSamples()
tt <- tab[order(tab$missing, decreasing = T), ]
tt[1:6, ]

quantile(sort(unique(tt$LngtCM)))
sort(unique(tt$LngtCM))

par(par.pre)
#legend("topleft", legend=c("Hauls with missing age ", "Hauls with length and age"),
#       col=c("red", "blue"), pch= c(3,3), cex=0.8)


library(geosphere)
rfa$areas.sqm<-areaPolygon(rfa)
rfa$areas.sqkm<-rfa$areas.sqm/(1000*1000)
print(rfa@data)

unique(CA[CA$Species=="Gadus morhua",]$LngtCm)
sort(unique(CA[CA$Species=="Gadus morhua" &  CA$Age =="1",]$LngtCm))

# new approach to missing age plotting

#for grouping lengths
#nages <- tabulate_ages_pr_length_pr_haul(lengthresCM = 5)

nages <- tabulate_ages_pr_length_pr_haul()

plotStations(nages)
par.pre <- par(no.readonly = T)
par(mfrow = c(2, 2))
plotStations(nages[nages$lengthgroup=="(15,16]",])
plotStations(nages[nages$lengthgroup=="(24,25]",])
plotStations(nages[nages$lengthgroup=="(26,27]",])
plotStations(nages[nages$lengthgroup=="(13,14]",])
par(par.pre)

#get fraction of length registrations with missing age
sum(nages$agesamples==0)/(nrow(nages))

#get fraction of hauls with some length group that is missing age registrations:
hauls_missing <- aggregate(list(has_missing=nages$agesamples), by=list(haul.id=nages$haul.id), FUN=function(x){any(x==0)})
sum(hauls_missing$has_missing)/nrow(hauls_missing)
