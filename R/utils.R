#' findLoc
#' @description
#' @export
#' @return Returns closest nabour to each trawl station
#' @examples
findLoc = function(dat, quarter,year,RFA){
  #Extract the data of interest-------------

  dataToSimulateFromHL = dat$hl_hh[!is.na(dat$hl_hh$Year) & dat$hl_hh$Year == year&
                                     !is.na(dat$hl_hh$Quarter) & dat$hl_hh$Quarter == quarter&
                                     !is.na(dat$hl_hh$Roundfish) & dat$hl_hh$Roundfish == RFA ,]

    #Find shortest distance to a neigbour trawl location---
    uniqueId = unique(dataToSimulateFromHL$haul.id)

    loc = data.frame(uniqueId)
    loc$lat = rep(-999,dim(loc)[1])
    loc$lon = rep(-999,dim(loc)[1])

    for(i in 1:length(uniqueId))
    {
      id = uniqueId[i]
      indeks = which(dataToSimulateFromHL$haul.id== id)[1]
      loc$lat[i] = dataToSimulateFromHL$lat[indeks]
      loc$lon[i] = dataToSimulateFromHL$lon[indeks]
    }

    coordinates(loc) <- ~lon+lat
    proj4string(loc) ="+proj=longlat"
    d = spDists(loc)
    min.d <- apply(d, 1, function(x) order(x, decreasing=F)[2])
    loc$shortesDist = uniqueId[min.d]
    #-----------------------------------------------------

    return(loc)
}


#' obsInHL
#' @description
#' @param species The species of interest.
#' @param year The year of interest.
#' @param hl_hh The data of interest.
#' @param id The id of interest.
#' @param length The id of interest.
#' @export
#' @return Returns the number of observed fish of a given length
#' @examples
obsInHL = function(species, hl_hh, id,length)
{
  hl_hhOfInterest = hl_hh[!is.na(hl_hh$haul.id) & hl_hh$haul.id == id&
                            !is.na(hl_hh$Species) & hl_hh$Species == species&
                            !is.na(hl_hh$LngtCm) & floor(hl_hh$LngtCm) == floor(length),]

  #Calculates and returns mCPUE-----
  subfactor = hl_hhOfInterest$SubFactor

  nLength = 0
  if(dim(hl_hhOfInterest)[1]==0)return(0)

  for(i in 1:dim(hl_hhOfInterest)[1])
  {
    if(hl_hhOfInterest$DataType[i]=="R")
    {
      nLength =  nLength + (hl_hhOfInterest$HLNoAtLngt[i])*subfactor[i]
    }else if(hl_hhOfInterest$DataType[i]=="C")
    {
      nLength  =  nLength + hl_hhOfInterest$HLNoAtLngt[i]*subfactor[i]/60*hl_hhOfInterest$HaulDur[i]
    }
  }
  return(nLength)
}



#' extractStatRecAll
#' @description
#' @export
#' @return return a list with all the statistical rectangles in all RFAs
#' @examples
extractStatRecAll = function(){
  dataDir <- system.file("extdata", package = "IBTSindices")
  all = read.csv2(paste0(dataDir,"/CPUEallRec.csv"),sep = ",")
  all = all[,c("Area","SubArea")]

  toReturn = list()
  for(rfa in unique(all$Area)){
    toReturn[[rfa]] = sort(as.character(unique(all$SubArea[all$Area==rfa])))
  }
}

#' extractStatRecAll
#' @description
#' @export
#' @return return a vector with all the statistical rectangles in the index area
#' @examples
extractStatRecIndexArea = function(species){
  dataDir <- system.file("extdata", package = "IBTSindices")

  if(species=="Gadus morhua"){
    indexArea = read.csv2(paste0(dataDir,"/NS_Cod_TS_IndexArea.csv"),sep = ";")
  }
  indexArea = indexArea[,"Code"]
  indexArea = as.character(indexArea)

  return(indexArea)
}
