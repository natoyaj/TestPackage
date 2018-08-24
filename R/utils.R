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
