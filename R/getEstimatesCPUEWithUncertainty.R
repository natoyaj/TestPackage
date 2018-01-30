#' getEstimateCPUElength
#' @description .
#' @param RFA The roundfish area of interest.
#' @param species The species of interest.
#' @param year The year of interest.
#' @param quarter The quarter of interest.
#' @param dataHL datras-data with length
#' @param percentOfAreaRepresentative the percentage of the statical recangle within sea depth intervall
#' @param bootstrapProcedure The bootstrap procedure ("simple", ...)
#' @param B The number of simulations in the selected bootstrap procedure
#' @export
#' @return Returns the estimated mCPUE per length class in the given RFA with uncertainty
#' @examples
getEstimatesCPUElength = function(RFA, species, year, quarter,dataHL, percentOfAreaRepresentative = NULL, bootstrapProcedure="simple", B = 10)
{
  dataToSimulateFrom = dataHL[!is.na(dataHL$Year) & dataHL$Year == year&
                                !is.na(dataHL$Quarter) & dataHL$Quarter == quarter&
                                !is.na(dataHL$Roundfish) & dataHL$Roundfish == RFA ,]

  #Estimate CPUEs with uncertainty
  cpueEst = calcmCPUErfa(RFA = RFA, species = species, year = year, quarter = quarter, data = dataToSimulateFrom)

  if(bootstrapProcedure =="stratified"){
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
    d <- gDistance(loc, byid=T)
    min.d <- apply(d, 1, function(x) order(x, decreasing=F)[2])
    loc$shortesDist = uniqueId[min.d]
    #-----------------------------------------------------
  }


  simCPUEs = matrix(NA,length(cpueEst),B)
  for(i in 1:B)
  {
    if(bootstrapProcedure=="simple")
    {
      data = simTrawlHaulsHLSimple(RFA,year,quarter, data = dataToSimulateFrom)
    }else if(bootstrapProcedure =="stratified"){
      data = simTrawlHaulsHLStratified(RFA,year,quarter, data = dataToSimulateFromHL,loc = loc)
    }else if(bootstrapProcedure =="almost the datras procedure"){
      data = simTrawlHaulsHLdatras(RFA,year,quarter, data = dataToSimulateFromHL)
    }else{
      return("Select a valid bootstrap procedure.")
    }
    sim = calcmCPUErfa(RFA = RFA, species = species, year = year, quarter = quarter, data = data)

    if(length(sim)!= dim(simCPUEs)[1]) sim = c(sim,rep(0, dim(simCPUEs)[1] - length(sim))) #TODO: define the length classes and include them such that we can remove this line.

    simCPUEs[,i] = sim
    print(i)
  }

  #Construct a data.frame with estimates and C.I.
  cpue = data.frame(cpueEst)
  cpue$lQ = rep(0,length(cpueEst))
  cpue$uQ = rep(0,length(cpueEst))
  for(i in 1:length(cpueEst))
  {
    quantile = quantile(simCPUEs[i,],c(0.1,0.9))
    cpue$lQ[i] = quantile[1]
    cpue$uQ[i] = quantile[2]
  }
  return(cpue)
}




#' getEstimateCPUEage
#' @description .
#' @param RFA The roundfish area of interest.
#' @param species The species of interest.
#' @param year The year of interest.
#' @param quarter The quarter of interest.
#' @param dataHL datras-data with length
#' @param dataCA datras-data with age
#' @param percentOfAreaRepresentative the percentage of the statical recangle within sea depth intervall
#' @param bootstrapProcedure The bootstrap procedure ("simple", "stratisfied", ...)
#' @param B The number of simulations in the selected bootstrap procedure
#' @export
#' @return Returns the mCPUE per age class in the given RFA with uncertainty
#' @examples
getEstimatesCPUEage = function(RFA, species, year, quarter,dataHL,dataCA, percentOfAreaRepresentative = NULL, bootstrapProcedure="simple", B = 10, removeProportionsOfCA =0,removeProportionsOfHL =0)
{

  #Extract the data of interest-------------
  dataToSimulateFromCA = dataCA[!is.na(dataCA$Year) & dataCA$Year == year&
                                 !is.na(dataCA$Quarter) & dataCA$Quarter == quarter&
                                 !is.na(dataCA$Roundfish) & dataCA$Roundfish == RFA ,]

  dataToSimulateFromHL = dataHL[!is.na(dataHL$Year) & dataHL$Year == year&
                                  !is.na(dataHL$Quarter) & dataHL$Quarter == quarter&
                                  !is.na(dataHL$Roundfish) & dataHL$Roundfish == RFA ,]

#  #Try to remove some data fram HL-file to reduce the computation time
#  uniqueIDWith = unique(dataToSimulateFromHL$haul.id[dataToSimulateFromHL$Species==species])
#  uniqueID = unique(dataToSimulateFromHL$haul.id)
#  uniqueIDWithout = setdiff(uniqueID,uniqueIDWith)
#  dataToSimulateFromHL = dataToSimulateFromHL[which(dataToSimulateFromHL$Species==species),]
#  for(id in uniqueIDWithout)
#  {
#    dataToSimulateFromHL = rbind(dataToSimulateFromHL, dataHL[which(dataHL$haul.id == id)[1],])
#  }
  #------------------------------------------

  #Estimate CPUEs----------------------------

#  ALK = calculateALKForHaul(RFA = RFA, species = species, year = year, quarter = quarter,data = dataToSimulateFromCA, idHaul = ..)

  ALK = calculateALK(RFA = RFA, species = species, year = year, quarter = quarter,data = dataToSimulateFromCA)
  cpueEst = calcmCPUErfaWithALK(RFA = RFA,species = species, year = year, quarter = quarter, data = dataToSimulateFromHL,ALK = ALK)
  #------------------------------------------

  if(bootstrapProcedure =="stratified"){
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
    d <- gDistance(loc, byid=T)
    min.d <- apply(d, 1, function(x) order(x, decreasing=F)[2])
    loc$shortesDist = uniqueId[min.d]
    #-----------------------------------------------------
  }


  #Simulate CPUES per age B times-------------------------
  simCPUEs = matrix(NA,length(cpueEst),B)
  for(i in 1:B)
  {
    if(bootstrapProcedure =="simple")
    {
      simDataCA = simTrawlHaulsCASimple(RFA,year,quarter, data = dataToSimulateFromCA)
      simDataHL = simTrawlHaulsHLSimple(RFA,year,quarter, data = dataToSimulateFromHL)
    }else if(bootstrapProcedure =="stratified"){
      simDataCA = simTrawlHaulsCAStratified(RFA,year,quarter, data = dataToSimulateFromCA)
      simDataHL = simTrawlHaulsHLStratified(RFA,year,quarter, data = dataToSimulateFromHL,loc = loc)
    }else if(bootstrapProcedure =="almost the datras procedure"){
      simDataCA = simTrawlHaulsCAStratified(RFA,year,quarter, data = dataToSimulateFromCA)
      simDataHL = simTrawlHaulsHLdatras(RFA,year,quarter, data = dataToSimulateFromHL)
    }else{
      return("Select a valid bootstrap procedure.")
    }

#    if(removeProportionsOfCA>0)
#    {
#      remove = rep(FALSE,dim(simDataCA)[1])
#      q = runif(remove)
#      simDataCA = simDataCA[which(q>removeProportionsOfCA),]
#    }
#    if(removeProportionsOfHL>0)
#    {
#      uniqueID = unique(simDataHL$haul.id)
#      remove = rep(FALSE,length(uniqueID))
#      q = runif(remove)
#      uniqueIDKeep = uniqueID[which(q>removeProportionsOfHL)]
#
#      keep = rep(FALSE,dim(simDataHL)[1])
#      for(kk in 1:length(uniqueIDKeep))
#      {
#        indeks = grepl(uniqueIDKeep[kk],simDataHL$haul.id)
#        keep[indeks] = TRUE
#      }
#      simDataHL = simDataHL[which(keep),] #this is wrong since the haul.id's are changed in simTrawlsHauls..()
#    }

    simALK = calculateALK(RFA = RFA, species = species, year = year, quarter = quarter,data = simDataCA)
    sim = calcmCPUErfaWithALK(RFA = RFA, species = species, year = year, quarter = quarter, data = simDataHL,ALK = simALK)

    simCPUEs[,i] = sim
    print(i)

  }
  #-----------------------------------------------------

  #Construct a data.frame with estimates and P.I. of CPUEs per age----
  cpue = data.frame(cpueEst)
  cpue$bootstrapMean = rep(0,length(cpueEst))
  cpue$lQ = rep(0,length(cpueEst))
  cpue$uQ = rep(0,length(cpueEst))
  for(i in 1:length(cpueEst))
  {
    quantile = quantile(simCPUEs[i,],c(0.1,0.9))
    cpue$lQ[i] = quantile[1]
    cpue$uQ[i] = quantile[2]
    cpue$bootstrapMean[i] = mean(simCPUEs[i,])

  }
  #-----------------------------------------------------
  return(cpue)
}




