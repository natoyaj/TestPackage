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
getEstimatesCPUElength = function(RFA, species, year, quarter,dataHL, percentOfAreaRepresentative = NULL, bootstrapProcedure="simple", B = 10, weightStatRec = NULL)
{
  dataToSimulateFrom = dataHL[!is.na(dataHL$Year) & dataHL$Year == year&
                                !is.na(dataHL$Quarter) & dataHL$Quarter == quarter&
                                !is.na(dataHL$Roundfish) & dataHL$Roundfish == RFA ,]

  #Estimate CPUEs with uncertainty
  cpueEst = calcmCPUErfa(RFA = RFA, species = species, year = year, quarter = quarter, data = dataToSimulateFrom, weightStatRec = weightStatRec)

  if(bootstrapProcedure =="stratified"){
    #Find shortest distance to a neigbour trawl location---
    uniqueId = unique(dataToSimulateFrom$haul.id)
    loc = data.frame(uniqueId)
    loc$lat = rep(-999,dim(loc)[1])
    loc$lon = rep(-999,dim(loc)[1])

    for(i in 1:length(uniqueId))
    {
      id = uniqueId[i]
      indeks = which(dataToSimulateFrom$haul.id== id)[1]
      loc$lat[i] = dataToSimulateFrom$lat[indeks]
      loc$lon[i] = dataToSimulateFrom$lon[indeks]
    }

    coordinates(loc) <- ~lon+lat
    proj4string(loc) ="+proj=longlat"
    d = spDists(loc)
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
      data = simTrawlHaulsHLStratified(RFA,year,quarter, data = dataToSimulateFrom,loc = loc)
    }else if(bootstrapProcedure =="hierarchical"){
      sim <- simTrawlHaulsHiearchical(RFA, year, quarter, dataToSimulateFrom, dataToSimulateFrom)
      data <- sim$simHL
    }
    else if(bootstrapProcedure =="almost the datras procedure"){
      data = simTrawlHaulsHLdatras(RFA,year,quarter, data = dataToSimulateFrom)
    }else{
      return("Select a valid bootstrap procedure.")
    }
    sim = calcmCPUErfa(RFA = RFA, species = species, year = year, quarter = quarter, data = data, weightStatRec = weightStatRec)

    if(length(sim)!= dim(simCPUEs)[1]) sim = c(sim,rep(0, dim(simCPUEs)[1] - length(sim))) #TODO: define the length classes and include them such that we can remove this line.

    simCPUEs[,i] = sim
    print(i)
  }

  #Construct a data.frame with estimates and C.I.
  cpue = data.frame(cpueEst)
  cpue$lQ = rep(0,length(cpueEst))
  cpue$uQ = rep(0,length(cpueEst))
  cpue$sd = rep(0,length(cpueEst))
  for(i in 1:length(cpueEst))
  {
    quantile = quantile(simCPUEs[i,],c(0.025,0.975))
    cpue$lQ[i] = quantile[1]
    cpue$uQ[i] = quantile[2]
    cpue$sd[i] = sd(simCPUEs[i,])
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
#' @param procedure Logical if we use the new procedure (defaul = false). Call ALK-procedure ?
#' @param weightStatRec The weights for the statistical rectangles for Saithe
#' @export
#' @return Returns the mCPUE per age class in the given RFA with uncertainty
#' @examples
getEstimatesCPUEage = function(RFA, species, year, quarter,dataHL,dataCA, percentOfAreaRepresentative = NULL,
                               bootstrapProcedure="simple", B = 10, removeProportionsOfCA =0,removeProportionsOfHL =0,
                               procedure = "",weightStatRec = NULL){
  #Extract the data of interest-------------
  dataToSimulateFromCA = dataCA[!is.na(dataCA$Year) & dataCA$Year == year&
                                 !is.na(dataCA$Quarter) & dataCA$Quarter == quarter&
                                 !is.na(dataCA$Roundfish) & dataCA$Roundfish == RFA ,]

  dataToSimulateFromHL = dataHL[!is.na(dataHL$Year) & dataHL$Year == year&
                                  !is.na(dataHL$Quarter) & dataHL$Quarter == quarter&
                                  !is.na(dataHL$Roundfish) & dataHL$Roundfish == RFA ,]
  dataToSimulateFromHL$haul.idReal = dataToSimulateFromHL$haul.id #Need in this in the procedyre for calculating CPUE with model when HL data is simulated

  #Estimate CPUEs----------------------------
  if(procedure == "haulBased"){
    ALKNew = calculateALKNew(RFA = RFA, species = species, year = year, quarter = quarter,data = dataToSimulateFromCA, data_hl = dataToSimulateFromHL)
    cpueEst = calcmCPUErfaWithALKNew(RFA = RFA,species = species, year = year, quarter = quarter, data = dataToSimulateFromHL,ALKNew = ALKNew, weightStatRec = weightStatRec)
  }else if(procedure == "modelBased"){
    ALKModel = calculateALKModel(RFA = RFA, species = species, year = year, quarter = quarter,hh = hh,fitModel = fitModel,keyIdMeshHaul= keyIdMeshHaul)
    cpueEst = calcmCPUErfaWithALKNew(RFA = RFA,species = species, year = year, quarter = quarter, data = dataToSimulateFromHL,ALKNew = ALKModel,procedure = procedure, weightStatRec = weightStatRec)
  }else if(procedure == ""){
    cpueEst = calcmCPUErfaWithALKNew(RFA = RFA,species = species, year = year, quarter = quarter, data = dataToSimulateFromHL,ALKNew = ALKModel,procedure = procedure, weightStatRec = weightStatRec)
  }else{
    ALK = calculateALK(RFA = RFA, species = species, year = year, quarter = quarter,data = dataToSimulateFromCA)
    cpueEst = calcmCPUErfaWithALK(RFA = RFA,species = species, year = year, quarter = quarter, data = dataToSimulateFromHL,ALK = ALK,weightStatRec = weightStatRec)
  }
  else{
    stop("Unkown procedure.")
  }
  #------------------------------------------

  if(bootstrapProcedure =="stratified"| bootstrapProcedure =="stratifiedNewALK"){
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
  }


  #Simulate CPUES per age B times-------------------------
  simCPUEs = matrix(NA,length(cpueEst),B)
  for(i in 1:B)
  {
    if(bootstrapProcedure =="simple")
    {
      simDataCA = simTrawlHaulsCASimple(RFA,year,quarter, data = dataToSimulateFromCA)
      simDataHL = simTrawlHaulsHLSimple(RFA,year,quarter, data = dataToSimulateFromHL)
    }else if(bootstrapProcedure =="hierarchical"){
      sim <- simTrawlHaulsHiearchical(RFA, year, quarter, dataToSimulateFromHL, dataToSimulateFromCA)
      simDataCA = sim$simCA
      simDataHL = sim$simHL
    }
    else if(bootstrapProcedure =="stratified"){
      simDataCA = simTrawlHaulsCAStratified(RFA,year,quarter, data = dataToSimulateFromCA, species = species)
      simDataHL = simTrawlHaulsHLStratified(RFA,year,quarter, data = dataToSimulateFromHL,loc = loc)
    }else if(bootstrapProcedure =="almost the datras procedure"){
      simDataCA = simTrawlHaulsCAStratified(RFA,year,quarter, data = dataToSimulateFromCA, species = species)
      simDataHL = simTrawlHaulsHLdatras(RFA,year,quarter, data = dataToSimulateFromHL)
    }else if(bootstrapProcedure =="stratifiedNewALK"){
      simHauls = simCaHlSimultaniousyStratified(RFA,year,quarter, dataHH = hh,loc = loc)
      simDataCA = dataToSimulateFromCA[1,]#Define the structure in the data, this line is removed later.
      simDataHL = dataToSimulateFromHL[1,]#Define the structure in the data, this line is removed later.

      for(j in 1:dim(simHauls)[1]){
        tmpCA = dataToSimulateFromCA[which(dataToSimulateFromCA$haul.id== simHauls$haul.id[j]),]
        tmpHL = dataToSimulateFromHL[which(dataToSimulateFromHL$haul.id== simHauls$haul.id[j]),]

        if(dim(tmpCA)[1]>0){
          tmpCA$StatRec = simHauls$StatRec[j]
          tmpCA$haul.id = paste(simHauls$haul.id[j],j,sep = "") #Set an unique ID to the haul
          simDataCA = rbind(simDataCA,tmpCA)
        }
        if(dim(tmpHL)[1]>0){
          tmpHL$StatRec = simHauls$StatRec[j]
          tmpHL$haul.id = paste(simHauls$haul.id[j],j,sep = "") #Set an unique ID to the haul
          simDataHL = rbind(simDataHL,tmpHL)
        }
      }
      simDataCA = simDataCA[-1,]#Removes the first line which was created for defining the structure of the data
      simDataHL = simDataHL[-1,]#Removes the first line which was created for defining the structure of the data

    }else{
      return("Select a valid bootstrap procedure.")
    }


    if(procedure == "haulBased"){
      simALK = calculateALKNew(RFA = RFA, species = species, year = year, quarter = quarter,data = simDataCA, data_hl = simDataHL)
      if(simALK[1]=="No observations in period given")print("There were simulated zero age observations and the program crash")
      sim = calcmCPUErfaWithALKNew(RFA = RFA, species = species, year = year, quarter = quarter, data = simDataHL,ALKNew = simALK,procedure = procedure, weightStatRec = weightStatRec)
    }else if(procedure == "modelBased"){
      simALK = simALKModel(RFA = RFA, species = species, year = year, quarter = quarter,hh=hh,fitModel=fitModel,keyIdMeshHaul=keyIdMeshHaul)
      sim = calcmCPUErfaWithALKNew(RFA = RFA,species = species, year = year, quarter = quarter, data = simDataHL,ALKNew = simALK,procedure = procedure, weightStatRec = weightStatRec)
    }else{
      simALK = calculateALK(RFA = RFA, species = species, year = year, quarter = quarter,data = simDataCA)
      sim = calcmCPUErfaWithALK(RFA = RFA, species = species, year = year, quarter = quarter, data = simDataHL,ALK = simALK, weightStatRec = weightStatRec)
    }
    simCPUEs[,i] = sim


    #Print the progress------------------
    print(i)
    print(sim)
    #------------------------------------

    if(is.na(sum(sim)))break
  }
  #-----------------------------------------------------

  #Construct a data.frame with estimates and P.I. of CPUEs per age----
  cpue = data.frame(cpueEst)
  cpue$bootstrapMean = rep(0,length(cpueEst))
  cpue$lQ = rep(0,length(cpueEst))
  cpue$uQ = rep(0,length(cpueEst))
  cpue$sd = rep(0,length(cpueEst))
  for(i in 1:length(cpueEst))
  {
    quantile = quantile(simCPUEs[i,],c(0.025,0.975))
    cpue$lQ[i] = quantile[1]
    cpue$uQ[i] = quantile[2]
    cpue$sd[i] = sd(simCPUEs[i,])
    cpue$bootstrapMean[i] = mean(simCPUEs[i,])

  }
  #-----------------------------------------------------
  return(cpue)
}




