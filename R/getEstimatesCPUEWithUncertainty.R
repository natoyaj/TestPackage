#' getEstimateCPUElength
#' @description .
#' @param RFA The roundfish area of interest.
#' @param species The species of interest.
#' @param year The year of interest.
#' @param quarter The quarter of interest.
#' @param bootstrapProcedure The bootstrap procedure ("simple", ...)
#' @param B The number of simulations in the selected bootstrap procedure
#' @export
#' @return Returns the estimated mCPUE per length class in the given RFA with uncertainty
#' @examples
getEstimatesCPUElength = function(RFA, species, year, quarter,dat, bootstrapProcedure="simple", B = 10)
{
  dataToSimulateFrom = dat$hl_hh[!is.na(dat$hl_hh$Year) & dat$hl_hh$Year == year&
                                !is.na(dat$hl_hh$Quarter) & dat$hl_hh$Quarter == quarter&
                                !is.na(dat$hl_hh$Roundfish) & dat$hl_hh$Roundfish == RFA ,]

  #Estimate CPUEs with uncertainty
  cpueEst = calcmCPUErfa(RFA = RFA, species = species, year = year, quarter = quarter, data = dataToSimulateFrom, weightStatRec = dat$weightStatRec)

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
    }else if(bootstrapProcedure =="datras"){
      data = simTrawlHaulsHLdatras(RFA,year,quarter, data = dataToSimulateFrom)
    }else{
      return("Select a valid bootstrap procedure.")
    }
    sim = calcmCPUErfa(RFA = RFA, species = species, year = year, quarter = quarter, data = data, weightStatRec = dat$weightStatRec)

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
#' @param bootstrapProcedure The bootstrap procedure ("simple", "stratisfied", ...)
#' @param B The number of simulations in the selected bootstrap procedure
#' @param ALKprocedure Either datras, haulbased or modelbased.
#' @param doBootstrap A boolean stating if bootstrap should be done. Default is TRUE.
#' @export
#' @return Returns the mCPUE per age class in the given RFA with uncertainty
#' @examples
getEstimatesCPUEage = function(RFA, species, year, quarter,dat,
                               bootstrapProcedure="simple", B = 10,
                               ALKprocedure = "datras",doBootstrap = TRUE){
  #Extract the data of interest-------------
  dataToSimulateFromCA = dat$ca_hh[!is.na(dat$ca_hh$Year) & dat$ca_hh$Year == year&
                                 !is.na(dat$ca_hh$Quarter) & dat$ca_hh$Quarter == quarter&
                                 !is.na(dat$ca_hh$Roundfish) & dat$ca_hh$Roundfish == RFA ,]

  dataToSimulateFromHL = dat$hl_hh[!is.na(dat$hl_hh$Year) & dat$hl_hh$Year == year&
                                  !is.na(dat$hl_hh$Quarter) & dat$hl_hh$Quarter == quarter&
                                  !is.na(dat$hl_hh$Roundfish) & dat$hl_hh$Roundfish == RFA ,]
  dataToSimulateFromHL$haul.idReal = dataToSimulateFromHL$haul.id #Need in this in the procedyre for calculating CPUE with model when HL data is simulated



  #Estimate CPUEs----------------------------
  if(ALKprocedure == "haulBased"){
    ALKNew = calculateALKNew(RFA = RFA, species = species, year = year, quarter = quarter,data = dataToSimulateFromCA, data_hl = dataToSimulateFromHL)
    cpueEst = calcmCPUErfaWithALKNew(RFA = RFA,species = species, year = year, quarter = quarter, data = dataToSimulateFromHL,ALKNew = ALKNew, weightStatRec = dat$weightStatRec)
  }else if(ALKprocedure == "modelBased"){
    ALKModel = calculateALKModel(RFA = RFA, species = species, year = year, quarter = quarter,hh = dat$hh)
    cpueEst = calcmCPUErfaWithALKNew(RFA = RFA,species = species, year = year, quarter = quarter, data = dataToSimulateFromHL,ALKNew = ALKModel,procedure = ALKprocedure, weightStatRec = dat$weightStatRec)
  }else if(ALKprocedure == "datras"){
    ALK = calculateALK(RFA = RFA, species = species, year = year, quarter = quarter,data = dataToSimulateFromCA)
    cpueEst = calcmCPUErfaWithALK(RFA = RFA,species = species, year = year, quarter = quarter, data = dataToSimulateFromHL,ALK = ALK,weightStatRec = dat$weightStatRec)
  }else{
    stop("Unkown ALKprocedure")
  }
  #------------------------------------------

  #Investigate if it is observed zero data---
  tmp = dataToSimulateFromCA[!is.na(dat$ca_hh$Roundfish) & dat$ca_hh$Roundfish == RFA&
                               !is.na(dat$ca_hh$Species) & dat$ca_hh$Species == species,]
  if(sum(!is.na(tmp))==0){
    return(data.frame(cpueEst))
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
  if(doBootstrap){
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
      }else if(bootstrapProcedure =="stratified"){
        simDataCA = simTrawlHaulsCAStratified(RFA,year,quarter, data = dataToSimulateFromCA, species = species)
        simDataHL = simTrawlHaulsHLStratified(RFA,year,quarter, data = dataToSimulateFromHL,loc = loc)
      }else if(bootstrapProcedure =="datras"){
        simDataCA = simTrawlHaulsCAStratified(RFA,year,quarter, data = dataToSimulateFromCA, species = species)
        simDataHL = simTrawlHaulsHLdatras(RFA,year,quarter, data = dataToSimulateFromHL)
      }else if(bootstrapProcedure =="stratifiedNewALK"){
        simHauls = simCaHlSimultaniousyStratified(RFA,year,quarter, dataHH = dat$hh,loc = loc)
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


      if(ALKprocedure == "haulBased"){
        simALK = calculateALKNew(RFA = RFA, species = species, year = year, quarter = quarter,data = simDataCA, data_hl = simDataHL)
        if(simALK[1]=="No observations in period given")print("There were simulated zero age observations and the program crash")
        sim = calcmCPUErfaWithALKNew(RFA = RFA, species = species, year = year, quarter = quarter, data = simDataHL,ALKNew = simALK,procedure = ALKprocedure, weightStatRec = dat$weightStatRec)
      }else if(ALKprocedure == "modelBased"){
        simALK = simALKModel(RFA = RFA, species = species, year = year, quarter = quarter,hh=dat$hh)
        sim = calcmCPUErfaWithALKNew(RFA = RFA,species = species, year = year, quarter = quarter, data = simDataHL,ALKNew = simALK,procedure = ALKprocedure, weightStatRec = dat$weightStatRec)
      }else{
        simALK = calculateALK(RFA = RFA, species = species, year = year, quarter = quarter,data = simDataCA)
        sim = calcmCPUErfaWithALK(RFA = RFA, species = species, year = year, quarter = quarter, data = simDataHL,ALK = simALK, weightStatRec = dat$weightStatRec)
      }
      simCPUEs[,i] = sim


      #Print the progress------------------
      print(i)
      print(sim)
      #------------------------------------

      if(is.na(sum(sim)))break
    }
  }
    #-----------------------------------------------------

  #Construct a data.frame with estimates and P.I. of CPUEs per age----
  cpue = data.frame(cpueEst)
  cpue$bootstrapMean = rep(0,length(cpueEst))
  cpue$lQ = rep(0,length(cpueEst))
  cpue$uQ = rep(0,length(cpueEst))
  cpue$sd = rep(0,length(cpueEst))
  if(doBootstrap){
    for(i in 1:length(cpueEst))
    {
      quantile = quantile(simCPUEs[i,],c(0.025,0.975))
      cpue$lQ[i] = quantile[1]
      cpue$uQ[i] = quantile[2]
      cpue$sd[i] = sd(simCPUEs[i,])
      cpue$bootstrapMean[i] = mean(simCPUEs[i,])

    }
  }
  #-----------------------------------------------------
  return(cpue)
}

#' calcCPUEwholeNorthSea
#' @description
#' @export
#' @return Returns the mCPUE per age class in the whole North Sea
#' @examples
calcMCPUEwholeNorthSea = function(species, year, quarter,dat, bootstrapProcedure="simple",
                               B = 10, removeProportionsOfCA =0,removeProportionsOfHL =0,
                               procedure = "datras"){

  #Read shape file for roundfish areas and calcualte area---------
  rfa <-
    readOGR(file.path(
      system.file("shapefiles", package = "TestPackage"),
      "Roundfish_shapefiles"
    ))
  rfa$areas.sqm<-areaPolygon(rfa)
  rfa$areas.sqkm<-rfa$areas.sqm/(1000*1000)
  #---------------------------------------------------------------

  #Defines the matrix with cpue to be returned--------------------
  if(species=="Gadus morhua"){
    maxAge = 6
  }else if(species=="Pollachius virens"){
    maxAge = 6
  }else{
    stop("No valid species selected, only Gadus morhua and Pollachius virens is implemented")
  }
  totalArea = 0
  mCpue = matrix(0,maxAge+1,B+1)
  #---------------------------------------------------------------

  #Calcualte the mCPUE for each RFA and calcualtes scaled average w.r.t. area----------------
  for(RFA in 1:9){ #TODO, why do we dont have RFA 10 in the data?
    areaThisRFA = rfa@data$areas.sqkm[which( as.numeric(as.character(rfa@data$AreaName)) == RFA)]

    if(procedure=="datras"){
      cpueThisRFA = getEstimatesCPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dat,
                                       bootstrapProcedure = bootstrapProcedure, B = n,doBootstrap = FALSE)
    }

    mCpue[,1] = mCpue[,1] + cpueThisRFA[,1] *areaThisRFA

    totalArea = totalArea + areaThisRFA

    print(paste("Done with rfa number", RFA, ", mean cpue so far is:"))
    print(mCpue[,1]/totalArea )
  }
  mCpue = mCpue/totalArea
  #-----------------------------------------------------------------------------------------

  #TODO: Simulate data for constructin unceratinty intervalls

  return(mCpue)
}
