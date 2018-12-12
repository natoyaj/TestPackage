#' investigateRemovalAfterMeeting
#' @description Remove a proportion of the data in a given way. Return a list regarding changes of summery statistics.
#' @param dat Data object
#' @param bootstrapProcedure Bootstrap procedure used.
#' @param B Samples in the bootstrap
#' @param ALKprocedure ALK procedure used for calculating CPUE
#' @param species The species of interest.
#' @param year The year species of interest.
#' @param quarter The quarter species of interest.
#' @param lengthDivision  Take one otholit from each intervall (in cm).
#' @param samplesWithinEachIntervall  Number of samples taken within each length intervall.
#' @export
#' @return Returns a list with summary about changes in estimates by removal of data.
#' @examples
investigateRemoval = function(species, year, quarter,dat,
                              bootstrapProcedure= "stratifiedHLandCA", B, ALKprocedure = "haulBased",
                              lengthDivision, samplesWithinEachIntervall = 1){

  toReturn= list()

  #Calculate the mCPUE and possibly more with all data---------------------
   toReturn$WithFullData = CPUEnorthSea(species = species, year = year, quarter = quarter,dat = dat,
                                         bootstrapProcedure = bootstrapProcedure, B = n, ALKprocedure = ALKprocedure,doBootstrap = FALSE,lengthDivision = lengthDivision)
  #--------------------------------------------------------------------------


  #Define a data frame were results shall be stored -----------------------
  tmpResults = list()
  tmpResults$mCPUE = matrix(0,confALK(species,quarter)$maxAge+1, B)
  tmpResults$bootstrapMean = matrix(0,confALK(species,quarter)$maxAge+1, B)
  tmpResults$median = matrix(0,confALK(species,quarter)$maxAge+1, B)
  tmpResults$sd = matrix(0,confALK(species,quarter)$maxAge+1, B)
  tmpResults$Q025 = matrix(0,confALK(species,quarter)$maxAge+1, B)
  tmpResults$Q975 = matrix(0,confALK(species,quarter)$maxAge+1, B)
  tmpResults$BiasCQ025 = matrix(0,confALK(species,quarter)$maxAge+1, B)
  tmpResults$BiasCQ075 = matrix(0,confALK(species,quarter)$maxAge+1, B)
  #--------------------------------------------------------------------------

  nOtolithsRemoved = rep(0,B)
  nOtolithsTotal = rep(0,B)
  nWithDatras = rep(0,B)
  nWithoutDatras = rep(0,B)
  nFoundWithin = rep(0,B)
  nNotFoundWithin = rep(0,B)
  #Sample hauls, remove data, and calulates mCPUE --------------------------------------
  for(i in 1:B){
    print("Information about progress in otholit removal simulation: ")
    print(paste("Simulation ",i))
    resultSim = CPUEnorthSea(species = species, year = year, quarter = quarter,dat = dat,
                             bootstrapProcedure = bootstrapProcedure, B=1, ALKprocedure = ALKprocedure,onlySimulate = TRUE, lengthDivision = lengthDivision,samplesWithinEachIntervall = samplesWithinEachIntervall)
    tmpResults$mCPUE[,i] = resultSim$mCPUE
    nOtolithsRemoved[i] = attributes(resultSim)$nOtolithsRemoved
    nOtolithsTotal[i]= attributes(resultSim)$nOtolithsTotal

    nWithDatras[i] = attributes(resultSim)$nWithDatras
    nWithoutDatras[i]= attributes(resultSim)$nWithoutDatras
    nFoundWithin[i] = attributes(resultSim)$nFoundWithin
    nNotFoundWithin[i]= attributes(resultSim)$nNotFoundWithin
  }
  #--------------------------------------------------------------------------

  #Extract what we are interested in (mCPUE, sd and quantiles)----------------
  toReturn$mCPUE = data.frame(resultSim[,1],resultSim[,1],resultSim[,1],resultSim[,1],resultSim[,1],resultSim[,1],resultSim[,1])
  names(toReturn$mCPUE) = c("mean","median", "Q025","Q975","BiasCQ025", "BiasCQ075", "sd")
  for(i in 1:dim(toReturn$mCPUE)[1]){
    toReturn$mCPUE$mean[i] = mean(tmpResults$mCPUE[i,])
    toReturn$mCPUE$median[i] = median(tmpResults$mCPUE[i,])
    quantile = quantile(tmpResults$mCPUE[i,],c(0.025,0.975))
    toReturn$mCPUE$Q025[i] = quantile[1]
    toReturn$mCPUE$Q975[i] = quantile[2]
    toReturn$mCPUE$sd[i] = sd(tmpResults$mCPUE[i,])
    toReturn$mCPUE$cv[i] = toReturn$mCPUE$sd[i]/toReturn$mCPUE$mean[i]

    # #bias-corrected-------------
    b= qnorm((sum(tmpResults$mCPUE[i,] > toReturn$WithFullData[i,1])+ sum(tmpResults$mCPUE[i,]==toReturn$WithFullData[i,1])/2)/length(tmpResults$mCPUE[i,]))
    alph      = 0.05                                  # 95% limits
    z         = qnorm(c(alph/2,1-alph/2))             # Std. norm. limits
    p         = pnorm(z-2*b)                          # bias-correct & convert to proportions
    qq        = quantile(tmpResults$mCPUE[i,],p=p)    # Bias-corrected percentile lims.
    toReturn$mCPUE$BiasCQ025[i] = qq[1]
    toReturn$mCPUE$BiasCQ075[i] = qq[2]
  }
  #--------------------------------------------------------------------------

  attributes(toReturn)$nOtolithsRemoved = nOtolithsRemoved
  attributes(toReturn)$nOtolithsTotal = nOtolithsTotal
  attributes(toReturn)$nWithDatras = nWithDatras
  attributes(toReturn)$nWithoutDatras = nWithoutDatras
  attributes(toReturn)$nFoundWithin = nFoundWithin
  attributes(toReturn)$nNotFoundWithin = nNotFoundWithin
  return(toReturn)
}






#' simulateAgeHaulbased
#' @description .
#' @param species The species of interest.
#' @param year The year of interest.
#' @param quarter The quarter of interest.
#' @param whatToRemove
#' @export
#' @return Returns a modified data set of the data used for calculating the CPUE. The data is modified by removing
#' observations in a certain procedure.
#' @examples
simulateAgeHaulBased = function(year, quarter,species, dat,whatToRemove,lengthDivision,samplesWithinEachIntervall = 1){

  #Extract data which shall be thinned------------------------
  reducedData = dat
  reducedData$hh = reducedData$hh[!is.na(reducedData$hh$Year) &reducedData$hh$Year ==year &
                                    !is.na(reducedData$hh$Quarter) & reducedData$hh$Quarter == quarter,]
  reducedData$ca_hh = reducedData$ca_hh[!is.na(reducedData$ca_hh$Year) &reducedData$ca_hh$Year ==year &
                                    !is.na(reducedData$ca_hh$Quarter) & reducedData$ca_hh$Quarter == quarter&
                                      !is.na(reducedData$ca_hh$Species) &reducedData$ca_hh$Species ==species,]
  reducedData$hl_hh = reducedData$hl_hh[!is.na(reducedData$hl_hh$Year) &reducedData$hl_hh$Year ==year &
                                    !is.na(reducedData$hl_hh$Quarter) & reducedData$hl_hh$Quarter == quarter,]
  #----------------------------------------------------------
  print("Sampling otoliths...")
  numberOfOtholits = dim(reducedData$ca_hh)[1]

  #Remove data-----------------------------------------------
  if("CA" %in% whatToRemove)reducedData$ca_hh = removeDataCA(datDetailed = reducedData$ca_hh,species,
                                                             quarter, lengthDivision = lengthDivision,samplesWithinEachIntervall = samplesWithinEachIntervall,
                                                             hl_hh = reducedData$hl_hh)
  #----------------------------------------------------------

  #provide user with information regarding number of otholits removed-------
  nOtolithsRemoved = numberOfOtholits- dim(reducedData$ca_hh)[1]
  print(paste("Removed ",nOtolithsRemoved, " out of ", numberOfOtholits," otoliths.",sep = ""))
  #----------------------------------------------------------

  toReturn = list()
  toReturn$reducedData = reducedData
  toReturn$nOtolithsRemoved = nOtolithsRemoved
  return(toReturn)
}


#' removeDataCA
#' @description .
#' @param datDetailed The quarter of interest.
#' @export
#' @return Returns a modified data set of the data used for calculating the CPUE. The data is modified by removing
#' observations in a certain procedure.
#' @examples
removeDataCA = function(datDetailed,species,quarter,lengthDivision,samplesWithinEachIntervall,hl_hh){

  toReturn = datDetailed
  toReturn = datDetailed[1,]
  for(id in unique(datDetailed$haul.id)){
    obsTmp = datDetailed[which(datDetailed$haul.id==id),]
    obsReduced = removeObsFromHaul(obsTmp,lengthDivision, samplesWithinEachIntervall,species = species,hl_hh = hl_hh)
    if(dim(obsTmp)[1]>dim(obsReduced)[1]){
      counter = counter +1
    }
    toReturn = rbind(toReturn,obsReduced)
  }
  toReturn = toReturn[-1,]

  return(toReturn)
}

#' removeObsFromHaul
#' @description .
#' @param datDetailed The quarter of interest.
#' @param RFA The roundfish area of interest.
#' @export
#' @return Returns a modified data set of the data used for calculating the CPUE. The data is modified by removing
#' observations in a certain procedure.
#' @examples
removeObsFromHaul = function(obsTmp,lengthDivision,samplesWithinEachIntervall,species,hl_hh){
  toReturn = obsTmp[1,]
  for(i in 2:(length(lengthDivision)+1)){
    if(i <= length(lengthDivision)){
      obsInside = which(obsTmp$LngtCm>=lengthDivision[i-1]  & obsTmp$LngtCm<lengthDivision[i])
    }else if(i>length(lengthDivision)){
      obsInside = which(obsTmp$LngtCm>=lengthDivision[i-1])
    }
    if(length(obsInside)>1){
      prob = rep(0,length(obsInside))
      for(j in 1:length(obsInside)){
        prob[j] = obsInHL(species = species,hl_hh = hl_hh,id = unique(obsTmp$haul.id),length = obsTmp$LngtCm[obsInside[j]])
      }
      prob = prob/sum(prob) #Sample with probability proportional to the number observed at length
      nSample = min(samplesWithinEachIntervall,length(obsInside))
      obsSelected = sample(obsInside,nSample,replace = TRUE,prob = prob)
    }else{
      obsSelected = obsInside
    }
    toReturn = rbind(toReturn,obsTmp[obsSelected,])
  }
  toReturn = toReturn[-1,]

  return(toReturn)
}

