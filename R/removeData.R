#' investigateRemoval
#' @description Remove a proportion of the data in a given way. Return a list regarding changes of summery statistics.
#' @param removeProcedure Removal procedure, given as an integer. removeProcedure==1 means that data is removed at random.
#' @param RFA The roundfish area of interest.
#' @param propRemove proportion of data to remove.
#' @param dat Data object
#' @param bootstrapProcedure Bootstrap procedure used.
#' @param B Samples in the bootstrap
#' @param ALKprocedure ALK procedure used for calculating CPUE
#' @param whatToRemove What to remove, either CA (otholits) or hauls. (currently not implemented for hauls)
#' @param typeOfAreaToInvestigate What area to investigate, either equals "RFA" or "NorthSea".
#' @param species The species of interest.
#' @param year The year species of interest.
#' @param quarter The quarter species of interest.
#' @param nSim The number of times to simulate removal of data.
#' @param whatToInvestigate If this variable is equal "mean", then we only investigate the effect on the mean which do not require internal bootstrapping.
#' @export
#' @return Returns a list with summary about changes in estimates by removal of data.
#' @examples
investigateRemoval = function(RFA,species, year, quarter,dat ,
                              bootstrapProcedure, B, ALKprocedure,
                              removeProcedure, propRemove,
                              nSim, whatToInvestigate, whatToRemove,typeOfAreaToInvestigate){

  tmpResults = list()
  tmpResults$mCPUE = matrix(0,confALK(species,quarter)$maxAge+1, nSim)
  tmpResults$bootstrapMean = matrix(0,confALK(species,quarter)$maxAge+1, nSim)
  tmpResults$sd = matrix(0,confALK(species,quarter)$maxAge+1, nSim)
  tmpResults$Q025 = matrix(0,confALK(species,quarter)$maxAge+1, nSim)
  tmpResults$Q975 = matrix(0,confALK(species,quarter)$maxAge+1, nSim)

  for(i in 1:nSim){

    #Remove data in the given procedure
    datRemoved = removeData(year, quarter,species,dat,removeProcedure,propRemove,whatToRemove)

    if(whatToInvestigate=="mean"){ #Return only information of changes in point estimate of CPUE to reduce computation time.
      if(typeOfAreaToInvestigate =="RFA"){
        resultSim = CPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dat = datRemoved,
                           bootstrapProcedure = bootstrapProcedure, B, ALKprocedure = ALKprocedure,doBootstrap = FALSE)
      }else{
        resultSim = CPUEnorthSea(species = species, year = year, quarter = quarter,dat = datRemoved,
                           bootstrapProcedure = bootstrapProcedure, B, ALKprocedure = ALKprocedure,doBootstrap = FALSE)
      }
      tmpResults$mCPUE[,i] = resultSim$mCPUE
    }else{ #Do bootstrap with the reduced data set.
      if(typeOfAreaToInvestigate =="RFA"){
        resultSim = CPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dat = datRemoved,
                            bootstrapProcedure = bootstrapProcedure, B, ALKprocedure = ALKprocedure,doBootstrap = TRUE)
      }else{
        resultSim = CPUEnorthSea(species = species, year = year, quarter = quarter,dat = datRemoved,
                                 bootstrapProcedure = bootstrapProcedure, B, ALKprocedure = ALKprocedure,doBootstrap = TRUE)
      }

      tmpResults$mCPUE[,i] = resultSim$mCPUE
      tmpResults$bootstrapMean[,i] = resultSim$bootstrapMean
      tmpResults$sd[,i] = resultSim$sd
      tmpResults$Q025[,i] = resultSim$Q025
      tmpResults$Q975[,i] = resultSim$Q975

    }

    if(i==1)print("Information about progress in outer bootstrap: ")
    print(paste("Bootstrap sample ",i))
    print(tmpResults$mCPUE[,i])

  }

  toReturn= list()
  if(whatToInvestigate=="mean"){
    toReturn$mCPUE = data.frame(resultSim[,1],resultSim[,1],resultSim[,1],resultSim[,1] )
    names(toReturn$mCPUE) = c("mean","Q025","Q975", "sd")
    for(i in 1:dim(toReturn$mCPUE)[1]){
      toReturn$mCPUE$mean[i] = mean(tmpResults$mCPUE[i,])
      quantile = quantile(tmpResults$mCPUE[i,],c(0.025,0.975))
      toReturn$mCPUE$Q025[i] = quantile[1]
      toReturn$mCPUE$Q975[i] = quantile[2]
      toReturn$mCPUE$sd[i] = sd(tmpResults$mCPUE[i,])
    }
  }else{
    toReturn$mCPUE = data.frame(resultSim[,1],resultSim[,1],resultSim[,1],resultSim[,1] )
    names(toReturn$mCPUE) = c("mean","Q025","Q975", "sd")
    for(i in 1:dim(toReturn$mCPUE)[1]){
      toReturn$mCPUE$mean[i] = mean(tmpResults$mCPUE[i,])
      quantile = quantile(tmpResults$mCPUE[i,],c(0.025,0.975))
      toReturn$mCPUE$Q025[i] = quantile[1]
      toReturn$mCPUE$Q975[i] = quantile[2]
      toReturn$mCPUE$sd[i] = sd(tmpResults$mCPUE[i,])
    }

    toReturn$bootstrapMean = data.frame(resultSim[,1],resultSim[,1],resultSim[,1],resultSim[,1] )
    names(toReturn$bootstrapMean) = c("mean","Q025","Q975", "sd")
    for(i in 1:dim(toReturn$bootstrapMean)[1]){
      toReturn$bootstrapMean$mean[i] = mean(tmpResults$bootstrapMean[i,])
      quantile = quantile(tmpResults$bootstrapMean[i,],c(0.025,0.975))
      toReturn$bootstrapMean$Q025[i] = quantile[1]
      toReturn$bootstrapMean$Q975[i] = quantile[2]
      toReturn$bootstrapMean$sd[i] = sd(tmpResults$bootstrapMean[i,])
    }
    toReturn$sd = data.frame(resultSim[,1],resultSim[,1],resultSim[,1],resultSim[,1] )
    names(toReturn$sd) = c("mean","Q025","Q975", "sd")
    for(i in 1:dim(toReturn$sd)[1]){
      toReturn$sd$mean[i] = mean(tmpResults$sd[i,])
      quantile = quantile(tmpResults$sd[i,],c(0.025,0.975))
      toReturn$sd$Q025[i] = quantile[1]
      toReturn$sd$Q975[i] = quantile[2]
      toReturn$sd$sd[i] = sd(tmpResults$sd[i,])
    }
    toReturn$Q025 = data.frame(resultSim[,1],resultSim[,1],resultSim[,1],resultSim[,1] )
    names(toReturn$Q025) = c("mean","Q025","Q975", "sd")
    for(i in 1:dim(toReturn$Q025)[1]){
      toReturn$Q025$mean[i] = mean(tmpResults$Q025[i,])
      quantile = quantile(tmpResults$Q025[i,],c(0.025,0.975))
      toReturn$Q025$Q025[i] = quantile[1]
      toReturn$Q025$Q975[i] = quantile[2]
      toReturn$Q025$sd[i] = sd(tmpResults$Q025[i,])
    }
    toReturn$Q975 = data.frame(resultSim[,1],resultSim[,1],resultSim[,1],resultSim[,1] )
    names(toReturn$Q975) = c("mean","Q025","Q975", "sd")
    for(i in 1:dim(toReturn$Q975)[1]){
      toReturn$Q975$mean[i] = mean(tmpResults$Q975[i,])
      quantile = quantile(tmpResults$Q975[i,],c(0.025,0.975))
      toReturn$Q975$Q025[i] = quantile[1]
      toReturn$Q975$Q975[i] = quantile[2]
      toReturn$Q975$sd[i] = sd(tmpResults$Q975[i,])
    }
  }

  return(toReturn)


}

#' removeData
#' @description .
#' @param species The species of interest.
#' @param year The year of interest.
#' @param quarter The quarter of interest.
#' @param removeProcedure Removal procedure, given as an integer. removeProcedure==1 means that data is removed at random.
#' @param propRemove proportion of data to remove.
#' @param whatToRemove
#' @export
#' @return Returns a modified data set of the data used for calculating the CPUE. The data is modified by removing
#' observations in a certain procedure.
#' @examples
removeData = function(year, quarter,species, dat,removeProcedure=1,propRemove,whatToRemove){

  datToReturn = dat

  datToReturn$hh = datToReturn$hh[!is.na(datToReturn$hh$Year) &datToReturn$hh$Year ==year &
                                    !is.na(datToReturn$hh$Quarter) & datToReturn$hh$Quarter == quarter,]
  datToReturn$ca_hh = datToReturn$ca_hh[!is.na(datToReturn$ca_hh$Year) &datToReturn$ca_hh$Year ==year &
                                    !is.na(datToReturn$ca_hh$Quarter) & datToReturn$ca_hh$Quarter == quarter&
                                      !is.na(datToReturn$ca_hh$Species) &datToReturn$ca_hh$Species ==species,]
  datToReturn$hl_hh = datToReturn$hl_hh[!is.na(datToReturn$hl_hh$Year) &datToReturn$hl_hh$Year ==year &
                                    !is.na(datToReturn$hl_hh$Quarter) & datToReturn$hl_hh$Quarter == quarter,]


  if("HH" %in% whatToRemove)datToReturn$hh = removeDataDetailedHH(datToReturn$hh,removeProcedure,propRemove)
  if("CA" %in% whatToRemove)datToReturn$ca_hh = removeDataDetailedCA(datToReturn$ca_hh,removeProcedure,propRemove,species,quarter)
  if("HL" %in% whatToRemove)datToReturn$hl_hh = removeDataDetailedHL(datToReturn$hl_hh,removeProcedure,propRemove)

  return(datToReturn)
}


#' removeDataDetailedCA
#' @description .
#' @param datDetailed The quarter of interest.
#' @param removeProcedure Removal procedure, given as an integer. removeProcedure==1 means that data is removed at random.
#' @param propRemove proportion of data to remove.
#' @export
#' @return Returns a modified data set of the data used for calculating the CPUE. The data is modified by removing
#' observations in a certain procedure.
#' @examples
removeDataDetailedCA = function(datDetailed,removeProcedure, propRemove,species,quarter){

  if(removeProcedure=="random"){
    length = dim(datDetailed)[1]
    keep = sample(1:length,ceiling(length*(1-propRemove)))
    datDetailed = datDetailed[keep,]

    return(datDetailed)
  }else if(removeProcedure=="stratified"){
    dLength = confALK(species = species,quarter = quarter)$lengthClassIntervallLengths
#    maxLength = confALK(species = species,quarter = quarter)$maxLength
    if(dLength==1){
      lengthArray = sort(unique(floor(datDetailed$LngtCm)))
      for(length in lengthArray){
        for(RFA in 1:10){
          tmp = which(floor(datDetailed$LngtCm)==length & datDetailed$Roundfish==RFA)
          print(tmp)
        }
      }
    }
  }
}


#' removeDataDetailedHL
#' @description .
#' @param datDetailed The quarter of interest.
#' @param removeProcedure Removal procedure, given as an integer. removeProcedure==1 means that data is removed at random.
#' @param RFA The roundfish area of interest.
#' @param propRemove proportion of data to remove.
#' @export
#' @return Returns a modified data set of the data used for calculating the CPUE. The data is modified by removing
#' observations in a certain procedure.
#' @examples
removeDataDetailedHL = function(datDetailed,removeProcedure, propRemove,RFA){

  return(datDetailed)
}


#' removeDataDetailedHH
#' @description .
#' @param datDetailed The quarter of interest.
#' @param removeProcedure Removal procedure, given as an integer. removeProcedure==1 means that data is removed at random.
#' @param RFA The roundfish area of interest.
#' @param propRemove proportion of data to remove.
#' @export
#' @return Returns a modified data set of the data used for calculating the CPUE. The data is modified by removing
#' observations in a certain procedure.
#' @examples
removeDataDetailedHH = function(datDetailed,removeProcedure, propRemove,RFA){

  return(datDetailed)
}


