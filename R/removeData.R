#' investigateRemoval
#' @description Remove a proportion of the data in a given way. Return a list regarding changes of summery statistics.
#' @param removeProcedure Removal procedure, given as an integer. removeProcedure==1 means that data is removed at random.
#' @param RFA The roundfish area of interest.
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
#' @param doNotRemoveAbove  Do not remove fish that is larger than this (in cm).
#' @param lengthDivision  Take one otholit from each intervall (in cm).
#' @export
#' @return Returns a list with summary about changes in estimates by removal of data.
#' @examples
investigateRemoval = function(RFA,species, year, quarter,dat ,
                              bootstrapProcedure, B, ALKprocedure,
                              removeProcedure,
                              nSim, whatToInvestigate, whatToRemove,typeOfAreaToInvestigate,
                              doNotRemoveAbove = 9999,propRemove = NULL,
                              lengthDivision=NULL){

  toReturn= list()

  #Calculate the mCPUE and possibly more with all data---------------------
  if(whatToInvestigate=="mean"){
    if(typeOfAreaToInvestigate =="RFA"){
      toReturn$WithFullData = CPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dat = dat,
                          bootstrapProcedure = bootstrapProcedure, B = n, ALKprocedure = ALKprocedure,doBootstrap = FALSE)
    }else{
      toReturn$WithFullData = CPUEnorthSea(species = species, year = year, quarter = quarter,dat = dat,
                   bootstrapProcedure = bootstrapProcedure, B = n, ALKprocedure = ALKprocedure,doBootstrap = FALSE)
    }
  }else{
    if(typeOfAreaToInvestigate =="RFA"){
      toReturn$WithFullData = CPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dat = dat,
                                      bootstrapProcedure = bootstrapProcedure, B = n, ALKprocedure = ALKprocedure)
    }else{
      toReturn$WithFullData = CPUEnorthSea(species = species, year = year, quarter = quarter,dat = dat,
                                           bootstrapProcedure = bootstrapProcedure, B = n, ALKprocedure = ALKprocedure)
    }
  }
  #--------------------------------------------------------------------------


  #Define a data frame were results shall be stored -----------------------
  tmpResults = list()
  tmpResults$mCPUE = matrix(0,confALK(species,quarter)$maxAge+1, nSim)
  tmpResults$bootstrapMean = matrix(0,confALK(species,quarter)$maxAge+1, nSim)
  tmpResults$sd = matrix(0,confALK(species,quarter)$maxAge+1, nSim)
  tmpResults$Q025 = matrix(0,confALK(species,quarter)$maxAge+1, nSim)
  tmpResults$Q975 = matrix(0,confALK(species,quarter)$maxAge+1, nSim)
  tmpResults$BiasCQ025 = matrix(0,confALK(species,quarter)$maxAge+1, nSim)
  tmpResults$BiasCQ075 = matrix(0,confALK(species,quarter)$maxAge+1, nSim)
  #--------------------------------------------------------------------------

  #Remove data and calulates mCPUE etc.--------------------------------------
  for(i in 1:nSim){
    print("Information about progress in otholit removal simulation: ")
    print(paste("Simulation ",i))

    #Remove data in the given procedure
    datRemoved = removeData(year, quarter,species,dat,removeProcedure,whatToRemove,doNotRemoveAbove,lengthDivision,propRemove = propRemove)

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
      tmpResults$BiasCQ025[,i] = resultSim$BiasCQ025
      tmpResults$BiasCQ075[,i] = resultSim$BiasCQ075
    }
    print(tmpResults$mCPUE[,i])
  }
  #--------------------------------------------------------------------------

  #Extract what we are interested in (mCPUE, sd and quantiles)----------------
  if(whatToInvestigate=="mean"){
    toReturn$mCPUE = data.frame(resultSim[,1],resultSim[,1],resultSim[,1],resultSim[,1] ,resultSim[,1],resultSim[,1],resultSim[,1])
    names(toReturn$mCPUE) = c("mean","median", "Q025","Q975","BiasCQ025", "BiasCQ075", "sd")
    for(i in 1:dim(toReturn$mCPUE)[1]){
      toReturn$mCPUE$mean[i] = mean(tmpResults$mCPUE[i,])
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
  }else{
    toReturn$mCPUE = data.frame(resultSim[,1],resultSim[,1],resultSim[,1],resultSim[,1] )
    names(toReturn$mCPUE) = c("mean","Q025","Q975", "sd")
    for(i in 1:dim(toReturn$mCPUE)[1]){
      toReturn$mCPUE$mean[i] = mean(tmpResults$mCPUE[i,])
      quantile = quantile(tmpResults$mCPUE[i,],c(0.025,0.975))
      toReturn$mCPUE$Q025[i] = quantile[1]
      toReturn$mCPUE$Q975[i] = quantile[2]
      toReturn$mCPUE$sd[i] = sd(tmpResults$mCPUE[i,])
      toReturn$mCPUE$cv[i] = toReturn$mCPUE$sd[i]/toReturn$mCPUE$mean[i]
    }

    toReturn$bootstrapMean = data.frame(resultSim[,1],resultSim[,1],resultSim[,1],resultSim[,1] )
    names(toReturn$bootstrapMean) = c("mean","Q025","Q975", "sd")
    for(i in 1:dim(toReturn$bootstrapMean)[1]){
      toReturn$bootstrapMean$mean[i] = mean(tmpResults$bootstrapMean[i,])
      quantile = quantile(tmpResults$bootstrapMean[i,],c(0.025,0.975))
      toReturn$bootstrapMean$Q025[i] = quantile[1]
      toReturn$bootstrapMean$Q975[i] = quantile[2]
      toReturn$bootstrapMean$sd[i] = sd(tmpResults$bootstrapMean[i,])
      toReturn$bootstrapMean$cv[i] = toReturn$bootstrapMean$sd[i]/toReturn$bootstrapMean$mean[i]
    }
    toReturn$sd = data.frame(resultSim[,1],resultSim[,1],resultSim[,1],resultSim[,1] )
    names(toReturn$sd) = c("mean","Q025","Q975", "sd")
    for(i in 1:dim(toReturn$sd)[1]){
      toReturn$sd$mean[i] = mean(tmpResults$sd[i,])
      quantile = quantile(tmpResults$sd[i,],c(0.025,0.975))
      toReturn$sd$Q025[i] = quantile[1]
      toReturn$sd$Q975[i] = quantile[2]
      toReturn$sd$sd[i] = sd(tmpResults$sd[i,])
      toReturn$sd$cv[i] = toReturn$sd$sd[i]/toReturn$sd$mean[i]
    }
    toReturn$Q025 = data.frame(resultSim[,1],resultSim[,1],resultSim[,1],resultSim[,1] )
    names(toReturn$Q025) = c("mean","Q025","Q975", "sd")
    for(i in 1:dim(toReturn$Q025)[1]){
      toReturn$Q025$mean[i] = mean(tmpResults$Q025[i,])
      quantile = quantile(tmpResults$Q025[i,],c(0.025,0.975))
      toReturn$Q025$Q025[i] = quantile[1]
      toReturn$Q025$Q975[i] = quantile[2]
      toReturn$Q025$sd[i] = sd(tmpResults$Q025[i,])
      toReturn$Q025$cv[i] = toReturn$Q025$sd[i]/toReturn$Q025$mean[i]
    }
    toReturn$Q975 = data.frame(resultSim[,1],resultSim[,1],resultSim[,1],resultSim[,1] )
    names(toReturn$Q975) = c("mean","Q025","Q975", "sd")
    for(i in 1:dim(toReturn$Q975)[1]){
      toReturn$Q975$mean[i] = mean(tmpResults$Q975[i,])
      quantile = quantile(tmpResults$Q975[i,],c(0.025,0.975))
      toReturn$Q975$Q025[i] = quantile[1]
      toReturn$Q975$Q975[i] = quantile[2]
      toReturn$Q975$sd[i] = sd(tmpResults$Q975[i,])
      toReturn$Q975$cv[i] = toReturn$Q975$sd[i]/toReturn$Q975$mean[i]
    }
  }
  #--------------------------------------------------------------------------

  return(toReturn)


}





#' investigateRemovalParallel
#' @description This is the same function as investigateRemoval, but do the calculations with several threads. Remove a proportion of the data in a given way. Return a list regarding changes of summery statistics.
#' @param removeProcedure Removal procedure, given as an integer. removeProcedure==1 means that data is removed at random.
#' @param RFA The roundfish area of interest.
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
#' @param doNotRemoveAbove  Do not remove fish that is larger than this (in cm).
#' @param lengthDivision  Take one otholit from each intervall (in cm).
#' @export
#' @return Returns a list with summary about changes in estimates by removal of data.
#' @examples
investigateRemovalParallel = function(RFA,species, year, quarter,dat ,
                              bootstrapProcedure, B, ALKprocedure,
                              removeProcedure,
                              nSim, whatToInvestigate, whatToRemove,typeOfAreaToInvestigate,
                              doNotRemoveAbove = 9999, propRemove = NULL,
                              lengthDivision=NULL){

  toReturn= list()

  #Calculate the mCPUE and possibly more with all data---------------------
  if(whatToInvestigate=="mean"){
    if(typeOfAreaToInvestigate =="RFA"){
      toReturn$WithFullData = CPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dat = dat,
                                      bootstrapProcedure = bootstrapProcedure, B = n, ALKprocedure = ALKprocedure,doBootstrap = FALSE)
    }else{
      toReturn$WithFullData = CPUEnorthSea(species = species, year = year, quarter = quarter,dat = dat,
                                           bootstrapProcedure = bootstrapProcedure, B = n, ALKprocedure = ALKprocedure,doBootstrap = FALSE)
    }
  }else{
    if(typeOfAreaToInvestigate =="RFA"){
      toReturn$WithFullData = CPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dat = dat,
                                      bootstrapProcedure = bootstrapProcedure, B = n, ALKprocedure = ALKprocedure)
    }else{
      toReturn$WithFullData = CPUEnorthSea(species = species, year = year, quarter = quarter,dat = dat,
                                           bootstrapProcedure = bootstrapProcedure, B = n, ALKprocedure = ALKprocedure)
    }
  }
  #--------------------------------------------------------------------------


  #Define a data frame were results shall be stored after foreach()----------
  tmpResults = list()
  tmpResults$mCPUE = matrix(0,confALK(species,quarter)$maxAge+1, nSim)
  tmpResults$bootstrapMean = matrix(0,confALK(species,quarter)$maxAge+1, nSim)
  tmpResults$sd = matrix(0,confALK(species,quarter)$maxAge+1, nSim)
  tmpResults$median = matrix(0,confALK(species,quarter)$maxAge+1, nSim)
  tmpResults$Q025 = matrix(0,confALK(species,quarter)$maxAge+1, nSim)
  tmpResults$Q975 = matrix(0,confALK(species,quarter)$maxAge+1, nSim)
  tmpResults$BiasCQ025 = matrix(0,confALK(species,quarter)$maxAge+1, nSim)
  tmpResults$BiasCQ075 = matrix(0,confALK(species,quarter)$maxAge+1, nSim)

  #--------------------------------------------------------------------------

  #Remove data and calulates mCPUE etc. with usage of several cores----------------
  resForEach <- foreach(i = 1:nSim,.packages='TestPackage') %dopar%  {

    print("Information about progress in otholit removal simulation: ")
    print(paste("Simulation ",i))

    datRemoved = removeData(year, quarter,species,dat,removeProcedure,whatToRemove,doNotRemoveAbove,lengthDivision,propRemove = propRemove)

    if(whatToInvestigate=="mean"){ #Return only information of changes in point estimate of CPUE to reduce computation time.
      if(typeOfAreaToInvestigate =="RFA"){
        resultSim = CPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dat = datRemoved,
                            bootstrapProcedure = bootstrapProcedure, B, ALKprocedure = ALKprocedure,doBootstrap = FALSE)
      }else{
        resultSim = CPUEnorthSea(species = species, year = year, quarter = quarter,dat = datRemoved,
                                 bootstrapProcedure = bootstrapProcedure, B, ALKprocedure = ALKprocedure,doBootstrap = FALSE)
      }
    }else{ #Do bootstrap with the reduced data set.
      if(typeOfAreaToInvestigate =="RFA"){
        resultSim = CPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dat = datRemoved,
                            bootstrapProcedure = bootstrapProcedure, B, ALKprocedure = ALKprocedure,doBootstrap = TRUE)
      }else{
        resultSim = CPUEnorthSea(species = species, year = year, quarter = quarter,dat = datRemoved,
                                 bootstrapProcedure = bootstrapProcedure, B, ALKprocedure = ALKprocedure,doBootstrap = TRUE)
      }
    }

    k = list() #List to be returned by foreach()
    k$mCPUE = resultSim$mCPUE
    k$bootstrapMean =resultSim$bootstrapMean
    k$median =resultSim$median
    k$sd =resultSim$sd
    k$Q025 =resultSim$Q025
    k$Q975 =resultSim$Q975
    # bias-corrected---------------------
    k$BiasCQ025 = resultSim$BiasCQ025
    k$BiasCQ075 = resultSim$BiasCQ075
    k
  }
  #--------------------------------------------------------------------------

  #Combine the results from foreach() in the list rmpResults-----------------
  for(i in 1:nSim){
    tmpResults$mCPUE[,i] = resForEach[[i]]$mCPUE
    tmpResults$bootstrapMean[,i]= resForEach[[i]]$bootstrapMean
    tmpResults$median[,i]= resForEach[[i]]$median
    tmpResults$sd[,i]= resForEach[[i]]$sd
    tmpResults$Q025[,i]= resForEach[[i]]$Q025
    tmpResults$Q975[,i]= resForEach[[i]]$Q975
  # bias-corrected------------
    tmpResults$BiasCQ025[,i]= resForEach[[i]]$BiasCQ025
    tmpResults$BiasCQ075[,i]= resForEach[[i]]$BiasCQ075
  }
  #--------------------------------------------------------------------------

  #Extract what we are interested in (mCPUE, sd and quantiles)----------------
  if(whatToInvestigate=="mean"){
    toReturn$mCPUE = data.frame(toReturn$WithFullData[,1],toReturn$WithFullData[,1] ,toReturn$WithFullData[,1],
                      toReturn$WithFullData[,1] ,toReturn$WithFullData[,1],toReturn$WithFullData[,1],toReturn$WithFullData[,1])
    names(toReturn$mCPUE) = c("mean","median", "Q025", "Q975","BiasCQ025", "BiasCQ075", "sd")
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
  }else{
    toReturn$mCPUE = data.frame(toReturn$WithFullData[,1],toReturn$WithFullData[,1],toReturn$WithFullData[,1],toReturn$WithFullData[,1] )
    names(toReturn$mCPUE) = c("mean","Q025","Q975", "sd")
    for(i in 1:dim(toReturn$mCPUE)[1]){
      toReturn$mCPUE$mean[i] = mean(tmpResults$mCPUE[i,])
      toReturn$mCPUE$median[i] = median(tmpResults$mCPUE[i,])
      quantile = quantile(tmpResults$mCPUE[i,],c(0.025,0.975))
      toReturn$mCPUE$Q025[i] = quantile[1]
      toReturn$mCPUE$Q975[i] = quantile[2]
      toReturn$mCPUE$sd[i] = sd(tmpResults$mCPUE[i,])
      toReturn$mCPUE$cv[i] = toReturn$mCPUE$sd[i]/toReturn$mCPUE$mean[i]
    }

    toReturn$bootstrapMean = data.frame(toReturn$WithFullData[,1],toReturn$WithFullData[,1],toReturn$WithFullData[,1],toReturn$WithFullData[,1] )
    names(toReturn$bootstrapMean) = c("mean","Q025","Q975", "sd")
    for(i in 1:dim(toReturn$bootstrapMean)[1]){
      toReturn$bootstrapMean$mean[i] = mean(tmpResults$bootstrapMean[i,])
      quantile = quantile(tmpResults$bootstrapMean[i,],c(0.025,0.975))
      toReturn$bootstrapMean$Q025[i] = quantile[1]
      toReturn$bootstrapMean$Q975[i] = quantile[2]
      toReturn$bootstrapMean$sd[i] = sd(tmpResults$bootstrapMean[i,])
      toReturn$bootstrapMean$cv[i] = toReturn$bootstrapMean$sd[i]/toReturn$bootstrapMean$mean[i]
    }
    toReturn$sd = data.frame(toReturn$WithFullData[,1],toReturn$WithFullData[,1],toReturn$WithFullData[,1],toReturn$WithFullData[,1] )
    names(toReturn$sd) = c("mean","Q025","Q975", "sd")
    for(i in 1:dim(toReturn$sd)[1]){
      toReturn$sd$mean[i] = mean(tmpResults$sd[i,])
      quantile = quantile(tmpResults$sd[i,],c(0.025,0.975))
      toReturn$sd$Q025[i] = quantile[1]
      toReturn$sd$Q975[i] = quantile[2]
      toReturn$sd$sd[i] = sd(tmpResults$sd[i,])
      toReturn$sd$cv[i] = toReturn$sd$sd[i]/toReturn$sd$mean[i]
    }
    toReturn$Q025 = data.frame(toReturn$WithFullData[,1],toReturn$WithFullData[,1],toReturn$WithFullData[,1],toReturn$WithFullData[,1] )
    names(toReturn$Q025) = c("mean","Q025","Q975", "sd")
    for(i in 1:dim(toReturn$Q025)[1]){
      toReturn$Q025$mean[i] = mean(tmpResults$Q025[i,])
      quantile = quantile(tmpResults$Q025[i,],c(0.025,0.975))
      toReturn$Q025$Q025[i] = quantile[1]
      toReturn$Q025$Q975[i] = quantile[2]
      toReturn$Q025$sd[i] = sd(tmpResults$Q025[i,])
      toReturn$Q025$cv[i] = toReturn$Q025$sd[i]/toReturn$Q025$mean[i]
    }
    toReturn$Q975 = data.frame(toReturn$WithFullData[,1],toReturn$WithFullData[,1],toReturn$WithFullData[,1],toReturn$WithFullData[,1] )
    names(toReturn$Q975) = c("mean","Q025","Q975", "sd")
    for(i in 1:dim(toReturn$Q975)[1]){
      toReturn$Q975$mean[i] = mean(tmpResults$Q975[i,])
      quantile = quantile(tmpResults$Q975[i,],c(0.025,0.975))
      toReturn$Q975$Q025[i] = quantile[1]
      toReturn$Q975$Q975[i] = quantile[2]
      toReturn$Q975$sd[i] = sd(tmpResults$Q975[i,])
      toReturn$Q975$cv[i] = toReturn$Q975$sd[i]/toReturn$Q975$mean[i]
    }
  }
  #--------------------------------------------------------------------------
  return(toReturn)
}



#' removeData
#' @description .
#' @param species The species of interest.
#' @param year The year of interest.
#' @param quarter The quarter of interest.
#' @param removeProcedure Removal procedure, given as an integer. removeProcedure==1 means that data is removed at random.
#' @param whatToRemove
#' @export
#' @return Returns a modified data set of the data used for calculating the CPUE. The data is modified by removing
#' observations in a certain procedure.
#' @examples
removeData = function(year, quarter,species, dat,removeProcedure=1,whatToRemove,doNotRemoveAbove,lengthDivision,propRemove = NULL){

  #Extract data which shall be thinned------------------------
  datToReturn = dat
  datToReturn$hh = datToReturn$hh[!is.na(datToReturn$hh$Year) &datToReturn$hh$Year ==year &
                                    !is.na(datToReturn$hh$Quarter) & datToReturn$hh$Quarter == quarter,]
  datToReturn$ca_hh = datToReturn$ca_hh[!is.na(datToReturn$ca_hh$Year) &datToReturn$ca_hh$Year ==year &
                                    !is.na(datToReturn$ca_hh$Quarter) & datToReturn$ca_hh$Quarter == quarter&
                                      !is.na(datToReturn$ca_hh$Species) &datToReturn$ca_hh$Species ==species,]
  datToReturn$hl_hh = datToReturn$hl_hh[!is.na(datToReturn$hl_hh$Year) &datToReturn$hl_hh$Year ==year &
                                    !is.na(datToReturn$hl_hh$Quarter) & datToReturn$hl_hh$Quarter == quarter,]
  #----------------------------------------------------------
  print("Removing otholits...")
  numberOfOtholits = dim(datToReturn$ca_hh)[1]

  #Remove data-----------------------------------------------
#  if("HH" %in% whatToRemove)datToReturn$hh = removeDataDetailedHH(datToReturn$hh,removeProcedure,propRemove)
  if("CA" %in% whatToRemove)datToReturn$ca_hh = removeDataDetailedCA(datDetailed = datToReturn$ca_hh,removeProcedure,species,quarter,doNotRemoveAbove, lengthDivision,propRemove = propRemove)
#  if("HL" %in% whatToRemove)datToReturn$hl_hh = removeDataDetailedHL(datToReturn$hl_hh,removeProcedure,propRemove)
  #----------------------------------------------------------

  #provide user with information regarding number of otholits removed-------
  print(paste("Removed ",numberOfOtholits- dim(datToReturn$ca_hh)[1], " otholits out of ", numberOfOtholits,".",sep = ""))
  #----------------------------------------------------------

  return(datToReturn)
}


#' removeDataDetailedCA
#' @description .
#' @param datDetailed The quarter of interest.
#' @param removeProcedure Removal procedure, given as an integer. removeProcedure==1 means that data is removed at random.
#' @export
#' @return Returns a modified data set of the data used for calculating the CPUE. The data is modified by removing
#' observations in a certain procedure.
#' @examples
removeDataDetailedCA = function(datDetailed,removeProcedure,species,quarter,doNotRemoveAbove,lengthDivision,propRemove = NULL){

  toReturn = datDetailed
  #Removes data in the procedure selected in "removeProcedure"-----------------------------------
  if(removeProcedure=="random"){
    length = dim(toReturn)[1]
    keep = sample(1:length,ceiling(length*(1-propRemove)))
    keep = unique(c(keep,which(toReturn$LngtCm>doNotRemoveAbove))) #Do not remove the fish larger than a certain level
    toReturn = toReturn[keep,]

    return(toReturn)
  }else if(removeProcedure=="stratified"){
    dLength = confALK(species = species,quarter = quarter)$lengthClassIntervallLengths
    if(dLength==1){
      lengthArray = sort(unique(floor(toReturn$LngtCm)))
      lengthArray = lengthArray[which(lengthArray<=doNotRemoveAbove)]#Do not remove the fish larger than a certain level
      for(length in lengthArray){
        for(RFA in 1:9){
          tmp = which(floor(toReturn$LngtCm)==length & toReturn$Roundfish==RFA)
          u = runif(length(tmp))
          remove = tmp[which(u<propRemove)] #
          if(length(remove)>0){
            toReturn = toReturn[-remove,]
          }
        }
      }
    }
    return(toReturn)
  }else if(removeProcedure=="edvin"){#Remove otholits such that only at random only one otholit is read from longer length intervalls
    toReturn = datDetailed[1,]
    for(id in unique(datDetailed$haul.id)){
      obsTmp = datDetailed[which(datDetailed$haul.id==id),]
      obsReduced = removeObsFromHaul(obsTmp,lengthDivision)
      toReturn = rbind(toReturn,obsReduced)
    }
    toReturn = toReturn[-1,]
    return(toReturn)
  }
  #---------------------------------------------------------------------------------
}

#' removeObsFromHaul
#' @description .
#' @param datDetailed The quarter of interest.
#' @param removeProcedure Removal procedure, given as an integer. removeProcedure==1 means that data is removed at random.
#' @param RFA The roundfish area of interest.
#' @export
#' @return Returns a modified data set of the data used for calculating the CPUE. The data is modified by removing
#' observations in a certain procedure.
#' @examples
removeObsFromHaul = function(obsTmp,lengthDivision){
  toReturn = obsTmp[1,]
  for(i in 2:length(lengthDivision)){
    obsInside = which(obsTmp$LngtCm>=lengthDivision[i-1]  & obsTmp$LngtCm<lengthDivision[i] )
    if(length(obsInside)>1){
      obsSelected = sample(obsInside,1)#Sample one of the observations to be keep.
    }else{
      obsSelected = obsInside
    }
    toReturn = rbind(toReturn,obsTmp[obsSelected,])
  }
  toReturn = toReturn[-1,]

  return(toReturn)
}



#' removeDataDetailedHL
#' @description .
#' @param datDetailed The quarter of interest.
#' @param removeProcedure Removal procedure, given as an integer. removeProcedure==1 means that data is removed at random.
#' @param RFA The roundfish area of interest.
#' @export
#' @return Returns a modified data set of the data used for calculating the CPUE. The data is modified by removing
#' observations in a certain procedure.
#' @examples
removeDataDetailedHL = function(datDetailed,removeProcedure,RFA){

  return(datDetailed)
}


#' removeDataDetailedHH
#' @description .
#' @param datDetailed The quarter of interest.
#' @param removeProcedure Removal procedure, given as an integer. removeProcedure==1 means that data is removed at random.
#' @param RFA The roundfish area of interest.
#' @export
#' @return Returns a modified data set of the data used for calculating the CPUE. The data is modified by removing
#' observations in a certain procedure.
#' @examples
removeDataDetailedHH = function(datDetailed,removeProcedure,RFA){

  return(datDetailed)
}


