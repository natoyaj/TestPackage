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
                                         bootstrapProcedure = bootstrapProcedure, B = B, ALKprocedure = ALKprocedure,doBootstrap = FALSE,lengthDivision = lengthDivision)
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






