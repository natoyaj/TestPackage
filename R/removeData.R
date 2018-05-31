#' removeDataDetailed
#' @description .
#' @param datDetailed The quarter of interest.
#' @param removeProcedure Removal procedure, given as an integer. removeProcedure==1 means that data is removed at random.
#' @param RFA The roundfish area of interest.
#' @param propRemove proportion of data to remove.
#' @export
#' @return Returns a modified data set of the data used for calculating the CPUE. The data is modified by removing
#' observations in a certain procedure.
#' @examples
investigateRemoval = function(RFA,species, year, quarter,dat ,
                              bootstrapProcedure, B, ALKprocedure,
                              removeProcedure, propRemove,
                              outerBootstrapN, whatToInvestigate, whatToRemove,typeOfAreaToInvestigate){

  tmpResults = matrix(0,confALK(species,quarter)$maxAge+1, outerBootstrapN)


  for(i in 1:outerBootstrapN){
    datRemoved = removeData(year, quarter,dat,removeProcedure,propRemove,whatToRemove)

    if(whatToInvestigate=="mean"){
      if(typeOfAreaToInvestigate =="RFA"){
        resultSim = CPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dat = datRemoved,
                           bootstrapProcedure = bootstrapProcedure, B, ALKprocedure = ALKprocedure,doBootstrap = FALSE)
      }else{
        resultSim = CPUEnorthSea(species = species, year = year, quarter = quarter,dat = datRemoved,
                           bootstrapProcedure = bootstrapProcedure, B, ALKprocedure = ALKprocedure,doBootstrap = FALSE)
      }

      tmpResults[,i] = resultSim[,1]
    }else if (whatToInvestigate=="sd"){
      if(typeOfAreaToInvestigate =="RFA"){
        resultSim = CPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dat = datRemoved,
                            bootstrapProcedure = bootstrapProcedure, B, ALKprocedure = ALKprocedure,doBootstrap = TRUE)
      }else{
        resultSim = CPUEnorthSea(species = species, year = year, quarter = quarter,dat = datRemoved,
                                 bootstrapProcedure = bootstrapProcedure, B, ALKprocedure = ALKprocedure,doBootstrap = TRUE)
      }

      tmpResults[,i] = resultSim$sd
    }

    if(i==1)print("Information about progress in outer bootstrap: ")
    print(paste("Bootstrap sample ",i))
    print(tmpResults[,i])

  }


  toReturn = data.frame(resultSim[,1],resultSim[,1],resultSim[,1],resultSim[,1] )
  names(toReturn) = c("bootstrapMean","Q025","Q975", "sd")
  for(i in 1:dim(toReturn)[1]){
    toReturn$bootstrapMean[i] = mean(tmpResults[i,])
    quantile = quantile(tmpResults[i,],c(0.025,0.975))
    toReturn$Q025[i] = quantile[1]
    toReturn$Q975[i] = quantile[2]
    toReturn$sd[i] = sd(tmpResults[i,])
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
removeData = function(year, quarter,dat,removeProcedure=1,propRemove,whatToRemove){

  datToReturn = dat

  datToReturn$hh = datToReturn$hh[!is.na(datToReturn$hh$Year) &datToReturn$hh$Year ==year &
                                    !is.na(datToReturn$hh$Quarter) & datToReturn$hh$Quarter == quarter,]
  datToReturn$ca_hh = datToReturn$ca_hh[!is.na(datToReturn$ca_hh$Year) &datToReturn$ca_hh$Year ==year &
                                    !is.na(datToReturn$ca_hh$Quarter) & datToReturn$ca_hh$Quarter == quarter,]
  datToReturn$hl_hh = datToReturn$hl_hh[!is.na(datToReturn$hl_hh$Year) &datToReturn$hl_hh$Year ==year &
                                    !is.na(datToReturn$hl_hh$Quarter) & datToReturn$hl_hh$Quarter == quarter,]


  if("HH" %in% whatToRemove)datToReturn$hh = removeDataDetailedHH(datToReturn$hh,removeProcedure,propRemove)
  if("CA" %in% whatToRemove)datToReturn$ca_hh = removeDataDetailedCA(datToReturn$ca_hh,removeProcedure,propRemove)
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
removeDataDetailedCA = function(datDetailed,removeProcedure, propRemove){

  if(removeProcedure=="random"){
    length = dim(datDetailed)[1]
    keep = sample(1:length,ceiling(length*(1-propRemove)))
    datDetailed = datDetailed[keep,]

    return(datDetailed)
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


