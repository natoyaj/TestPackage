#' removeDataDetailed
#' @description .
#' @param datDetailed The quarter of interest.
#' @param removeProcedure Removal procedure, given as an integer. removeProcedure==1 means that data is removed at random.
#' @param RFA The roundfish area of interest.
#' @param prop proportion of data to remove.
#' @export
#' @return Returns a modified data set of the data used for calculating the CPUE. The data is modified by removing
#' observations in a certain procedure.
#' @examples
investigateRemoval = function(RFA,species, year, quarter,dat ,
                              bootstrapProcedure, B, ALKprocedure,
                              removeProcedure, prop,
                              outerBootstrapN, whatToInvestigate, whatToRemove){

  tmpResults = matrix(0,confALK(species,quarter)$maxAge+1, outerBootstrapN)


  for(i in 1:outerBootstrapN){
    datRemoved = removeData(RFA, year, quarter,dat,removeProcedure,prop,whatToRemove)

    reultSim = CPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dat = datRemoved,
            bootstrapProcedure = bootstrapProcedure, B = n, ALKprocedure = ALKprocedure,doBootstrap = FALSE)

    if(whatToInvestigate=="mean"){
      tmpResults[,i] = reultSim[,1]
    }

    if(i==1)print("Information about progress in outer bootstrap: ")
    print(paste("Bootstrap sample ",i))
    print(tmpResults[,i])

  }


  toReturn = data.frame(reultSim[,1],reultSim[,1],reultSim[,1],reultSim[,1] )
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
#' @param RFA The roundfish area of interest.
#' @param species The species of interest.
#' @param year The year of interest.
#' @param quarter The quarter of interest.
#' @param removeProcedure Removal procedure, given as an integer. removeProcedure==1 means that data is removed at random.
#' @param prop proportion of data to remove.
#' @export
#' @return Returns a modified data set of the data used for calculating the CPUE. The data is modified by removing
#' observations in a certain procedure.
#' @examples
removeData = function(RFA, year, quarter,dat,removeProcedure=1,prop=0.5,whatToRemove){

  datToReturn = dat

  datToReturn$hh = datToReturn$hh[!is.na(datToReturn$hh$Year) &datToReturn$hh$Year ==year &
                                    !is.na(datToReturn$hh$Quarter) & datToReturn$hh$Quarter == quarter,]
  datToReturn$ca_hh = datToReturn$ca_hh[!is.na(datToReturn$ca_hh$Year) &datToReturn$ca_hh$Year ==year &
                                    !is.na(datToReturn$ca_hh$Quarter) & datToReturn$ca_hh$Quarter == quarter,]
  datToReturn$hl_hh = datToReturn$hl_hh[!is.na(datToReturn$hl_hh$Year) &datToReturn$hl_hh$Year ==year &
                                    !is.na(datToReturn$hl_hh$Quarter) & datToReturn$hl_hh$Quarter == quarter,]


  if("HH" %in% whatToRemove)datToReturn$hh = removeDataDetailedHH(datToReturn$hh,removeProcedure,prop,RFA)
  if("CA" %in% whatToRemove)datToReturn$ca_hh = removeDataDetailedCA(datToReturn$ca_hh,removeProcedure,prop,RFA)
  if("HL" %in% whatToRemove)datToReturn$hl_hh = removeDataDetailedHL(datToReturn$hl_hh,removeProcedure,prop,RFA)

  return(datToReturn)
}


#' removeDataDetailedCA
#' @description .
#' @param datDetailed The quarter of interest.
#' @param removeProcedure Removal procedure, given as an integer. removeProcedure==1 means that data is removed at random.
#' @param RFA The roundfish area of interest.
#' @param prop proportion of data to remove.
#' @export
#' @return Returns a modified data set of the data used for calculating the CPUE. The data is modified by removing
#' observations in a certain procedure.
#' @examples
removeDataDetailedCA = function(datDetailed,removeProcedure, prop,RFA){

  if(removeProcedure=="random"){
    length = dim(datDetailed)[1]
    keep = sample(1:length,ceiling(length*(1-prop)))
    datDetailed = datDetailed[keep,]

    return(datDetailed)
  }
}


#' removeDataDetailedHL
#' @description .
#' @param datDetailed The quarter of interest.
#' @param removeProcedure Removal procedure, given as an integer. removeProcedure==1 means that data is removed at random.
#' @param RFA The roundfish area of interest.
#' @param prop proportion of data to remove.
#' @export
#' @return Returns a modified data set of the data used for calculating the CPUE. The data is modified by removing
#' observations in a certain procedure.
#' @examples
removeDataDetailedHL = function(datDetailed,removeProcedure, prop,RFA){

  return(datDetailed)
}


#' removeDataDetailedHH
#' @description .
#' @param datDetailed The quarter of interest.
#' @param removeProcedure Removal procedure, given as an integer. removeProcedure==1 means that data is removed at random.
#' @param RFA The roundfish area of interest.
#' @param prop proportion of data to remove.
#' @export
#' @return Returns a modified data set of the data used for calculating the CPUE. The data is modified by removing
#' observations in a certain procedure.
#' @examples
removeDataDetailedHH = function(datDetailed,removeProcedure, prop,RFA){

  return(datDetailed)
}


