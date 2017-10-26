#This R-file consist of the functions which are doing the simulations in the bootstrap

#' simTrawlHaulsHL
#' @description Simulates trawl hauls with only length information used in the bootstrap procedure.
#' @param RFA Roundfish area number.
#' @param year The year of interest.
#' @param quarter The quarter of interest.
#' @export
#' @return Returns simulations of the dataras-data with length information on a similar format as the data used in the functions for calculating the CPUEs
#' @examples
simTrawlHaulsHL = function(RFA,year, quarter)
{
  #TODO
}



#' simTrawlHaulsCA
#' @description Simulates trawl hauls with both length and age information used in the bootstrap procedure.
#' @param RFA Roundfish area number.
#' @param year The year of interest.
#' @param quarter The quarter of interest.
#' @export
#' @return Returns simulations of the dataras-data with both length and age information on a similar format as the data used in the functions for calculating the CPUEs
#' @examples
simTrawlHaulsCA = function(RFA,year, quarter)
{
  #TODO
}



#' simTrawlHaulsHLSimple
#' @description Simulates trawl hauls with only length information used in the bootstrap procedure.
#' @param RFA Roundfish area number.
#' @param year The year of interest.
#' @param quarter The quarter of interest.
#' @export
#' @return Returns simulations of the dataras-data with length information on a similar format as the data used in the functions for calculating the CPUEs
#' @examples
simTrawlHaulsHLSimple = function(RFA,year, quarter,data)
{
  #Extract the data of interest-------------------------
  dataOfInterest = data[!is.na(data$Year) & data$Year == year&
                          !is.na(data$Quarter) & data$Quarter == quarter&
                          !is.na(data$Roundfish) & data$Roundfish == RFA ,]
  #-----------------------------------------------------

  #Simulate trawl hauls---------------------------------
  haulsID = unique(dataOfInterest$haul.id)
  nSim = length(haulsID)
  simHauls = sample(haulsID,nSim,replace = T)

  simData = list(NULL)

  for(i in 1:nSim)
  {
    simData[[i]]= dataOfInterest[dataOfInterest$haul.id==simHauls[i],]
  }
  simDataToBeReturned  =   do.call(rbind.data.frame,simData)
  #----------------------------------------------------

  return(simDataToBeReturned)
}



#' simTrawlHaulsCASimple
#' @description Simulates trawl hauls with both length and age information used in the bootstrap procedure.
#' @param RFA Roundfish area number.
#' @param year The year of interest.
#' @param quarter The quarter of interest.
#' @export
#' @return Returns simulations of the dataras-data with both length and age information on a similar format as the data used in the functions for calculating the CPUEs
#' @examples
simTrawlHaulsCASimple = function(RFA,year, quarter)
{
  #TODO
}
