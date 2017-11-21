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
    simData[[i]]$haul.id = paste(simData[[i]]$haul.id,i) #Needs unique haul.id, which is achived here.
  }
  simDataToBeReturned  =   do.call(rbind.data.frame,simData)
  #----------------------------------------------------

  return(simDataToBeReturned)
}



#' simTrawlHaulsHLStratified
#' @description Simulates trawl hauls with only length information used in the bootstrap procedure.
#' @param RFA Roundfish area number.
#' @param year The year of interest.
#' @param quarter The quarter of interest.
#' @export
#' @return Returns simulations of the dataras-data with length information on a similar format as the data used in the functions for calculating the CPUEs.
#' The simulated trawl hauls are simulated stratisfied on the statistical rectangles. TODO: choose a procedure if there is only one observation in the statistical recangle, I suggest to include the closest trawl haul no matter which statistical recangles it is assosiated with..
#' @examples
simTrawlHaulsHLStratified = function(RFA,year, quarter,data)
{
  #Extract the data of interest-------------------------
  dataOfInterest = data[!is.na(data$Year) & data$Year == year&
                          !is.na(data$Quarter) & data$Quarter == quarter&
                          !is.na(data$Roundfish) & data$Roundfish == RFA ,]
  #-----------------------------------------------------

  #Simulate trawl hauls---------------------------------
  statRec = unique(dataOfInterest$StatRec)
  simData = list(NULL)
  for(i in 1:length(statRec))
  {
    rec = statRec[i]
    trawls = unique(dataOfInterest$haul.id[dataOfInterest$StatRec==rec])

    if(length(trawls)==1)
    {
      #TODO, combine with the closest observation perhaps?
      sampledTreawls = sample(trawls,length(trawls),replace = TRUE)
      dTmp = dataOfInterest[dataOfInterest$haul.id==sampledTreawls[1],]
    }else{
      sampledTreawls = sample(trawls,length(trawls),replace = TRUE)
      dTmp = dataOfInterest[dataOfInterest$haul.id==sampledTreawls[1],]
      for(j in 2:length(trawls))
      {
        add = dataOfInterest[dataOfInterest$haul.id==sampledTreawls[j],]
        add$haul.id = paste(add$haul.id ,"i:",i," j:",j,sep = "") #Needs unique haul.id, which is achived here.
        dTmp = rbind(dTmp,add)
      }
    }
    simData[[i]]= dTmp

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
simTrawlHaulsCASimple = function(RFA,year, quarter,data)
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
    simData[[i]]$haul.id = paste(simData[[i]]$haul.id,i) #Needs unique haul.id, which is achived here.
  }
  simDataToBeReturned  =   do.call(rbind.data.frame,simData)
  #----------------------------------------------------

  return(simDataToBeReturned)
}

#' simTrawlHaulsCAStratified
#' @description Simulates trawl hauls with both length and age information used in the bootstrap procedure.
#' @param RFA Roundfish area number.
#' @param year The year of interest.
#' @param quarter The quarter of interest.
#' @export
#' @return Returns simulations of the dataras-data with both length and age information on a similar format as the data used in the functions for calculating the CPUEs
#' Sample stratisfied with respect to the length observed. This gives meaning since it is assumed that the ALK is similar in the whole roundfish area.
#' TODO: Choose a procedure of how to simulate if there is only one observation of a length class. I suggest the same suggestion as for simulating HL-date when there is only one observation in the statistical area: include the closest length class in the simulation.
#' @examples
simTrawlHaulsCAStratified = function(RFA,year, quarter,data,species = "Gadus morhua")
{
  #Extract the data of interest-------------------------
  dataOfInterest = data[!is.na(data$Year) & data$Year == year&
                          !is.na(data$Quarter) & data$Quarter == quarter&
                          !is.na(data$Roundfish) & data$Roundfish == RFA &
                          !is.na(data$Species) & data$Species == species &
                          !is.na(data$Age),]
  #-----------------------------------------------------

  #Simulate trawl hauls---------------------------------
  lengths = unique(sort(floor(dataOfInterest$LngtCm)))
  simData = list(NULL)

  for(i in lengths){
    n = sum(floor(dataOfInterest$LngtCm)==i)
    if(n ==1)
    {
      #TODO, combine with the closest length perhaps?
      dTmp = dataOfInterest[floor(dataOfInterest$LngtCm)==i,]
      dTmp = sample_n(dTmp, size = n,replace = TRUE)

    }else{
      dTmp = dataOfInterest[floor(dataOfInterest$LngtCm)==i,]
      dTmp = sample_n(dTmp, size = n,replace = TRUE)
    }
    simData[[i]]= dTmp

  }
  simDataToBeReturned  =   do.call(rbind.data.frame,simData)
  #------------------------------------------------------

  return(simDataToBeReturned)
}
