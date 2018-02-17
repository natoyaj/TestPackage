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


#' simTrawlHaulsHLdatras
#' @description Simulates trawl hauls with only length information used in the bootstrap procedure.
#' Keep the number of trawl ahuls within each statistical rectangle fixed, and sample them with replacement from the whole RFA.
#' @param RFA Roundfish area number.
#' @param year The year of interest.
#' @param quarter The quarter of interest.
#' @export
#' @return Returns simulations of the dataras-data with length information on a similar format as the data used in the functions for calculating the CPUEs
#' @examples
simTrawlHaulsHLdatras = function(RFA,year, quarter,data)
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


  #Define a help variable for keeping the number of trawl hauls within each statistical rectangle fixed----
  rectangleID = rep(NA,nSim)
  for(i in 1:nSim)
  {
    rectangleID[i] = unique(dataOfInterest$StatRec[dataOfInterest$haul.id== haulsID[i]])
  }
  #-----------------------------------------------------

  simData = list(NULL)
  for(i in 1:nSim)
  {
    simData[[i]]= dataOfInterest[dataOfInterest$haul.id==simHauls[i],]
    simData[[i]]$haul.id = paste(simData[[i]]$haul.id,i) #Needs unique haul.id, which is achived here.

    simData[[i]]$StatRec = rectangleID[i] #Overwrite the statstical rectangle to keep the number of trawl hauls within each statistical rectangle fixed.
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
simTrawlHaulsHLStratified = function(RFA,year, quarter,data, loc = NULL)
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
      idClosest = loc$shortesDist[which(loc$uniqueId == trawls)]
      toSample = c(toString(idClosest),toString(trawls[1]))
      sampledTreawls = sample(toSample,1)
      dTmp = dataOfInterest[dataOfInterest$haul.id==sampledTreawls,]
      dTmp$haul.idReal = trawls[1] #The id to the real trawl taken at that location
      dTmp$haul.id = paste(dTmp$haul.id ,"i:",i,sep = "") #Needs unique haul.id, which is achived here. Used in the model based approach
      dTmp$StatRec = rec
    }else{
      sampledTreawls = sample(trawls,length(trawls),replace = TRUE)
      dTmp = dataOfInterest[dataOfInterest$haul.id==sampledTreawls[1],]
      dTmp$haul.idReal = trawls[1] #The id to the real trawl taken at that location
      dTmp$haul.id = paste(dTmp$haul.id ,"i:",i,sep = "") #Needs unique haul.id, which is achived here. Used in the model based approach

      for(j in 2:length(trawls))
      {
        add = dataOfInterest[dataOfInterest$haul.id==sampledTreawls[j],]
        add$haul.idReal = trawls[j] #The id to the real trawl taken at that location. Used in the model based approach
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
                          !is.na(data$Roundfish) & data$Roundfish == RFA,]
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

  for(i in 1:length(lengths)){
    l = lengths[i]
    n = sum(floor(dataOfInterest$LngtCm)==l)
    if(n ==1)
    {
      if(l<max(lengths) & l>min(lengths))
      {
        distDown = l-lengths[i-1]
        distUp = lengths[i+1] -l
        if(distUp == distDown) distDown = distDown + 0.5 - runif(1)

        if(distDown<distUp)
        {
          extra = which(floor(dataOfInterest$LngtCm)==lengths[i-1])
        }else{
          extra = which(floor(dataOfInterest$LngtCm)==lengths[i+1])
        }
      }else if(l==min(lengths)){
        extra = which(floor(dataOfInterest$LngtCm)==lengths[i-1])
      }else if(l==max(lengths)){
        extra = which(floor(dataOfInterest$LngtCm)==lengths[i+1])
      }

      whichToAdd = sample(length(extra),1)
      dTmp = rbind(dataOfInterest[floor(dataOfInterest$LngtCm)==l,],
                   dataOfInterest[extra[whichToAdd],])
      dTmp$LngtCm = l
      dTmp = sample_n(dTmp, size = n,replace = TRUE)

    }else{
      dTmp = dataOfInterest[floor(dataOfInterest$LngtCm)==l,]
      dTmp = sample_n(dTmp, size = n,replace = TRUE)
    }
    simData[[l]]= dTmp

  }
  simDataToBeReturned  =   do.call(rbind.data.frame,simData)
  #------------------------------------------------------

  return(simDataToBeReturned)
}









#' simCaHlSimultaniousyStratified
#' @description Simulates trawl hauls with only length information used in the bootstrap procedure.
#' @param RFA Roundfish area number.
#' @param year The year of interest.
#' @param quarter The quarter of interest.
#' @export
#' @return Returns simulations of the dataras-data with length information on a similar format as the data used in the functions for calculating the CPUEs.
#' The simulated trawl hauls are simulated stratisfied on the statistical rectangles. TODO: choose a procedure if there is only one observation in the statistical recangle, I suggest to include the closest trawl haul no matter which statistical recangles it is assosiated with..
#' @examples
simCaHlSimultaniousyStratified = function(RFA,year, quarter,data, loc = NULL)
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
      idClosest = loc$shortesDist[which(loc$uniqueId == trawls)]
      toSample = c(toString(idClosest),toString(trawls[1]))
      sampledTreawls = sample(toSample,1)
      dTmp = dataOfInterest[dataOfInterest$haul.id==sampledTreawls,]
      dTmp$StatRec = rec

    }else{
      sampledTreawls = sample(trawls,length(trawls),replace = TRUE)
      dTmp = dataOfInterest[dataOfInterest$haul.id==sampledTreawls[1],]

      for(j in 2:length(trawls))
      {
        add = dataOfInterest[dataOfInterest$haul.id==sampledTreawls[j],]
        dTmp = rbind(dTmp,add)
      }
    }
    simData[[i]]= dTmp

  }
  simDataToBeReturned  =   do.call(rbind.data.frame,simData)
  #----------------------------------------------------
  return(simDataToBeReturned)

}

