#' simTrawlHaulsHLdatras
#' @description Simulates trawl hauls with only length information used in the bootstrap procedure.
#' Keep the number of trawl ahuls within each statistical rectangle fixed, and sample them with replacement from the whole RFA.
#' @param RFA Roundfish area number.
#' @param year The year of interest.
#' @param quarter The quarter of interest.
#' @export
#' @return Returns simulations of the dataras-data with length information on a similar format as the data used in the functions for calculating the CPUEs
#' @examples
simTrawlHaulsHLdatras = function(RFA,year, quarter,data,ca_hh)
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


  simDataCA = ca_hh[1,]#Define the structure in the data, this line is removed later.
  for(j in 1:length(simHauls)){
    tmpCA = ca_hh[which(ca_hh$haul.id== simHauls[j]),]
    if(dim(tmpCA)[1]>0){
      simDataCA = rbind(simDataCA,tmpCA)
    }
  }
  simDataCA = simDataCA[-1,]#Removes the first line which was created for defining the structure of the data


  #Define a help variable for keeping the number of trawl hauls within each statistical rectangle fixed----
  rectangleID = rep(NA,nSim)
  for(i in 1:nSim)
  {
    rectangleID[i] = as.character(unique(dataOfInterest$StatRec[dataOfInterest$haul.id== haulsID[i]]))
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

  toReturn = list()
  toReturn$ca_hh = simDataCA
  toReturn$hl_hh = simDataToBeReturned
  return(toReturn)
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
simTrawlHaulsCAdatras = function(RFA,year, quarter,data,species = "Gadus morhua")
{
  #Extract the data of interest-------------------------
  dataOfInterest = data[!is.na(data$Year) & data$Year == year&
                          !is.na(data$Quarter) & data$Quarter == quarter&
                          !is.na(data$Roundfish) & data$Roundfish == RFA &
                          !is.na(data$Species) & data$Species == species &
                          !is.na(data$Age),]
  #-----------------------------------------------------

  if(dim(dataOfInterest)[1]==0){
    return(dataOfInterest)
  }
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

      whichToAdd = sample(length(extra),1) #Extract one which is sampled with 0.5 probability
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
simCaHlSimultaniousyStratified = function(RFA,year, quarter,dataHH, loc = NULL)
{
  #Extract the data of interest-------------------------
  dataOfInterest = dataHH[!is.na(dataHH$Year) & dataHH$Year == year&
                          !is.na(dataHH$Quarter) & dataHH$Quarter == quarter&
                          !is.na(dataHH$Roundfish) & dataHH$Roundfish == RFA ,]
  #-----------------------------------------------------

  if(dim(dataOfInterest)[1] ==1){
    warning(paste("Only one haul in the whole RFA: ", RFA, sep = ""))
    dataOfInterest$originalIdAtThisLocation = toString(dataOfInterest$haul.id[1])
    return(dataOfInterest)
  }
  #Simulate trawl hauls---------------------------------
  statRec = unique(dataOfInterest$StatRec)
  simData = list(NULL)
  for(i in 1:length(statRec))
  {
    rec = statRec[i]
    trawls = unique(dataOfInterest$haul.id[dataOfInterest$StatRec==rec])
    lon = dataOfInterest$lon[is.element(dataOfInterest$haul.id,trawls)]
    lat = dataOfInterest$lat[is.element(dataOfInterest$haul.id,trawls)]

    if(length(trawls)==1)
    {
      idClosest = loc$shortesDist[which(loc$uniqueId == trawls)]
      toSample = c(toString(idClosest),toString(trawls[1]))
      if(length(idClosest)==0){
        toSample = toString(trawls[1])
        print("There was an haul with zero information in HL-data. This was strange, we should use the HH data when defining all hauls, this is TODO")
      }
      sampledTreawls = sample(toSample,1)
      dTmp = dataOfInterest[dataOfInterest$haul.id==sampledTreawls,]
      dTmp$StatRec = rec
      dTmp$lon = lon
      dTmp$lat = lat

      dTmp$originalIdAtThisLocation = toString(trawls[1])
    }else{
      sampledTreawls = sample(trawls,length(trawls),replace = TRUE)
      dTmp = dataOfInterest[dataOfInterest$haul.id==sampledTreawls[1],]


      dTmp$lon = lon[1]
      dTmp$lat = lat[1]
      dTmp$originalIdAtThisLocation = toString(trawls[1])

      for(j in 2:length(trawls))
      {
        add = dataOfInterest[dataOfInterest$haul.id==sampledTreawls[j],]
        add$lon = lon[j]
        add$lat = lat[j]
        add$originalIdAtThisLocation = toString(trawls[j])

        dTmp = rbind(dTmp,add)
      }
    }
    simData[[i]]= dTmp

  }
  simDataToBeReturned  =   do.call(rbind.data.frame,simData)
  #----------------------------------------------------
  return(simDataToBeReturned)

}

#' sampleCA
#' @description .
#' @param datDetailed The quarter of interest.
#' @export
#' @return Returns a modified data set of the data used for calculating the CPUE. The data is modified by removing
#' observations in a certain procedure.
#' @examples
sampleCA = function(datDetailed,species,quarter,lengthDivision,samplesWithinEachIntervall,hl_hh){
  toReturn = datDetailed
  toReturn = datDetailed[1,]
  for(id in unique(datDetailed$haul.id)){
    obsTmp = datDetailed[which(datDetailed$haul.id==id),]
    obsReduced = sampleCAHaul(obsTmp,lengthDivision, samplesWithinEachIntervall,species = species,hl_hh = hl_hh)
    toReturn = rbind(toReturn,obsReduced)
  }
  toReturn = toReturn[-1,]

  return(toReturn)
}

#' sampleCAHaul
#' @description .
#' @param datDetailed The quarter of interest.
#' @param RFA The roundfish area of interest.
#' @export
#' @return Returns a modified data set of the data used for calculating the CPUE. The data is modified by removing
#' observations in a certain procedure.
#' @examples
sampleCAHaul = function(obsTmp,lengthDivision,samplesWithinEachIntervall,species,hl_hh){
  toReturn = obsTmp[1,]
  for(i in 2:(length(lengthDivision)+1)){
    if(i <= length(lengthDivision)){
      obsInside = which(obsTmp$LngtCm>=lengthDivision[i-1]  & obsTmp$LngtCm<lengthDivision[i])
    }else if(i>length(lengthDivision)){
      obsInside = which(obsTmp$LngtCm>=lengthDivision[i-1])
    }
    if(length(obsInside)>1){
      whichLengthsInside = sort(unique(floor(obsTmp$LngtCm[obsInside])))
      psaudoPopulation = NULL
      counter = 1
      for(j in 1:length(whichLengthsInside)){
        insideThisLength = round(obsInHL(species = species,hl_hh = hl_hh,id = unique(obsTmp$haul.id),length = whichLengthsInside[j]))

        nTmp = sum(floor(obsTmp$LngtCm)==whichLengthsInside[j])#Number of observed ages in this sub length group
        repeatedObs =  rep(which(floor(obsTmp$LngtCm)==whichLengthsInside[j]),
                            floor(insideThisLength/nTmp))
        extraObs = sample(which(floor(obsTmp$LngtCm)==whichLengthsInside[j]),(insideThisLength %% nTmp),replace = FALSE)

        if(insideThisLength!=0){
          psaudoPopulation[counter:(counter + insideThisLength-1)] = c(repeatedObs,extraObs)
          counter = counter + insideThisLength
        }else{
          #This should not happen, a length was reported in CA data but not in HL data.
          #print("Observation in CA data was not in HL data")
          #print("Length")
          #print(whichLengthsInside[j])
          #print("ID: ")
          #print(unique(obsTmp$haul.id))
          psaudoPopulation[counter:((counter + length(c(which(floor(obsTmp$LngtCm)==whichLengthsInside[j]),extraObs))-1))] = c(which(floor(obsTmp$LngtCm)==whichLengthsInside[j]),extraObs)
          counter = counter + length(c(which(floor(obsTmp$LngtCm)==whichLengthsInside[j]),extraObs))
        }
      }

      nSample = min(samplesWithinEachIntervall,length(obsInside))
      obsSelected = sample(psaudoPopulation,size = nSample, replace = FALSE) #Sample without replecement from the psaudo population
    }else{
      obsSelected = obsInside
    }
    toReturn = rbind(toReturn,obsTmp[obsSelected,])
  }
  toReturn = toReturn[-1,]

  return(toReturn)
}


