#This R-file conisist of functions calculating the CPUEs of interest.

#' calcmCPUErfa
#' @description Calculates CPUE per length class in a given roundfish area.
#' @param RFA Roundfish area number.
#' @param species The species of interest.
#' @param year The year of interest.
#' @param quarter The quarter of interest.
#' @param data the datras-data needed.
#' @param ALK the ALK.
#' @export
#' @return Returns the mCPUE per length class in the given roundfish area.
#' @examples
#'
calcmCPUErfa = function(RFA,species,year, quarter, data, ALK = NULL)
{
  #Extract the data of interest-------------------------
  dataOfInterest = data[!is.na(data$Year) & data$Year == year&
                          !is.na(data$Quarter) & data$Quarter == quarter&
                          !is.na(data$Roundfish) & data$Roundfish == RFA ,]
  #-----------------------------------------------------

  #Construct a matrix with mCPUEs for each statistical rectangel---
  statRects = unique(dataOfInterest$StatRec)
  numberOfStatRectangles = length(statRects)
  nLengthClass = max(floor(dataOfInterest$LngtCm[which(dataOfInterest$Species ==species)])) #This is to be changed, e.g. find te lenght classes from ALK. TODO
  mCPUEstatRec = matrix(NA,nLengthClass,numberOfStatRectangles)
  #---------------------------------------------------------------

  #Calculate the mCPUEs for each statistical rectangangle---------
  if(numberOfStatRectangles==0) return("No observations in RFA")
  for(i in 1:numberOfStatRectangles)
  {
    cpueStatRec = calcmCPUEstatRec(statRec = statRects[i],species = species,year= year , quarter = quarter ,data = dataOfInterest, nLengthClass = nLengthClass)
    mCPUEstatRec[,i] = cpueStatRec
  }
  #---------------------------------------------------------------

  #Average over the statistical recangles and return mCPUE-----
  mCPUE = rep(NA,nLengthClass)
  for(i in 1:nLengthClass)
  {
    mCPUE[i] = mean(mCPUEstatRec[i,])# TODO: We must multiply with the proportion of the area covered by deep enough water.
  }
  return(mCPUE)
  #------------------------------------------------------------
}




#' calcmCPUEstatRec
#' @description Calculates CPUE per length class in a given statistical rectangle.
#' @param statRec Statistical area.
#' @param species The species of interest.
#' @param year The year of interest.
#' @param quarter The quarter of interest.
#' @param data the datras-data needed.
#' @param percentOfAreaRepresentative the percentage of the statical recangle within sea depth intervall
#' @export
#' @return Returns the mCPUE per length class in the given statistical rectangle
#' @examples
calcmCPUEstatRec = function(statRec,species,year, quarter, data, ALK = NULL,percentOfAreaRepresentative = NULL,nLengthClass)
{
  #Extract the number of hauls in the statistical area
  nHauls = length(unique(data$haul.id[which(data$StatRec == statRec)]))


  #Extract the data of interest-------------------------
  dataWithTheSpecies = data[!is.na(data$Species) & data$Species==species &
                          !is.na(data$Year) & data$Year == year&
                          !is.na(data$Quarter) & data$Quarter == quarter&
                          !is.na(data$StatRec) & data$StatRec == statRec ,]
  #-----------------------------------------------------

  #Calculates and returns mCPUE-----
  subfactor = dataWithTheSpecies$SubFactor

  CPUE = rep(0,nLengthClass)

  if(dim(dataWithTheSpecies)[1]>0)
  {
    for(i in 1:dim(dataWithTheSpecies)[1])
    {
      if(dataWithTheSpecies$DataType[i]=="R")
      {
        CPUE[floor(dataWithTheSpecies$LngtCm[i])] =  CPUE[floor(dataWithTheSpecies$LngtCm[i])] + (dataWithTheSpecies$Count[i]*60/dataWithTheSpecies$HaulDur[i])*subfactor[i]
      }else if(dataWithTheSpecies$DataType[i]=="C")
      {
        CPUE[floor(dataWithTheSpecies$LngtCm[i])]  =  CPUE[floor(dataWithTheSpecies$LngtCm[i])] + dataWithTheSpecies$HLNoAtLngt[i]*subfactor[i]
      }
    }
  }
  mCPUE = CPUE/nHauls

  return(mCPUE)
  #----------------------------------
}





#' calcmCPUErfaWithALK
#' @description Calculates CPUE per length class in a given roundfish area.
#' @param RFA Roundfish area number.
#' @param species The species of interest.
#' @param year The year of interest.
#' @param quarter The quarter of interest.
#' @param data the datras-data needed.
#' @param ALK the ALK.
#' @export
#' @return Returns the mCPUE per length class in the given roundfish area.
#' @examples
#'
calcmCPUErfaWithALK = function(RFA,species,year, quarter, data, ALK)
{

  #Extract the data of interest-------------------------
  dataOfInterest = data[!is.na(data$Year) & data$Year == year&
                          !is.na(data$Quarter) & data$Quarter == quarter&
                          !is.na(data$Roundfish) & data$Roundfish == RFA ,]
  #-----------------------------------------------------

  #Construct a matrix with mCPUEs for each statistical rectangel---
  statRects = unique(dataOfInterest$StatRec)
  numberOfStatRectangles = length(statRects)
  nAgeClasses = dim(ALK)[2]-1
  mCPUEstatRec = matrix(NA,nAgeClasses,numberOfStatRectangles)
  #---------------------------------------------------------------

  #Calculate the mCPUEs for each statistical rectangangle---------
  if(numberOfStatRectangles==0) return("No observations in RFA")
  for(i in 1:numberOfStatRectangles)
  {
    cpueStatRec = calcmCPUEstatRecWithALK(statRec = statRects[i],species = species,year= year , quarter = quarter, data = dataOfInterest,ALK = ALK)
    mCPUEstatRec[,i] = as.double(cpueStatRec)
  }
  #---------------------------------------------------------------

  #Average over the statistical recangles and return mCPUE-----
  weightUsed = rep(0,numberOfStatRectangles)
  for(i in 1:numberOfStatRectangles){
    weightUsed[i] = 1 #TODO: find weights for cod and make this parte species dependent
   # weightUsed[i] =  weightStatRec$Weight[which(weightStatRec$StatRec== statRects[i])]
  }

  mCPUE = rep(0,nAgeClasses)
  for(i in 1:nAgeClasses)
  {
    for(j in 1:numberOfStatRectangles){
      mCPUE[i] = mCPUE[i] + mCPUEstatRec[i,j] *weightUsed[j]/sum(weightUsed)
    }
  }
  return(mCPUE)
  #------------------------------------------------------------
}




#' calcmCPUEstatRecWithALK
#' @description Calculates CPUE per length class in a given statistical rectangle.
#' @param statRec Statistical area.
#' @param species The species of interest.
#' @param year The year of interest.
#' @param quarter The quarter of interest.
#' @param data the datras-data needed.
#' @param percentOfAreaRepresentative the percentage of the statical recangle within sea depth intervall
#' @param ALK the ALK.
#' @export
#' @return Returns the mCPUE per length class in the given statistical rectangle
#' @examples
calcmCPUEstatRecWithALK = function(statRec,species,year, quarter, data, ALK,percentOfAreaRepresentative = NULL)
{
  #Extract the number of hauls in the statistical area
  nHauls = length(unique(data$haul.id[which(data$StatRec == statRec)]))


  #Extract the data of interest-------------------------
  dataWithTheSpecies = data[!is.na(data$Species) & data$Species==species &
                              !is.na(data$Year) & data$Year == year&
                              !is.na(data$Quarter) & data$Quarter == quarter&
                              !is.na(data$StatRec) & data$StatRec == statRec ,]
  #-----------------------------------------------------


  #Calculates and returns mCPUE-----
  subfactor = dataWithTheSpecies$SubFactor

  nAgeClasses = dim(ALK)[2]-1
  CPUE = rep(0,nAgeClasses)

  if(dim(dataWithTheSpecies)[1]==0)return(CPUE)

  if(dim(dataWithTheSpecies)[1]>0)
  {
    for(i in 1:dim(dataWithTheSpecies)[1])
    {
      lineInAlkToUse = NA
      if(min(ALK$length)>dataWithTheSpecies$LngtCm[i])
      {
        lineInAlkToUse = 1
      }else if(max(ALK$length)<dataWithTheSpecies$LngtCm[i])
      {
        lineInAlkToUse = length(ALK$length)
      }else{
        lineInAlkToUse = min(which(ALK$length>=dataWithTheSpecies$LngtCm[i]))
      }

      if(dataWithTheSpecies$DataType[i]=="R")
      {
        CPUE =  CPUE + (dataWithTheSpecies$Count[i]*60/dataWithTheSpecies$HaulDur[i])*subfactor[i] * ALK[lineInAlkToUse,-1]/sum(ALK[lineInAlkToUse,-1])
      }else if(dataWithTheSpecies$DataType[i]=="C")
      {
        CPUE  =  CPUE + dataWithTheSpecies$HLNoAtLngt[i]*subfactor[i] * ALK[lineInAlkToUse,-1]/sum(ALK[lineInAlkToUse,-1])
      }
    }
  }
  mCPUE = CPUE/nHauls

  return(mCPUE)
  #----------------------------------
}




