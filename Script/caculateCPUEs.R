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
#' @return Returns the mCPUE per length class in the given roundfish area. TODO: include the ALK to return per age class given ALK instead
#' @examples
#'
calcmCPUErfa = function(RFA,species,year, quarter, data, ALK = NULL)
{
  #Extract the data of interest-------------------------
  dataOfInterest = data[!is.na(data$Species) & data$Species==species &
                          !is.na(data$Year) & data$Year == year&
                          !is.na(data$Quarter) & data$Quarter == quarter&
                          !is.na(data$Roundfish) & data$Roundfish == RFA ,]
  #-----------------------------------------------------

  #Construct a matrix with mCPUEs for each statistical rectangel---
  statRects = unique(dataOfInterest$StatRec)
  numberOfStatRectangles = length(statRects)
  nLengthClass = max(floor(dataOfInterest$LngtCm)) #This is to be changed, e.g. find te lenght classes from ALK. TODO
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
    mCPUE[i] = mean(mCPUEstatRec[i,])
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
#' @return Returns the mCPUE per length class in the given statistical rectangle TODO, include the ALK to return per age class given ALK instead
#' @examples
calcmCPUEstatRec = function(statRec,species,year, quarter, data, ALK = NULL,percentOfAreaRepresentative = NULL,nLengthClass)
{
  #Extract the data of interest-------------------------
  dataOfInterest = data[!is.na(data$Species) & data$Species==species &
                          !is.na(data$Year) & data$Year == year&
                          !is.na(data$Quarter) & data$Quarter == quarter&
                          !is.na(data$StatRec) & data$StatRec == statRec ,]
  #-----------------------------------------------------

  #Calculates and returns mCPUE-----
  nHauls = length(unique(dataOfInterest$haul.id))
  subfactor = dataOfInterest$SubFactor

  CPUE = rep(0,nLengthClass)

  for(i in 1:dim(dataOfInterest)[1])
  {
    if(dataOfInterest$DataType[i]=="R")
    {
      CPUE[floor(dataOfInterest$LngtCm[i])] =  CPUE[floor(dataOfInterest$LngtCm[i])] + (dataOfInterest$Count[i]*60/dataOfInterest$HaulDur[i])*subfactor[i]
    }else if(dataOfInterest$DataType[i]=="C")
    {
      CPUE[floor(dataOfInterest$LngtCm[i])]  =  CPUE[floor(dataOfInterest$LngtCm[i])] + dataOfInterest$HLNoAtLngt[i]*subfactor[i]
    }
  }

  mCPUE = CPUE/nHauls
  return(mCPUE)
  #----------------------------------
}


