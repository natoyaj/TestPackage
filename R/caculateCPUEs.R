#' calcmCPUErfa
#' @description Calculates CPUE per length class in a given roundfish area.
#' @param RFA Roundfish area number.
#' @param species The species of interest.
#' @param year The year of interest.
#' @param quarter The quarter of interest.
#' @param data the datras-data needed.
#' @export
#' @return Returns the mCPUE per length class in the given roundfish area.
#' @examples
#'
calcmCPUErfa = function(RFA,species,year, quarter, data, weightStatRec = NULL, useICESindexArea = FALSE)
{
  #Extract the data of interest-------------------------
  dataOfInterest = data[!is.na(data$Year) & data$Year == year&
                          !is.na(data$Quarter) & data$Quarter == quarter&
                          !is.na(data$Roundfish) & data$Roundfish == RFA ,]
  #-----------------------------------------------------

  #Construct a matrix with mCPUEs for each statistical rectangel---
  statRects = unique(dataOfInterest$StatRec)
  if(useICESindexArea){
    #Exclude some of the statistical rectanlges not included in the index area.
    statRecsOfinterest = extractStatRecIndexArea(species)
    statRects = statRects[which(statRects %in% statRecsOfinterest)]
  }
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
  weightUsed = rep(0,numberOfStatRectangles)
  for(i in 1:numberOfStatRectangles){
    if(species =="Pollachius virens"){
      weightUsed[i] =  weightStatRec$Weight[which(weightStatRec$StatRec== statRects[i])]
    }else{
      weightUsed[i] = 1
    }
  }

  mCPUE = rep(0,nLengthClass)
  for(i in 1:nLengthClass)
  {
    for(j in 1:numberOfStatRectangles){
      mCPUE[i] = mCPUE[i] + mCPUEstatRec[i,j] *weightUsed[j]/sum(weightUsed)
    }
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
#' @export
#' @return Returns the mCPUE per length class in the given statistical rectangle
#' @examples
calcmCPUEstatRec = function(statRec,species,year, quarter, data,nLengthClass)
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
        CPUE[floor(dataWithTheSpecies$LngtCm[i])] =   CPUE[floor(dataWithTheSpecies$LngtCm[i])] + (dataWithTheSpecies$HLNoAtLngt[i]*60/dataWithTheSpecies$HaulDur[i])*subfactor[i]
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


#' calcmCPUErfaAreaBasedALK
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
calcmCPUErfaAreaBasedALK = function(RFA,species,year, quarter, data, ALK, weightStatRec = NULL, ALKprocedure = "datras", useICESindexArea = FALSE)
{
  nFoundWithin = 0
  nNotFoundWithin = 0

  #Extract the data of interest-------------------------
  dataOfInterest = data[!is.na(data$Year) & data$Year == year&
                          !is.na(data$Quarter) & data$Quarter == quarter&
                          !is.na(data$Roundfish) & data$Roundfish == RFA ,]
  #-----------------------------------------------------

  #Investigate if it is observed zero data---
  tmp = dataOfInterest[!is.na(dataOfInterest$Roundfish) & dataOfInterest$Roundfish == RFA&
                               !is.na(dataOfInterest$Species) & dataOfInterest$Species == species,]
  if(sum(!is.na(tmp))==0){
    nAgeClasses = dim(ALK)[2]-1
    mCPUE = rep(0,nAgeClasses)
    return(mCPUE)
  }
  #------------------------------------------

  #Construct a matrix with mCPUEs for each statistical rectangel---
  statRects = unique(dataOfInterest$StatRec)
  if(useICESindexArea){
    #Exclude some of the statistical rectanlges not included in the index area.
    statRecsOfinterest = extractStatRecIndexArea(species)
    statRects = statRects[which(statRects %in% statRecsOfinterest)]
  }
  numberOfStatRectangles = length(statRects)
  nAgeClasses = dim(ALK)[2]-1
  mCPUEstatRec = matrix(NA,nAgeClasses,numberOfStatRectangles)
  #---------------------------------------------------------------

  #Calculate the mCPUEs for each statistical rectangangle---------
  if(numberOfStatRectangles==0) return("No observations in RFA")
  for(i in 1:numberOfStatRectangles)
  {
    cpueStatRec = calcmCPUEstatRecAreaBasedALK(statRec = statRects[i],species = species,year= year , quarter = quarter, data = dataOfInterest,ALK = ALK)
    mCPUEstatRec[,i] = as.double(cpueStatRec)

    if(!is.null(attributes(cpueStatRec)$nFoundWithin) & !is.null(attributes(cpueStatRec)$nNotFoundWithin)){
      nFoundWithin = nFoundWithin + attributes(cpueStatRec)$nFoundWithin
      nNotFoundWithin = nNotFoundWithin + attributes(cpueStatRec)$nNotFoundWithin
    }
  }
  #---------------------------------------------------------------

  #Average over the statistical recangles and return mCPUE-----
  weightUsed = rep(0,numberOfStatRectangles)
  for(i in 1:numberOfStatRectangles){
    if(species =="Pollachius virens"){
      if(statRects[i] %in% weightStatRec$StatRec){
        weightUsed[i] =  weightStatRec$Weight[which(weightStatRec$StatRec== statRects[i])]
      }else{
        weightUsed[i] =  0 #Haul is in RFA 10 which is not included in the index for Saithe, this should have been included in the weighting file
      }
    }else{
      weightUsed[i] = 1
    }
  }

  mCPUE = rep(0,nAgeClasses)
  for(i in 1:nAgeClasses)
  {
    for(j in 1:numberOfStatRectangles){
      mCPUE[i] = mCPUE[i] + mCPUEstatRec[i,j] *weightUsed[j]/sum(weightUsed)
    }
  }

  attributes(mCPUE)$nFoundWithin = nFoundWithin
  attributes(mCPUE)$nNotFoundWithin = nNotFoundWithin

  return(mCPUE)
  #------------------------------------------------------------


}

#' calcmCPUEstatRecAreaBasedALK
#' @description Calculates CPUE per length class in a given statistical rectangle.
#' @param statRec Statistical area.
#' @param species The species of interest.
#' @param year The year of interest.
#' @param quarter The quarter of interest.
#' @param data the datras-data needed.
#' @param ALK the ALK.
#' @export
#' @return Returns the mCPUE per length class in the given statistical rectangle
#' @examples
calcmCPUEstatRecAreaBasedALK = function(statRec,species,year, quarter, data, ALK)
{
  nFoundWithin = 0
  nNotFoundWithin = 0

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
        CPUE =  CPUE + (dataWithTheSpecies$HLNoAtLngt[i]*60/dataWithTheSpecies$HaulDur[i])*subfactor[i] * ALK[lineInAlkToUse,-1]/sum(ALK[lineInAlkToUse,-1])
      }else if(dataWithTheSpecies$DataType[i]=="C")
      {
        CPUE  =  CPUE + dataWithTheSpecies$HLNoAtLngt[i]*subfactor[i] * ALK[lineInAlkToUse,-1]/sum(ALK[lineInAlkToUse,-1])
      }

      if ("foundWithin" %in% names(attributes(ALK))){
        if(!is.na(lineInAlkToUse)){
          if(dataWithTheSpecies$DataType[i]=="R")
          {
            if(attributes(ALK)$foundWithin[lineInAlkToUse]){
              nFoundWithin = nFoundWithin + (dataWithTheSpecies$HLNoAtLngt[i])*subfactor[i]
            }else{
              nNotFoundWithin = nNotFoundWithin + (dataWithTheSpecies$HLNoAtLngt[i])*subfactor[i]
            }
          }else if(dataWithTheSpecies$DataType[i]=="C")
          {
            if(attributes(ALK)$foundWithin[lineInAlkToUse]){
              nFoundWithin = nFoundWithin + dataWithTheSpecies$HLNoAtLngt[i]*subfactor[i]*dataWithTheSpecies$HaulDur[i]/60
            }else{
              nNotFoundWithin = nNotFoundWithin + dataWithTheSpecies$HLNoAtLngt[i]*subfactor[i]*dataWithTheSpecies$HaulDur[i]/60
            }
          }
        }
      }
    }
  }
  mCPUE = CPUE/nHauls

  attributes(mCPUE)$nFoundWithin = nFoundWithin
  attributes(mCPUE)$nNotFoundWithin = nNotFoundWithin

  return(mCPUE)
  #----------------------------------
}




#' calcmCPUErfaHaulbasedALK
#' @description Calculates CPUE per length class in a given roundfish area with  a new procedure TODO:document.
#' @param RFA Roundfish area number.
#' @param species The species of interest.
#' @param year The year of interest.
#' @param quarter The quarter of interest.
#' @param data the datras-data needed.
#' @param ALK A list with the ALKs. Id to trawl haul is given in the first coumn of each element in the list.
#' @export
#' @return
#' @examples
calcmCPUErfaHaulbasedALK = function(RFA,species,year, quarter, data, ALK,procedure = "", weightStatRec = NULL, useICESindexArea = FALSE)
{

  #Extract the data of interest-------------------------
  dataOfInterest = data[!is.na(data$Year) & data$Year == year&
                          !is.na(data$Quarter) & data$Quarter == quarter&
                          !is.na(data$Roundfish) & data$Roundfish == RFA ,]
  #-----------------------------------------------------

  #Help variable for invewstigating how larg proportion og ages is calculated with the DATRAS procedure
  nWithDatras = 0
  nWithoutDatras = 0
  nFoundWithin = 0
  nNotFoundWithin = 0

  #Investigate if it is observed zero data---
  tmp = dataOfInterest[!is.na(dataOfInterest$Roundfish) & dataOfInterest$Roundfish == RFA&
                         !is.na(dataOfInterest$Species) & dataOfInterest$Species == species,]
  if(sum(!is.na(tmp))==0){
    nAgeClasses = dim(ALK[[1]])[2]-2
    mCPUE = rep(0,nAgeClasses)
    return(mCPUE)
  }
  #------------------------------------------

  #Construct a matrix with mCPUEs for each statistical rectangel---
  statRects = unique(dataOfInterest$StatRec)
  if(useICESindexArea){
    #Exclude some of the statistical rectanlges not included in the index area.
    statRecsOfinterest = extractStatRecIndexArea(species)
    statRects = statRects[which(statRects %in% statRecsOfinterest)]
  }
  numberOfStatRectangles = length(statRects)
  nAgeClasses = dim(ALK[[1]])[2]-2
  mCPUEstatRec = matrix(NA,nAgeClasses,numberOfStatRectangles)
  #---------------------------------------------------------------

  #Calculate the mCPUEs for each statistical rectangangle---------
  if(numberOfStatRectangles==0) return("No observations in RFA")
  for(i in 1:numberOfStatRectangles)
  {
    cpueStatRec = calcmCPUEStatRecHaulBasedALK(statRec = statRects[i],species = species,year= year , quarter = quarter, data = dataOfInterest,ALK = ALK,procedure = procedure)

    mCPUEstatRec[,i] = as.double(cpueStatRec)

    if(!is.null(attributes(cpueStatRec)$nWithDatras) & !is.null(attributes(cpueStatRec)$nWithoutDatras)){
      nWithDatras = nWithDatras + attributes(cpueStatRec)$nWithDatras
      nWithoutDatras = nWithoutDatras + attributes(cpueStatRec)$nWithoutDatras
    }
    if(!is.null(attributes(cpueStatRec)$nFoundWithin) & !is.null(attributes(cpueStatRec)$nNotFoundWithin)){
      nFoundWithin = nFoundWithin + attributes(cpueStatRec)$nFoundWithin
      nNotFoundWithin = nNotFoundWithin + attributes(cpueStatRec)$nNotFoundWithin
    }

  }
  #---------------------------------------------------------------

  #Average over the statistical recangles and return mCPUE-----
  weightUsed = rep(0,numberOfStatRectangles)
  for(i in 1:numberOfStatRectangles){
    if(species =="Pollachius virens"){
      if(statRects[i] %in% weightStatRec$StatRec){
        weightUsed[i] =  weightStatRec$Weight[which(weightStatRec$StatRec== statRects[i])]
      }else{
        weightUsed[i] =  0 #Haul is in RFA 10 which is not included in the index for Saithe, this should have been included in the weighting file
      }
    }else{
      weightUsed[i] = 1
      }
  }

  mCPUE = rep(0,nAgeClasses)
  for(i in 1:nAgeClasses)
  {
    for(j in 1:numberOfStatRectangles){
      mCPUE[i] = mCPUE[i] + mCPUEstatRec[i,j] *weightUsed[j]/sum(weightUsed)
    }
  }

  attributes(mCPUE)$nWithDatras = nWithDatras
  attributes(mCPUE)$nWithoutDatras = nWithoutDatras

  attributes(mCPUE)$nFoundWithin = nFoundWithin
  attributes(mCPUE)$nNotFoundWithin = nNotFoundWithin
  return(mCPUE)
  #------------------------------------------------------------
}

#' calcmCPUEStatRecHaulBasedALK
#' @description Calculates CPUE per length class in a given statistical rectangle with the new procedure. TODO: document.
#' @param statRec Statistical area.
#' @param species The species of interest.
#' @param year The year of interest.
#' @param quarter The quarter of interest.
#' @param data the datras-data needed.
#' @param ALK List with the ALKs for each trawl haul. First element of each ALK in the list is a vector with the first element eual the haulId. This haulId is used when finding which of the ALK which are used.
#' @export
#' @return Returns the mCPUE per length class in the given statistical rectangle
#' @examples
calcmCPUEStatRecHaulBasedALK = function(statRec,species,year, quarter, data, ALK,procedure = "")
{
  #Extract the number of hauls in the statistical area
  nHauls = length(unique(data$haul.id[which(data$StatRec == statRec)]))


  #Extract the data of interest-------------------------
  dataWithTheSpecies = data[!is.na(data$Species) & data$Species==species &
                              !is.na(data$Year) & data$Year == year&
                              !is.na(data$Quarter) & data$Quarter == quarter&
                              !is.na(data$StatRec) & data$StatRec == statRec ,]
  #-----------------------------------------------------

  #Help variable for invewstigating how larg proportion og ages is calculated with the DATRAS procedure
  nWithDatras = 0
  nWithoutDatras = 0
  nFoundWithin = 0
  nNotFoundWithin = 0

  #Calculates and returns mCPUE-----
  subfactor = dataWithTheSpecies$SubFactor

  nAgeClasses = dim(ALK[[1]])[2]-2
  CPUE = rep(0,nAgeClasses)

  if(dim(dataWithTheSpecies)[1]==0)return(CPUE)

  if(dim(dataWithTheSpecies)[1]>0)
  {
    for(i in 1:dim(dataWithTheSpecies)[1])
    {
      trawlId = dataWithTheSpecies$haul.id[i]
      whichALK =NA
      for(indeksALK in 1:length(ALK))
      {
        if(ALK[[indeksALK]]$ID[1]==trawlId)whichALK = indeksALK
      }
      ALKtoUse = ALK[[whichALK]]

      lineInAlkToUse = NA
      if(min(ALKtoUse$Length)>dataWithTheSpecies$LngtCm[i])
      {
        lineInAlkToUse = 1
      }else if(max(ALKtoUse$Length)<dataWithTheSpecies$LngtCm[i])
      {
        lineInAlkToUse = length(ALKtoUse$Length)
      }else{
        lineInAlkToUse = max(which(ALKtoUse$Length <= dataWithTheSpecies$LngtCm[i]))
      }
      if(dataWithTheSpecies$DataType[i]=="R")
      {
        CPUE =  CPUE + (dataWithTheSpecies$HLNoAtLngt[i]*60/dataWithTheSpecies$HaulDur[i])*subfactor[i] * ALKtoUse[lineInAlkToUse,-c(1,2)]/sum(ALKtoUse[lineInAlkToUse,-c(1,2)])
        if(sum(ALKtoUse[lineInAlkToUse,-c(1,2)]) ==0)print("ALK er null")
      }else if(dataWithTheSpecies$DataType[i]=="C")
      {
        CPUE  =  CPUE + dataWithTheSpecies$HLNoAtLngt[i]*subfactor[i] * ALKtoUse[lineInAlkToUse,-c(1,2)]/sum(ALKtoUse[lineInAlkToUse,-c(1,2)])
        if(sum(ALKtoUse[lineInAlkToUse,-c(1,2)]) ==0)print("ALK er null")
      }
      #Store how many was calculated with datras procedure
      if ("datrasValue" %in% names(attributes(ALKtoUse))){
        if(!is.na(lineInAlkToUse)){
          if(dataWithTheSpecies$DataType[i]=="R")
          {
            if(attributes(ALKtoUse)$datrasValue[lineInAlkToUse]){
              nWithDatras = nWithDatras + (dataWithTheSpecies$HLNoAtLngt[i])*subfactor[i]
            }else{
              nWithoutDatras = nWithoutDatras + (dataWithTheSpecies$HLNoAtLngt[i])*subfactor[i]
            }
          }else if(dataWithTheSpecies$DataType[i]=="C")
          {
            if(attributes(ALKtoUse)$datrasValue[lineInAlkToUse]){
              nWithDatras = nWithDatras + dataWithTheSpecies$HLNoAtLngt[i]*subfactor[i]*dataWithTheSpecies$HaulDur[i]/60
            }else{
              nWithoutDatras = nWithoutDatras + dataWithTheSpecies$HLNoAtLngt[i]*subfactor[i]*dataWithTheSpecies$HaulDur[i]/60
            }
          }
        }
      }
      if ("foundWithin" %in% names(attributes(ALKtoUse))){
        if(!is.na(lineInAlkToUse)){
          if(dataWithTheSpecies$DataType[i]=="R")
          {
            if(attributes(ALKtoUse)$foundWithin[lineInAlkToUse]){
              nFoundWithin = nFoundWithin + (dataWithTheSpecies$HLNoAtLngt[i])*subfactor[i]
            }else{
              nNotFoundWithin = nNotFoundWithin + (dataWithTheSpecies$HLNoAtLngt[i])*subfactor[i]
            }
          }else if(dataWithTheSpecies$DataType[i]=="C")
          {
            if(attributes(ALKtoUse)$foundWithin[lineInAlkToUse]){
              nFoundWithin = nFoundWithin + dataWithTheSpecies$HLNoAtLngt[i]*subfactor[i]*dataWithTheSpecies$HaulDur[i]/60
            }else{
              nNotFoundWithin = nNotFoundWithin + dataWithTheSpecies$HLNoAtLngt[i]*subfactor[i]*dataWithTheSpecies$HaulDur[i]/60
            }
          }
        }
      }
    }
  }
  mCPUE = CPUE/nHauls
  attributes(mCPUE)$nWithDatras = nWithDatras
  attributes(mCPUE)$nWithoutDatras = nWithoutDatras
  attributes(mCPUE)$nFoundWithin = nFoundWithin
  attributes(mCPUE)$nNotFoundWithin = nNotFoundWithin

  return(mCPUE)
  #----------------------------------
}


#' calcmCPUEnorthSea
#' @description Internal function for calculating mCPUE in whole North sea.
#' @param rfa information about size of RFA's.
#' @param species The species of interest.
#' @param year The year of interest.
#' @param quarter The quarter of interest.
#' @param data the datras-data needed.
#' @param column column in the mCPUE matrix to calculate mCPUE
#' @export
#' @return Returns the mCPUE per length class in the given statistical rectangle
#' @examples
calcmCPUEnorthSea = function(species,year, quarter, dat,ALKprocedure,B,dimCPUE, lengthDivision, useICESindexArea = FALSE)
{
  #Help variable for invewstigating how larg proportion og ages is calculated with the DATRAS procedure
  nWithDatras = 0
  nWithoutDatras = 0
  nFoundWithin = 0
  nNotFoundWithin = 0

  #Read shape file for roundfish areas and calcualte area---------
  rfa <-
    readOGR(file.path(
      system.file("shapefiles", package = "IBTSindices"),
      "Roundfish_shapefiles"
    ),verbose = FALSE)
  rfa$areas.sqm<-areaPolygon(rfa)
  rfa$areas.sqkm<-rfa$areas.sqm/(1000*1000)
  #---------------------------------------------------------------

  #Read the calcualted area were saithe live in each RFA (between 10 to 200 meters)---
  data('areaRFA') #Stored in the data frame "areaRFA"
  #---------------------------------------------------------------

  mCPUEvector = rep(0,dimCPUE[1])

  totalArea = 0
  for(RFA in 1:9){
    areaThisRFA = rfa@data$areas.sqkm[which( as.numeric(as.character(rfa@data$AreaName)) == RFA)]

    if(RFA ==5){#WARNING! By some reason the rfa 5 and 10 are merged in the datras data
      areaThisRFA = areaThisRFA + rfa@data$areas.sqkm[which( as.numeric(as.character(rfa@data$AreaName)) == 10)]
    }

    if(useICESindexArea){ #Use index areas defined by statistical rectangles instead
      allRectanlges = extractStatRecAll()[[RFA]]
      areaThisRFA = length(allRectanlges)
      rectanglesOfInterest = extractStatRecIndexArea(species)
      proportionOfInterest = mean(allRectanlges %in% rectanglesOfInterest)
      areaThisRFA = areaThisRFA *proportionOfInterest
    }

#    if(species== "Pollachius virens"){ #The areas for saithe needs furhter work
#      areaThisRFA = areaRFA$areaSaithe[RFA] #Extrat the area with depth between 10 to 200 meters in the RFA
#    }

    if(sum(!is.na(dat$hl_hh$Roundfish) & dat$hl_hh$Roundfish==RFA
           & !is.na(dat$hl_hh$Species)& dat$hl_hh$Species ==species & areaThisRFA >0)>0 ){
      #Add the mCPUE from this RFA to mCPUEvector and scale with the area. Note we later divide by the total area.
      cpueThisRFA = CPUErfa(RFA = RFA, species = species, year = year, quarter = quarter,dat = dat,
                            ALKprocedure = ALKprocedure, B = n,lengthDivision = lengthDivision,useICESindexArea = useICESindexArea)


       mCPUEvector = mCPUEvector + cpueThisRFA[,1] *areaThisRFA

      #Collect how many ages that was calculated with different procedures.
      if(!is.null(attributes(cpueThisRFA)$nWithDatras) & !is.null(attributes(cpueThisRFA)$nWithoutDatras)){
        nWithDatras = nWithDatras + attributes(cpueThisRFA)$nWithDatras
        nWithoutDatras = nWithoutDatras + attributes(cpueThisRFA)$nWithoutDatras
      }
      if(!is.null(attributes(cpueThisRFA)$nFoundWithin) & !is.null(attributes(cpueThisRFA)$nNotFoundWithin)){
        nFoundWithin = nFoundWithin + attributes(cpueThisRFA)$nFoundWithin
        nNotFoundWithin = nNotFoundWithin + attributes(cpueThisRFA)$nNotFoundWithin
      }
    }
    totalArea = totalArea + areaThisRFA
  }
  mCPUEvector = mCPUEvector/totalArea

  print("Done with one simulation whole North Sea, mCPUE is:")
  print(mCPUEvector[1:length(mCPUEvector)])

  attributes(mCPUEvector)$nFoundWithin = nFoundWithin
  attributes(mCPUEvector)$nNotFoundWithin = nNotFoundWithin

  attributes(mCPUEvector)$nWithDatras = nWithDatras
  attributes(mCPUEvector)$nWithoutDatras = nWithoutDatras

  return(list(mCPUEvector))
}
