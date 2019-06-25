#' calculateALK
#' @description Calculates the ALK-key-matrix for a given year, quarter, species and RFA.
#' @param RFA Roundfish area number.
#' @param species The species of interest.
#' @param year The year which the ALK is calculated.
#' @param quarter The quarter of the year which the ALK is calculated.
#' @param ca The ca needed for calculating the ALK.
#' @param lengthDivision A vector giving the breakpoints in the length division.
#' @param dat All data, this is needed when filling the ALK using different RFAs. If dat==null, then ALK is not borrowed from other RFAs.
#' @export
#' @return Returns a matrix with the ALK for the given data, species, time and RFA
#' @examples
calculateALKDatras = function(RFA,species,year,quarter,ca,lengthDivision,dat = NULL)
{
    dimALK = max(lengthDivision)

    #Extract the data of interest----------------------
    caInterest = ca[which(!is.na(ca$Roundfish) & ca$Roundfish==RFA &
                            !is.na(ca$Year) & ca$Year==year &
                            !is.na(ca$Quarter) & ca$Quarter == quarter &
                            ca$Species == species),]

    caInterest = caInterest[which(!is.na(caInterest$Age) & !is.na(caInterest$LngtCm)),]
    #---------------------------------------------------

    #Find the configurations needed for the ALK---------
    conf = confALK(species = species,quarter = quarter)
    maxAge = conf$maxAge
    minLength = conf$minLength
    maxLength = conf$maxLength
    lengthClassIntervallLengths = conf$lengthClassIntervallLengths
    #----------------------------------------------------

    #Create the sceleton of the ALK----------------------
    alk = matrix(0,dimALK, maxAge+2)
    alk[,1] = seq(1,dimALK,by = 1)
    alk = as.data.frame(alk)
    names(alk) = c("length", c(0:maxAge))
    #----------------------------------------------------

    #Investigate if zero data, if so return the sceleton-
    if(dim(caInterest)[1]==0){
      warning(paste("No observations in period in RFA: " ,RFA,sep = ""))
      alk[,2:(maxAge+2)] = 0
      alk = as.data.frame(alk)
      names(alk) = c("length", c(0:maxAge))
      if(!is.null(dat)){
        alk = borrowALKfromNeighbourRFAs(RFA = RFA, species = species, year = year, quarter = quarter,dat = dat,ALK = alk, lengthDivision = lengthDivision)
      }
      attributes(alk)$foundWithin = rep(FALSE, dim(alk)[1])
      return(alk)
    }
    #----------------------------------------------------


    #Truncate all old fish to the pluss group------------
    caInterest$Age[caInterest$Age > maxAge] = maxAge
    #----------------------------------------------------

    #Construct the parts of the ALK were we have data-----
    for(i in 1:dim(caInterest)[1])
    {
        alk[which(alk[,1]==floor(caInterest$LngtCm[i])),floor(caInterest$Age[i])+2] =
          alk[which(alk[,1]==floor(caInterest$LngtCm[i])),floor(caInterest$Age[i])+2] +caInterest$NoAtALK[i]
    }
    #------------------------------------------------------

    #Use same resultion for the ALK as for the sampling---
    for(i in 1:(length(lengthDivision)-1)){
     for(j in 2:(dim(alk)[2])){
       alk[(lengthDivision[i]):(lengthDivision[i+1]-1),j] = sum(alk[(lengthDivision[i]):(lengthDivision[i+1]-1),j])
     }
    }
    #-----------------------------------------------------

    #Extrapolate the ALK to length calsses were we do not have data-----------------------------------
    whichIsMissing = rep(FALSE, dim(alk)[1])
    for(i in 1:dim(alk)[1])
    {
      if(sum(alk[i,-1]) == 0)whichIsMissing[i] = TRUE
    }

    #Assign attribut which say if age is found within haul and length group
    foundWithin = rep(TRUE, dim(alk)[1])
    foundWithin[whichIsMissing] = FALSE



    #Routine for filling the not observed length classes by looking at length groups close by
    if(quarter ==1)start = 3
    if(quarter >1)start = 2

    for(j in start:dim(alk)[2])
    {
      distToPrevious = 999999
      distToNext = which(!whichIsMissing)[1]  -minLength-1
      nextValue =alk[minLength+ distToNext+1,j]
      for(i in (minLength+1):(maxLength-1))
      {
        if(whichIsMissing[i])
        {
          if(distToPrevious<distToNext)
          {
            alk[i,j]= alk[i-1,j]
          }else if(distToPrevious == distToNext)
          {
            alk[i,j]= (alk[i-1,j] + nextValue)/2
          }else if(distToPrevious > distToNext)
          {
            alk[i,j]= nextValue
          }
          distToNext  = distToNext -1
          distToPrevious =distToPrevious +1

        }else{
          distToPrevious = 1
          distToNext = which(!whichIsMissing[i:length(whichIsMissing)])[2]-2
          if(is.na(distToNext))
            {
            distToNext = 999999
            nextValue = -999999
          }else{
            nextValue = alk[i + distToNext + 1,j]
          }
        }
      }
    }
    #--------------------------------------------------------------------------------------------

    #Fill in ages above max lengh, and below min length
    for(i in 1:minLength){
      if(whichIsMissing[i]){
        if(quarter==1)
        {
          alk[i,3] = 1
        }else if(quarter>1)
        {
          alk[i,2] = 1
        }
        whichIsMissing[i] = FALSE
      }
    }
    for(i in maxLength:dimALK){
      if(whichIsMissing[i]){
        alk[i,dim(alk)[2]] = 1
        whichIsMissing[i] = FALSE
      }
    }

    attributes(alk)$foundWithin = foundWithin
    return(alk)
}


#' calculateALKNew
#' @description
#' @param RFA Roundfish area number.
#' @param species The species of interest.
#' @param year The year which the ALKs are calculated.
#' @param quarter The quarter of the year which the ALKs are calculated.
#' @param ca The CA needed for calculating the ALKs.
#' @param hl The HL needed for calculating the ALKs (since there can be trawl hauls without age information).
#' @param dat All data, this is needed when filling the ALK using different RFAs. If dat==null, then ALK is not borrowed from other RFAs.
#' @export
#' @return Returns a list with ALK for each trawl haul
#' @examples
calculateALKHaulbased = function(RFA, species, year, quarter,ca,hl,lengthDivision, dat = NULL){

  dimALK = max(lengthDivision)
  #Define the list which shall be filled with the ALKs and returned-----
  alkToReturn = list()
  #----------------------------------------------------

  #Extract the data of interest----------------------
  caInterest = ca[which(ca$Roundfish==RFA & ca$Year==year &
                          ca$Quarter == quarter & ca$Species == species),]

  caInterest = caInterest[which(!is.na(caInterest$Age) & !is.na(caInterest$LngtCm)),]

  hlInterest = hl[!is.na(hl$Year) & hl$Year == year&
                         !is.na(hl$Quarter) & hl$Quarter == quarter&
                         !is.na(hl$Roundfish) & hl$Roundfish == RFA ,]

  hlInterest = hlInterest[which(!is.na(hlInterest$LngtCm)),]
  hlInterest = hlInterest[which(!is.na(hlInterest$Species)),]
  #---------------------------------------------------


  #Find the configurations needed for the ALK---------
  conf = confALK(species = species,quarter = quarter)
  maxAge = conf$maxAge
  minLength = conf$minLength
  maxLength = conf$maxLength
  lengthClassIntervallLengths = conf$lengthClassIntervallLengths #Needed for species with lenghtgroups more detiled than 1cm, include TODO
  #----------------------------------------------------

  #Create the sceleton of the ALK----------------------
  alk = matrix(0,dimALK, maxAge+3)
  alk[,2] = seq(1,dimALK,by = 1)
  #----------------------------------------------------

  #Investigate if zero data, if so return the sceleton with equal proportions for all ages-
  if(dim(caInterest)[1]==0){
    idHaul = unique(c(as.character(hlInterest$haul.id),as.character(caInterest$haul.id)))
    neste=1
    alkFictive = alk
    alkFictive[,3:(maxAge+3)] = 0
    ALKborrow = borrowALKfromNeighbourRFAs(RFA= RFA, species= species, year = year, quarter= quarter, dat = dat,ALK = as.data.frame(alkFictive[,-1]), lengthDivision = lengthDivision)

    for(id in idHaul){
      idTmp = as.character(id)
      alkThis = as.data.frame(alkFictive)
      names(alkThis) = c("ID","Length",0:maxAge)
      alkThis$ID[1] = idTmp
      alkThis[,3:dim(alkThis)[2]] = ALKborrow[2:dim(ALKborrow)[2]]
      alkToReturn[[neste]] = alkThis
      neste = neste+1
    }
    warning(paste("No observations in period in RFA: " ,RFA,sep = ""))

    return(alkToReturn)
  }
  #----------------------------------------------------

  #Find distance between trawl locations--------------
  id1 = as.character(caInterest$haul.id)
  id2 = as.character(hlInterest$haul.id)
  uniqueId = unique(c(id1,id2)) #Need ALK for every trawl haul. TODO: should use hh-data instead.
  loc = data.frame(uniqueId)
  loc$lat = rep(-999,dim(loc)[1])
  loc$lon = rep(-999,dim(loc)[1])

  for(i in 1:length(uniqueId))
  {
    id = uniqueId[i]
    indeks = which(caInterest$haul.id== id)[1]
    if(!is.na(indeks)){
      loc$lat[i] = caInterest$lat[indeks]
      loc$lon[i] = caInterest$lon[indeks]
    }else{
      indeks = which(hlInterest$haul.id== id)[1]
      loc$lat[i] = hlInterest$lat[indeks]
      loc$lon[i] = hlInterest$lon[indeks]
    }

  }
  coordinates(loc) <- ~lon+lat
  proj4string(loc) ="+proj=longlat"
  d = spDists(loc)
  #-----------------------------------------------------

  #Set old fish to belong to the pluss group-----------
  caInterest$Age[caInterest$Age > maxAge] = maxAge
  #----------------------------------------------------

  #Construct each element of the ALK-list----------------------------------------------------------------------
  haulId = uniqueId
  neste = 1

  #Construct the DATRAS ALK, which is used if there are no trawl haul close by.
  ALKnormal = calculateALKDatras(RFA = RFA, species = species, year = year, quarter = quarter,ca = caInterest,dat = dat,lengthDivision = lengthDivision)

  for(id in haulId){
    #Extract which lengts that are of interest (i.e. observed in either HL-data or CA-data in this trawl),
    whichLengtsAreInteresting = unique(c(hlInterest$LngtCm[hlInterest$haul.id==id & hlInterest$Species==species],
                                         caInterest$LngtCm[caInterest$haul.id==id & caInterest$Species==species]))

    whichLengtsAreInteresting = unique(floor(whichLengtsAreInteresting)) #Reduce time consumption by only calculating ALK for those lengths of interest
    whichIsMissing = rep(TRUE, dim(alk)[1])
    #---------------------------------------------------------------------------------------------------

    #Construct the parts of the ALK were we have data--------------------
    idTmp = as.character(id)
    alkThis = as.data.frame(alk)
    names(alkThis) = c("ID","Length",0:maxAge)
    alkThis$ID[1] = idTmp

    dataThisTrawl = caInterest[caInterest$haul.id == id,]

    if(dim(dataThisTrawl)[1]>0){
      for(i in 1:dim(dataThisTrawl)[1])
      {
          hvilke = min(which(alkThis[,2]> floor(dataThisTrawl$LngtCm[i]-1)))
          alkThis[hvilke,floor(dataThisTrawl$Age[i])+3] =
            alkThis[hvilke,floor(dataThisTrawl$Age[i])+3] +dataThisTrawl$NoAtALK[i]
          whichIsMissing[hvilke] = FALSE
      }
    }

    #Use same resultion for the ALK as for the sampling
    for(i in 1:(length(lengthDivision)-1)){
        for(j in 3:dim(alkThis)[2]){
          alkThis[(lengthDivision[i]):(lengthDivision[i+1]-1),j] = sum(alkThis[(lengthDivision[i]):(lengthDivision[i+1]-1),j])
        }
    }

    for(i in 1:dim(alkThis)[1]){
      if(sum(alkThis[i,3:dim(alkThis)[2]])>0){
        whichIsMissing[i] = FALSE
      }
    }

    #Assign attribut which say if age is found within haul and length group
    foundWithin = rep(TRUE, dim(ALKnormal)[1])
    foundWithin[whichIsMissing] = FALSE
    attributes(alkThis)$foundWithin = foundWithin


    if(length(whichLengtsAreInteresting)==0){
      whichIsMissing = rep(FALSE, dim(alkThis)[1])
    }else{
      tmpIndeks = 1:dim(alkThis)[1]
      tmpIndeks = tmpIndeks[-(whichLengtsAreInteresting)]
      whichIsMissing[tmpIndeks] = FALSE
    }
    #------------------------------------------------------

    #Extrapolate the ALK to length calsses were we do not have data by looking at hauls close by-----------------------------------
    if(sum(whichIsMissing)>0){
      #Routine for filling the not observed length classes
      for(i in 1:dim(alkThis)[1])
      {
        if(whichIsMissing[i])
        {
          sortedShortestDist = order(d[,which(loc$uniqueId== id)[1]])[-1]

          closestSorted = haulId[sortedShortestDist]
          foundAge = FALSE
          nesteHal = 1
          maxDistToBorrowStrength = 60*1.852; #We search this far away from the trawl to look at same length in other hauls
          closesId = closestSorted[nesteHal]
          while(!foundAge){
            distInKm = d[sortedShortestDist[nesteHal],which(loc$uniqueId== id)[1]]
            if(is.na(distInKm)) {
              distInKm = 9999 #No hauls in the same RFA
            }
            if(distInKm<maxDistToBorrowStrength){
              closestData = caInterest[caInterest$haul.id == closesId,]
              if(i==dim(alkThis)[1]){
                hvilke = which(closestData$LngtCm >= alkThis[i,2]) #Extact which fish to extract age from in haul close by
              }else if(i==1){
                hvilke = which(closestData$LngtCm < alkThis[2,2])
              }else if(i< dim(alkThis)[1]){
                hvilke = which(closestData$LngtCm >= alkThis[i,2] & closestData$LngtCm < alkThis[i+1,2] )
              }

              if(length(hvilke)>0)
              {
                row = rep(0,maxAge+1)
                for(l in hvilke)
                {
                  row[closestData$Age[l]+1] = row[closestData$Age[l]+1] +closestData$NoAtALK[l]
                }
                alkThis[i,3:dim(alkThis)[2]] = row
                foundAge = TRUE
                whichIsMissing[i] = FALSE
              }else{
                nesteHal = nesteHal+1
                if(nesteHal>length(closestSorted)){#We have now looked in all hauls in the RFA
                  foundAge = TRUE#The age will be filled with datars-procedure
                  nesteHal = 1
                }
                closesId = closestSorted[nesteHal]
                #Did not information in this trawl haul, go to next trawl haul.
              }
            }else{
              foundAge = TRUE#The age will be filled with datars-procedure
              nesteHal = 1
            }
          }
        }
      }
    #------------------------------------------------------
    }

    if(sum(whichIsMissing)>0){
      #Extrapolate the ALK to length calsses were we do not have data by looking at similar length in same haul-----------------------------------
      #Trick if we want to look more than one cm away, set every not observed to TRUE and run this procedure two times.
      whichIsMissingTmp = whichIsMissing
      alkThisTMP = alkThis
      if(quarter ==1)start = 4
      if(quarter >1)start = 3

      for(j in start:dim(alkThis)[2])
      {
        for(i in 1:dim(alkThis)[1])
        {
          if(whichIsMissing[i])
          {
            if(i>1){
              obsInPrevious = (sum(alkThis[i-1,start:dim(alkThis)[2]])>0) #Check if we have an observation in the closest length class
            }else{
              obsInPrevious = FALSE
            }
            if(i<dim(alkThis)[1]){
              obsInNext = (sum(alkThis[i+1,start:dim(alkThis)[2]])>0)
            }else{
              obsInNext = FALSE
            }
            if(obsInPrevious& obsInNext)
            {
              alkThisTMP[i,j]= (alkThisTMP[i-1,j] + alkThisTMP[i+1,j])/2
              whichIsMissingTmp[i] = FALSE
            }else if(obsInPrevious)
            {
              alkThisTMP[i,j]= alkThisTMP[i-1,j]
              whichIsMissingTmp[i] = FALSE
            }else if(obsInNext)
            {
              alkThisTMP[i,j]= alkThisTMP[i+1,j]
              whichIsMissingTmp[i] = FALSE
            }
          }
        }
      }
      whichIsMissing = whichIsMissingTmp #Inform which we have filled in
      alkThis = alkThisTMP
      #--------------------------------------------------------------------------------------------
    }



    #Set those we do not find any age equal the area based ALK
    alkThis[whichIsMissing,2:(maxAge+3)] = ALKnormal[whichIsMissing,]
    if(sum(whichIsMissing)>0)    attributes(alkThis)$MissAgeObservation = TRUE



    #Assign attribut which say if the DATRAS-procedure is used
    datrasValue = rep(FALSE, dim(ALKnormal)[1])
    datrasValue[whichIsMissing] = TRUE
    attributes(alkThis)$datrasValue = datrasValue

    #Store the ALK for this trawl haul in the list to be returned
    alkToReturn[[neste]] = alkThis
    neste = neste+1
  }

  #------------------------------------------------------------------------------------------------------------------------

  #Return the list with ALKs------
  return(alkToReturn)
  #-------------------------------
}

#' calculateALKModel
#' @description
#' @param RFA Roundfish area number.
#' @param species The species of interest.
#' @param year The year which the ALKs are calculated.
#' @param quarter The quarter of the year which the ALKs are calculated.
#' @param ca Data with age information
#' @export
#' @return Returns a list with ALK for each trawl haul
#' @examples
borrowALKfromNeighbourRFAs = function(RFA, species, year, quarter, dat,ALK, lengthDivision){

  neighborRFA = list()
  neighborRFA[[1]] = c(2,3)
  neighborRFA[[2]] = c(1,3,4,6,7)
  neighborRFA[[3]] = c(1,2,4)
  neighborRFA[[4]] = c(2,3,5,6)
  neighborRFA[[5]] = c(4,6,10)
  neighborRFA[[6]] = c(2,4,5,7)
  neighborRFA[[7]] = c(2,6,8)
  neighborRFA[[8]] = c(7,9)
  neighborRFA[[9]] = c(8)
  neighborRFA[[10]] = c(5)



  ALK[,2:dim(ALK)[[2]]] = 0
  for(i in 1:length(neighborRFA[[RFA]])){
    ALKtmp =  calculateALKDatras(RFA = neighborRFA[[RFA]][i], species = species, year = year, quarter = quarter,ca = dat$ca_hh,lengthDivision = lengthDivision,dat =  NULL) #Note that dat is null, if not the function retrun the borrowed ALK
    ALK[,2:dim(ALK)[[2]]] = ALK[,2:dim(ALK)[[2]]] + ALKtmp[,2:dim(ALK)[[2]]]
  }

  #Check if the neighbors have age reading, if not use the whole North Sea
  if(sum(ALK[,2:dim(ALK)[[2]]])==0){
    for(RFAall in 1:10){
      for(i in 1:length(neighborRFA[[RFAall]])){
        ALKtmp =  calculateALKDatras(RFA = neighborRFA[[RFAall]][i], species = species, year = year, quarter = quarter,ca = dat$ca_hh,lengthDivision = lengthDivision, dat = NULL)
        ALK[,2:dim(ALK)[[2]]] =ALK[,2:dim(ALK)[[2]]] + ALKtmp[,2:dim(ALK)[[2]]]
      }
    }
  }

  return(ALK)
}

