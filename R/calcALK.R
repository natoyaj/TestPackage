#' calculateALK
#' @description Calculates the ALK-key-matrix for a given year, quarter, species and RFA.
#' @param RFA Roundfish area number.
#' @param species The species of interest.
#' @param year The year which the ALK is calculated.
#' @param quarter The quarter of the year which the ALK is calculated.
#' @param data The data needed for calculating the ALK.
#' @export
#' @return Returns a matrix with the ALK for the given data, species, time and RFA
#' @examples
calculateALKDatras = function(RFA,species,year,quarter,data)
{
    #Extract the data of interest----------------------
    caInterest = data[which(data$Roundfish==RFA & data$Year==year &
                              data$Quarter == quarter & data$Species == species),]

    caInterest = caInterest[which(!is.na(caInterest$Age) & !is.na(caInterest$LngtCm)),]
    #---------------------------------------------------


    #Define variables used in the construction of the ALK--
    maxAge = NULL
    minLength = NULL
    maxLength = NULL
    lengthClassIntervallLengths = NULL
    #----------------------------------------------------

    #Find the configurations needed for the ALK---------
    conf = confALK(species = species,quarter = quarter)
    maxAge = conf$maxAge
    minLength = conf$minLength
    maxLength = conf$maxLength
    lengthClassIntervallLengths = conf$lengthClassIntervallLengths
    #----------------------------------------------------

    #Create the sceleton of the ALK----------------------
    alk = matrix(0,(maxLength-minLength + 1)/lengthClassIntervallLengths, maxAge+2)
    alk[,1] = seq(minLength,maxLength,by = lengthClassIntervallLengths)
    #----------------------------------------------------

    #Investigate if zero data, if so return the sceleton-
    if(dim(caInterest)[1]==0){
      warning(paste("No observations in period in RFA: " ,RFA,sep = ""))
      return(alk)
    }
    #----------------------------------------------------

    #Truncate all old fish to the pluss group------------
    caInterest$Age[caInterest$Age > maxAge] = maxAge
    #----------------------------------------------------

    #Construct the parts of the ALK were we have data-----
    if(species=="Gadus morhua"|species == "Pollachius virens")
    {
      for(i in 1:dim(caInterest)[1])
      {
        if(floor(caInterest$LngtCm[i])< minLength)
        {
          alk[1,floor(caInterest$Age[i])+2] =
          alk[1,floor(caInterest$Age[i])+2] +caInterest$NoAtALK[i] #!!! SEEMS THAT IT CAN BE MORE THAN ONE FISH PER ROW!!! THIS IS NOT REPORTED ANYWHERE AS I OLAV SEE
        }else if(floor(caInterest$LngtCm[i])> maxLength)
        {
          alk[dim(alk)[1],floor(caInterest$Age[i])+2] =
          alk[dim(alk)[1],floor(caInterest$Age[i])+2] +caInterest$NoAtALK[i]
        }else{
          alk[which(alk[,1]==floor(caInterest$LngtCm[i])),floor(caInterest$Age[i])+2] =
          alk[which(alk[,1]==floor(caInterest$LngtCm[i])),floor(caInterest$Age[i])+2] +caInterest$NoAtALK[i]
        }
      }
    }
    #------------------------------------------------------

    #Extrapolate the ALK to length calsses were we do not have data-----------------------------------
    whichIsMissing = rep(FALSE, dim(alk)[1])
    for(i in 1:dim(alk)[1])
    {
      if(sum(alk[i,-1]) == 0)whichIsMissing[i] = TRUE
    }


    #Assign attribut which say if age is found within haul and length group
    foundWithin = rep(TRUE, dim(alk)[1])
    foundWithin[whichIsMissing] = FALSE

    #Set the smallest length groops to age 0 or 1 if there are no observations of them
    first = which(!whichIsMissing)[1]
    if(first>1)
    {
     if(quarter==1)
      {
        alk[1:(first-1),3] = 1
      }else if(quarter>1)
      {
        alk[1:(first-1),2] = 1
      }
      whichIsMissing[1:first] = FALSE
    }

    #Routine for filling the not observed length classes, III) in datras procedure document for documentation
    distToNext = which(!whichIsMissing)[1]
    distToPrevious = 99999999
    nextValue = NA

    if(quarter ==1)start = 3
    if(quarter >1)start = 2

    for(j in start:dim(alk)[2])
    {
      for(i in 1:dim(alk)[1])
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
            distToNext = 999999999
            nextValue = -999999999
          }else{
            nextValue = alk[i + distToNext + 1,j]
          }
        }
      }
    }
    #--------------------------------------------------------------------------------------------


    alk = as.data.frame(alk)
    names(alk) = c("length", c(0:maxAge))
    attributes(alk)$foundWithin = foundWithin
    return(alk)
}


#' calculateALKNew
#' @description
#' @param RFA Roundfish area number.
#' @param species The species of interest.
#' @param year The year which the ALKs are calculated.
#' @param quarter The quarter of the year which the ALKs are calculated.
#' @param data The CA needed for calculating the ALKs.
#' @param data_hl The HL needed for calculating the ALKs (since there can be trawl hauls without age information).
#' @export
#' @return Returns a list with ALK for each trawl haul
#' @examples
calculateALKHaulbased = function(RFA, species, year, quarter,data,data_hl,lengthDivision = 1:299){

  #Define the list which shall be filled with the ALKs and returned-----
  alkToReturn = list()
  #----------------------------------------------------

  #Extract the data of interest----------------------
  caInterest = data[which(data$Roundfish==RFA & data$Year==year &
                            data$Quarter == quarter & data$Species == species),]

  caInterest = caInterest[which(!is.na(caInterest$Age) & !is.na(caInterest$LngtCm)),]

  hlInterest = data_hl[!is.na(data_hl$Year) & data_hl$Year == year&
                         !is.na(data_hl$Quarter) & data_hl$Quarter == quarter&
                         !is.na(data_hl$Roundfish) & data_hl$Roundfish == RFA ,]

  hlInterest = hlInterest[which(!is.na(hlInterest$LngtCm)),]
  hlInterest = hlInterest[which(!is.na(hlInterest$Species)),]
  #---------------------------------------------------


  #Find the configurations needed for the ALK---------
  conf = confALK(species = species,quarter = quarter)
  maxAge = conf$maxAge
  minLength = conf$minLength
  maxLength = conf$maxLength
  lengthClassIntervallLengths = conf$lengthClassIntervallLengths
  #----------------------------------------------------

  #Create the sceleton of the ALK----------------------
  alk = matrix(0,(maxLength-minLength)/lengthClassIntervallLengths +1, maxAge+3)
  alk[,2] = seq(minLength,maxLength,by = lengthClassIntervallLengths)
  #----------------------------------------------------

  #Investigate if zero data, if so return the sceleton-
  if(dim(caInterest)[1]==0){
    idHaul = unique(c(as.character(hlInterest$haul.id),as.character(caInterest$haul.id)))
    neste=1
    for(id in idHaul){
      idTmp = as.character(id)
      alkThis = as.data.frame(alk)
      names(alkThis) = c("ID","Length","0","1","2","3","4","5","6")
      alkThis$ID[1] = idTmp
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
  ALKnormal = calculateALKDatras(RFA = RFA, species = species, year = year, quarter = quarter,data = caInterest)

  for(id in haulId){
    #Extract which lengts that are of interest (i.e. observed in HL-data by this trawl),
    #Note that we also check CA-data, that is because a few times a fish is meshured in mm in CA and cm in HL---------------------------
    whichLengtsAreInteresting = unique(c(hlInterest$LngtCm[hlInterest$haul.id==id & hlInterest$Species==species],
                                         caInterest$LngtCm[caInterest$haul.id==id & caInterest$Species==species]))

    if(species=="Gadus morhua" | species=="Pollachius virens"){
      whichLengtsAreInteresting = unique(floor(whichLengtsAreInteresting))
    }
    if(length(whichLengtsAreInteresting)>0){
      if(min(whichLengtsAreInteresting)<minLength){
        whichLengtsAreInteresting = c(whichLengtsAreInteresting,minLength)
        whichLengtsAreInteresting = whichLengtsAreInteresting[-which(whichLengtsAreInteresting<minLength)]
      }
      if(max(whichLengtsAreInteresting)>maxLength){
        whichLengtsAreInteresting = c(whichLengtsAreInteresting,maxLength)
        whichLengtsAreInteresting = whichLengtsAreInteresting[-which(whichLengtsAreInteresting>maxLength)]

      }
    }
    whichLengtsAreInteresting = unique(whichLengtsAreInteresting)
    whichIsMissing = rep(TRUE, dim(alk)[1])
    #---------------------------------------------------------------------------------------------------

    #Construct the parts of the ALK were we have data--------------------
    if(species=="Gadus morhua" | species=="Pollachius virens")
    {
      idTmp = as.character(id)
      alkThis = as.data.frame(alk)
      names(alkThis) = c("ID","Length","0","1","2","3","4","5","6")
      alkThis$ID[1] = idTmp

      dataThisTrawl = caInterest[caInterest$haul.id == id,]

      if(dim(dataThisTrawl)[1]>0){
        for(i in 1:dim(dataThisTrawl)[1])
        {
          if(floor(dataThisTrawl$LngtCm[i])< (minLength+1))
          {
            alkThis[1,floor(dataThisTrawl$Age[i])+3] =
              alkThis[1,floor(dataThisTrawl$Age[i])+3] +dataThisTrawl$NoAtALK[i]
            whichIsMissing[1] = FALSE
          }else if(floor(dataThisTrawl$LngtCm[i])>= maxLength)
          {
            alkThis[dim(alkThis)[1],floor(dataThisTrawl$Age[i])+3] =
              alkThis[dim(alkThis)[1],floor(dataThisTrawl$Age[i])+3] +dataThisTrawl$NoAtALK[i]
            whichIsMissing[dim(alkThis)[1]] = FALSE

          }else{
            hvilke = min(which(alkThis[,2]> floor(dataThisTrawl$LngtCm[i]-1)))

            alkThis[hvilke,floor(dataThisTrawl$Age[i])+3] =
              alkThis[hvilke,floor(dataThisTrawl$Age[i])+3] +dataThisTrawl$NoAtALK[i]
            whichIsMissing[hvilke] = FALSE

          }
        }
      }
    }

    #Use same resultion for the ALK as for the sampling
    lengthDivision = c(lengthDivision,9999)
    for(i in 1:(length(lengthDivision)-1)){
      if(lengthDivision[i+1]<=maxLength & lengthDivision[i]>=minLength){
        for(j in 3:(3+maxAge)){
          alkThis[(lengthDivision[i]+1):lengthDivision[i+1]-minLength,j] = sum(alkThis[(lengthDivision[i]+1):lengthDivision[i+1]-minLength,j])
        }
      }else if(lengthDivision[i] < minLength & lengthDivision[i+1]>minLength){
        for(j in 3:(3+maxAge)){
          alkThis[1:sum(alkThis$Length<lengthDivision[i+1]),j] = sum(alkThis[1:sum(alkThis$Length<lengthDivision[i+1]),j])
        }
      }else if(lengthDivision[i+1] > maxLength & lengthDivision[i]<=maxLength ){
        for(j in 3:(3+maxAge)){
          alkThis[(lengthDivision[i]+1-minLength):dim(alkThis)[1],j] = sum(alkThis[(lengthDivision[i]+1-minLength):dim(alkThis)[1],j])
        }
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
      tmpIndeks = tmpIndeks[-(whichLengtsAreInteresting-alkThis$Length[1]+1)]
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
      #Routine for filling the not observed length classes
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



    #Set those we do not find any age  equal the original ALK from datras
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
#' @export
#' @return Returns a list with ALK for each trawl haul
#' @examples
calculateALKModel = function(RFA, species, year, quarter,hh,data, fitModel = NULL,report = NULL){

  useOriginalId = FALSE
  #Fit the model
  if(length(fitModel)==0){
    fitModel =  fitModel(species = species, quarter =quarter, year = year, ca_hh = data,hh = hh)
  }

  if(length(report)>0){
    useOriginalId = TRUE
  }else{
    report = fitModel$obj$report()
  }
  boarder = fitModel$boarder
  listWithOrderedId = fitModel$haulId

  #Define the list which shall be filled with the ALKs and returned-----
  alkToReturn = list()
  #----------------------------------------------------

  if(!fitModel$convergence){
    print("-------------------Convergence NOT OK--------------------------------------------")
    print(fitModel$opt$message)
    stop("Abort because of convergence issues")
  }
  #Extract the estimated continuous GRF-----------------
  Apred = fitModel$Apred
  field1 = Apred %*%report$x[,2]/exp(report$logTau[2])
  field2 = Apred %*%report$x[,3]/exp(report$logTau[3])
  field3 = Apred %*%report$x[,4]/exp(report$logTau[4])
  field4 = Apred %*%report$x[,5]/exp(report$logTau[5])
  field5 = Apred %*%report$x[,6]/exp(report$logTau[6])



  #beta0 = report$beta0
  repLength = report$repLength
  #----------------------------------------------------

  #Extract the data of interest----------------------
  whichHH = which(hh$Roundfish==RFA & hh$Year==year &
                    hh$Quarter == quarter)
  hh = hh[whichHH,]
  #---------------------------------------------------


  #Abort the calculations if no age information in the RFA----
  if(dim(hh)[1]==0)return("No observations in period")
  #-----------------------------------------------------

  #Define variables used in the construction of the ALK--
  conf = confALK(species = species,quarter = quarter)
  maxAge = conf$maxAge
  minLength = conf$minLength
  maxLength = conf$maxLength
  lengthClassIntervallLengths = conf$lengthClassIntervallLengths
  #----------------------------------------------------

  #Define the skelleton of the ALK---------------------
  alk = matrix(0,(maxLength-minLength)/lengthClassIntervallLengths +1, maxAge+3)
  alk[,2] = seq(minLength,maxLength,by = lengthClassIntervallLengths)
  #----------------------------------------------------

  truncationL = setBoundrySpline(species = species, quarter = quarter, ca_hh = data)$l
  truncationU = setBoundrySpline(species = species, quarter = quarter, ca_hh = data)$u


  #Construct each element of the ALK-list----------------------------------------------------------------------
  haulId = unique(hh$haul.id)
  neste = 1
  for(id in haulId){
    if(useOriginalId){
      originalId = hh$originalIdAtThisLocation[which(hh$haul.id==id)]
      omr = which(listWithOrderedId==originalId)
    }else{
      omr = which(listWithOrderedId==id)

    }

    #Construct the parts of the ALK were we have data-------------------
    if(species=="Gadus morhua" | species=="Pollachius virens")
    {
      idTmp = as.character(id)
      alkThis = as.data.frame(alk)
      names(alkThis) = c("ID","Length","0","1","2","3","4","5","6")
      alkThis$ID[1] = idTmp

      if(quarter ==1){
        for(l in 1:length(minLength:maxLength))
        {
          length = (minLength:maxLength)[l]
          nu2 = exp(repLength[length +maxLength*2] +field2[omr])
          nu3 = exp(repLength[length +maxLength*3] +field3[omr])
          nu4 = exp(repLength[length +maxLength*4] +field4[omr])
          nu5 = exp(repLength[length +maxLength*5] +field5[omr])


          if(truncationL[1]>length | truncationU[1]<length)nu2 = 0
          if(truncationL[2]>length | truncationU[2]<length)nu3 = 0
          if(truncationL[3]>length | truncationU[3]<length)nu4 = 0
          if(truncationL[4]>length | truncationU[4]<length)nu5 = 0

          prob2 = nu2/(1+nu2)
          prob3 = nu3/(1+nu3)*(1-prob2)
          prob4 = nu4/(1+nu4)*(1-prob2-prob3)
          prob5 = nu5/(1+nu5)*(1-prob2-prob3-prob4)

          if(length<=boarder){
            alkThis$'0'[l] = 0
            alkThis$'1'[l] = round(1-prob2-prob3-prob4 -prob5, digits = 3)
            alkThis$'6'[l] = 0
          }else{
            alkThis$'0'[l] = 0
            alkThis$'1'[l] = 0
            alkThis$'6'[l] = round(1-prob2-prob3-prob4 -prob5, digits = 3)
          }

          alkThis$'2'[l] = round(prob2,digits = 3)
          alkThis$'3'[l] = round(prob3,digits = 3)
          alkThis$'4'[l] = round(prob4,digits = 3)
          alkThis$'5'[l] = round(prob5,digits = 3)
        }
      }else if(quarter ==3){
        for(l in 1:length(minLength:maxLength))
        {
          length = (minLength:maxLength)[l]
          nu1 = exp(repLength[length +maxLength]   +field1[omr])
          nu2 = exp(repLength[length +maxLength*2] +field2[omr])
          nu3 = exp(repLength[length +maxLength*3] +field3[omr])
          nu4 = exp(repLength[length +maxLength*4] +field4[omr])
          nu5 = exp(repLength[length +maxLength*5] +field5[omr])

          if(truncationL[1]>l | truncationU[1]<l)nu1 = 0
          if(truncationL[2]>l | truncationU[2]<l)nu2 = 0
          if(truncationL[3]>l | truncationU[3]<l)nu3 = 0
          if(truncationL[4]>l | truncationU[4]<l)nu4 = 0
          if(truncationL[5]>l | truncationU[5]<l)nu5 = 0

          prob1 = nu1/(1+nu1)
          prob2 = nu2/(1+nu2)*(1-prob1)
          prob3 = nu3/(1+nu3)*(1-prob1-prob2)
          prob4 = nu4/(1+nu4)*(1-prob1-prob2-prob3)
          prob5 = nu5/(1+nu5)*(1-prob1-prob2-prob3-prob4)

          if(length<=boarder){
            alkThis$'0'[l] =round(1-prob1 -prob2-prob3-prob4 -prob5, digits = 3)
            alkThis$'6'[l] = 0
          }else{
            alkThis$'0'[l] = 0
            alkThis$'6'[l] = round(1-prob1 -prob2-prob3-prob4 -prob5, digits = 3)
          }

          alkThis$'1'[l] = round(prob1,digits = 3)
          alkThis$'2'[l] = round(prob2,digits = 3)
          alkThis$'3'[l] = round(prob3,digits = 3)
          alkThis$'4'[l] = round(prob4,digits = 3)
          alkThis$'5'[l] = round(prob5,digits = 3)
        }
      }
    }
    #Store the ALK for this trawl haul in the list to be returned
    alkToReturn[[neste]] = alkThis
    neste = neste+1
  }
  #--------------------------------------------------------------------

  #Return the list with ALKs------
  return(alkToReturn)
  #-------------------------------
}


#' simModelFisher
#' @description
#' @param RFA Roundfish area number.
#' @param species The species of interest.
#' @param year The year which the ALKs are calculated.
#' @param quarter The quarter of the year which the ALKs are calculated.
#' @param data The CA needed for calculating the ALKs.
#' @param data_hl The HL needed for calculating the ALKs (since there can be trawl hauls without age information).
#' @export
#' @return Returns a list with simulated model based ALK for each trawl haul
#' @examples
simModelFisher = function(species, quarter,rep,fit,sim,i){

  report = fit[[1]]$report()


  report$repLength[which(report$repLength!=0)] = report$repLength[which(report$repLength!=0)] +
    sim[i,which(rep$sd!=0 & names(rep$value)=="repLength")- sum(rep$sd[1:max(which(names(rep$value)=="repLength"))] ==0)]

#  report$beta0[which(report$beta0!=0)] = report$beta0[which(report$beta0!=0)] +
#    sim[i,which(rep$sd!=0 & names(rep$value)=="beta0")- sum(rep$sd[1:max(which(names(rep$value)=="beta0"))] ==0)]

  report$x1[which(report$x1!=0)] = report$x1[which(report$x1!=0)] +
    sim[i,which(rep$sd!=0 & names(rep$value)=="x1")-  sum(rep$sd[1:max(which(names(rep$value)=="x1"))] ==0)]
  report$x2[which(report$x2!=0)] = report$x2[which(report$x2!=0)] +
    sim[i,which(rep$sd!=0 & names(rep$value)=="x2")-  sum(rep$sd[1:max(which(names(rep$value)=="x2"))] ==0)]
  report$x3[which(report$x3!=0)] = report$x3[which(report$x3!=0)] +
    sim[i,which(rep$sd!=0 & names(rep$value)=="x3")-  sum(rep$sd[1:max(which(names(rep$value)=="x3"))] ==0)]
  report$x4[which(report$x4!=0)] = report$x4[which(report$x4!=0)] +
    sim[i,which(rep$sd!=0 & names(rep$value)=="x4")-  sum(rep$sd[1:max(which(names(rep$value)=="x4"))] ==0)]
  report$x5[which(report$x5!=0)] = report$x5[which(report$x5!=0)] +
    sim[i,which(rep$sd!=0 & names(rep$value)=="x5")-  sum(rep$sd[1:max(which(names(rep$value)=="x5"))] ==0)]

  return(report)

}
