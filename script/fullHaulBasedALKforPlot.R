#This function is a help function for plotting the haul based ALK
calculateALKNewFull = function(RFA, species, year, quarter,data,data_hl,dfLength = 1){

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
    warning(paste("No observations in period given in RFA: " ,RFA,sep = ""))
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
  ALKnormal = calculateALK(RFA = RFA, species = species, year = year, quarter = quarter,data = caInterest)

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
          if(floor(dataThisTrawl$LngtCm[i])< (minLength+dfLength))
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
            hvilke = min(which(alkThis[,2]> floor(dataThisTrawl$LngtCm[i]-dfLength)))

            alkThis[hvilke,floor(dataThisTrawl$Age[i])+3] =
              alkThis[hvilke,floor(dataThisTrawl$Age[i])+3] +dataThisTrawl$NoAtALK[i]
            whichIsMissing[hvilke] = FALSE

          }
        }
      }
    }

    if(length(whichLengtsAreInteresting)==0){
      whichIsMissing = rep(FALSE, dim(alkThis)[1])
    }
    #------------------------------------------------------


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
    #Set those we do not find any age  equal the original ALK from datras, this works only if dfLength = 1
    alkThis[whichIsMissing,2:(maxAge+3)] = ALKnormal[whichIsMissing,]

    #Store the ALK for this trawl haul in the list to be returned
    alkToReturn[[neste]] = alkThis
    neste = neste+1
  }

  #------------------------------------------------------------------------------------------------------------------------

  #Return the list with ALKs------
  return(alkToReturn)
  #-------------------------------
}

