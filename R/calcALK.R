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
calculateALK = function(RFA,species,year,quarter,data)
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
      warning(paste("No observations in period given in RFA: " ,RFA,sep = ""))
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
    return(alk)
}



#' simulateALK
#' @description Simulate a given number of ALK-key-matrix for a given year, quarter, species and RFA.
#' @param RFA Roundfish area number.
#' @param species The species of interest.
#' @param year The year which the ALK is calculated.
#' @param quarter The quarter of the year which the ALK is calculated.
#' @param data The data needed for calculating the ALK.
#' @param B The number of simulated ALK to be returned.
#' @export
#' @return Returns a list with simulated ALK given data, species, time, RFA and simulation procedure
#' @examples
simulateALK = function(RFA, species , year, quarter, dataCA,bootstrapProcedure = "simple", B = 10)
{
  #Extract the data of interest
  dataToSimulateFromCA = dataCA[!is.na(dataCA$Year) & dataCA$Year == year&
                                  !is.na(dataCA$Quarter) & dataCA$Quarter == quarter&
                                  !is.na(dataCA$Roundfish) & dataCA$Roundfish == RFA ,]

  ALK = calculateALK(RFA = RFA, species = species, year = year, quarter = quarter,data = dataToSimulateFromCA)

  simALK = list()
  for(i in 1:B)
  {
    if(bootstrapProcedure =="simple")
    {
      simDataCA = simTrawlHaulsCASimple(RFA,year,quarter, data = dataToSimulateFromCA)
    }else if(bootstrapProcedure =="stratified"){
      simDataCA = simTrawlHaulsCAStratified(RFA,year,quarter, data = dataToSimulateFromCA)

    }else{
      return("Select a valid bootstrap procedure.")
    }

    simALK[[i]] = calculateALK(RFA = RFA, species = species, year = year, quarter = quarter,data = simDataCA)
  }
  return(simALK)
}



#' calculateALKNew
#' @description
#' @param RFA Roundfish area number.
#' @param species The species of interest.
#' @param year The year which the ALKs are calculated.
#' @param quarter The quarter of the year which the ALKs are calculated.
#' @param data The CA needed for calculating the ALKs.
#' @param data_hl The HL needed for calculating the ALKs (since there can be trawl hauls without age information).
#' @param dfLength The length of the pooled length class. Default is 1, e.g. 1 length classes in each pooled length class.
#' @export
#' @return Returns a list with ALK for each trawl haul
#' @examples
calculateALKNew = function(RFA, species, year, quarter,data,data_hl,dfLength = 1){

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
    }else{
      tmpIndeks = 1:dim(alkThis)[1]
      tmpIndeks = tmpIndeks[-(whichLengtsAreInteresting-alkThis$Length[1]+1)]
      whichIsMissing[tmpIndeks] = FALSE
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




#' calculateALKModel
#' @description
#' @param RFA Roundfish area number.
#' @param species The species of interest.
#' @param year The year which the ALKs are calculated.
#' @param quarter The quarter of the year which the ALKs are calculated.
#' @export
#' @return Returns a list with ALK for each trawl haul
#' @examples
calculateALKModel = function(RFA, species, year, quarter,hh,simFitModel = NULL, doSimulate = FALSE){

  #Load data, currently only estimated for cod in year 2015
  if(species == "Gadus morhua"){
    if(!doSimulate){
      eval(parse(text =paste( "data('cod",year,"Q",quarter,"')",sep="")))
      eval(parse(text =paste( "fitModel = fitModelCod",year,"Q",quarter,sep="")))
    }else{
      fitModel = simFitModel
    }

    eval(parse(text =paste( "data('keyIdMeshHaulCod",year,"Q",quarter,"')",sep="")))
    eval(parse(text =paste( "keyIdMeshHaul = keyIdMeshHaulCod",year,"Q",quarter,sep="")))
  }else if(species == "Pollachius virens"){
    if(!doSimulate){
      eval(parse(text =paste( "data('saithe",year,"Q",quarter,"')",sep="")))
      eval(parse(text =paste( "fitModel = fitModelSaithe",year,"Q",quarter,sep="")))
    }else{
      fitModel = simFitModel
    }
    eval(parse(text =paste( "data('keyIdMeshHaulSaithe",year,"Q",quarter,"')",sep="")))
    eval(parse(text =paste( "keyIdMeshHaul = keyIdMeshHaulSaithe",year,"Q",quarter,sep="")))
  }

  #Define the list which shall be filled with the ALKs and returned-----
  alkToReturn = list()
  #----------------------------------------------------

  #Extract the data of interest----------------------
  hh = hh[which(hh$Roundfish==RFA & hh$Year==year &
                            hh$Quarter == quarter),]
  #---------------------------------------------------


  #Abort the calculations if no age information is given in the RFA----
  if(dim(hh)[1]==0)return("No observations in period given")
  #-----------------------------------------------------


  #Define variables used in the construction of the ALK--
  maxAge = NULL
  minLength = NULL
  maxLength = NULL
  lengthClassIntervallLengths = NULL
  #----------------------------------------------------

  #Define the skelleton of the ALK---------------------
  if(species == "Gadus morhua"| species=="Pollachius virens")
  {
    maxAge = 6
    minLength = 7
    maxLength = 110
    lengthClassIntervallLengths = 1
    if(quarter == 1)
    {
      minLength = 15
      maxLength = 90
    }

    alk = matrix(0,(maxLength-minLength)/lengthClassIntervallLengths +1, maxAge+3)
    alk[,2] = seq(minLength,maxLength,by = lengthClassIntervallLengths)


  }else{
  }
  #----------------------------------------------------


  #Extract the spatial latent fields---------------------------
  if(species=="Gadus morhua")
  {
    x1 = which(names(fitModel$par.random)=="x1")
    x3 = which(names(fitModel$par.random)=="x3")
    x5 = which(names(fitModel$par.random)=="x5")

    field1 = fitModel$par.random[x1]/exp(fitModel$par.fixed[which(names(fitModel$par.fixed)=="logTau")])[1]
    field2 = field1*0
    field3 = fitModel$par.random[x3]/exp(fitModel$par.fixed[which(names(fitModel$par.fixed)=="logTau")])[2]
    field4 = field1*0
    field5 = fitModel$par.random[x5]/exp(fitModel$par.fixed[which(names(fitModel$par.fixed)=="logTau")])[3]
  }else if (species=="Pollachius virens"){
    x1 = which(names(fitModel$par.random)=="x1")
    x2 = which(names(fitModel$par.random)=="x2")
    x3 = which(names(fitModel$par.random)=="x3")
    x4 = which(names(fitModel$par.random)=="x4")
    x5 = which(names(fitModel$par.random)=="x5")

    add = 0
    if(quarter==3){
      add = 1
      x0 = which(names(fitModel$par.random)=="x0")
      field0 = fitModel$par.random[x0]/exp(fitModel$par.fixed[which(names(fitModel$par.fixed)=="logTau")])[1]
    }else{
      field0 = fitModel$par.random[x1]*0
    }
    field1 = fitModel$par.random[x1]/exp(fitModel$par.fixed[which(names(fitModel$par.fixed)=="logTau")])[1+add]
    field2 = fitModel$par.random[x2]/exp(fitModel$par.fixed[which(names(fitModel$par.fixed)=="logTau")])[2+add]
    field3 = fitModel$par.random[x3]/exp(fitModel$par.fixed[which(names(fitModel$par.fixed)=="logTau")])[3+add]
    field4 = fitModel$par.random[x4]/exp(fitModel$par.fixed[which(names(fitModel$par.fixed)=="logTau")])[4+add]
    field5 = fitModel$par.random[x5]/exp(fitModel$par.fixed[which(names(fitModel$par.fixed)=="logTau")])[5+add]


    field3 = fitModel$par.random[x3]/exp(fitModel$par.fixed[which(names(fitModel$par.fixed)=="logTau")])[1]
    field5 = fitModel$par.random[x5]/exp(fitModel$par.fixed[which(names(fitModel$par.fixed)=="logTau")])[2]
    field0 = rep(0,9999)
    field1 = rep(0,9999)
    field2 = rep(0,9999)
    field3 = rep(0,9999)
    field4 = rep(0,9999)
    field5 = rep(0,9999)
  }
  #----------------------------------------------------


  #Construct each element of the ALK-list----------------------------------------------------------------------
  haulId = unique(hh$haul.id)
  neste = 1
  for(id in haulId){

    omr = keyIdMeshHaul$meshID[which(keyIdMeshHaul$haulID==as.character(id))]
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
          nu1 = exp(fitModel$par.fixed[1]+ fitModel$value[names(fitModel$value)=="repLength"][length] +field1[omr])
          nu2 = exp(fitModel$par.fixed[2]+ fitModel$value[names(fitModel$value)=="repLength"][length +maxLength] +field2[omr])
          nu3 = exp(fitModel$par.fixed[3]+ fitModel$value[names(fitModel$value)=="repLength"][length +maxLength*2] +field3[omr])
          nu4 = exp(fitModel$par.fixed[4]+ fitModel$value[names(fitModel$value)=="repLength"][length +maxLength*3] +field4[omr])
          nu5 = exp(fitModel$par.fixed[5]+ fitModel$value[names(fitModel$value)=="repLength"][length +maxLength*4] +field5[omr])

          probField1 = nu1/(1 + nu1);
          probField2 = nu2/(1 + nu2)*(1-probField1);
          probField3 = nu3/(1 + nu3)*(1-probField1-probField2);
          probField4 = nu4/(1 + nu4)*(1-probField1-probField2-probField3);
          probField5 = nu5/(1 + nu5)*(1-probField1-probField2-probField3-probField4);
          probField6 = 1-probField1-probField2-probField3-probField4-probField5

          alkThis$'0'[l] = 0
          alkThis$'1'[l] = round(probField1,digits = 2)
          alkThis$'2'[l] = round(probField2,digits = 2)
          alkThis$'3'[l] = round(probField3,digits = 2)
          alkThis$'4'[l] = round(probField4,digits = 2)
          alkThis$'5'[l] = round(probField5,digits = 2)
          alkThis$'6'[l] = round(probField6,digits = 2)
        }
      }else if(quarter ==3){
        for(l in 1:length(minLength:maxLength))
        {
          length = (minLength:maxLength)[l]
          nu0 = exp(fitModel$par.fixed[1]+ fitModel$value[names(fitModel$value)=="repLength"][length] +field0[omr])
          nu1 = exp(fitModel$par.fixed[2]+ fitModel$value[names(fitModel$value)=="repLength"][length +maxLength] +field1[omr])
          nu2 = exp(fitModel$par.fixed[3]+ fitModel$value[names(fitModel$value)=="repLength"][length +maxLength*2] +field2[omr])
          nu3 = exp(fitModel$par.fixed[4]+ fitModel$value[names(fitModel$value)=="repLength"][length +maxLength*3] +field3[omr])
          nu4 = exp(fitModel$par.fixed[5]+ fitModel$value[names(fitModel$value)=="repLength"][length +maxLength*4] +field4[omr])
          nu5 = exp(fitModel$par.fixed[6]+ fitModel$value[names(fitModel$value)=="repLength"][length +maxLength*5] +field5[omr])

          probField0 = nu0/(1 + nu0);
          probField1 = nu1/(1 + nu1)*(1-probField0);
          probField2 = nu2/(1 + nu2)*(1-probField0-probField1);
          probField3 = nu3/(1 + nu3)*(1-probField0-probField1-probField2);
          probField4 = nu4/(1 + nu4)*(1-probField0-probField1-probField2-probField3);
          probField5 = nu5/(1 + nu5)*(1-probField0-probField1-probField2-probField3-probField4);
          probField6 = 1-probField0-probField1-probField2-probField3-probField4-probField5

          alkThis$'0'[l] = round(probField0,digits = 2)
          alkThis$'1'[l] = round(probField1,digits = 2)
          alkThis$'2'[l] = round(probField2,digits = 2)
          alkThis$'3'[l] = round(probField3,digits = 2)
          alkThis$'4'[l] = round(probField4,digits = 2)
          alkThis$'5'[l] = round(probField5,digits = 2)
          alkThis$'6'[l] = round(probField6,digits = 2)
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




#' simALKModel
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
simALKModel = function(RFA, species, year, quarter,hh){
  if(species == "Gadus morhua"){
    eval(parse(text =paste( "data('cod",year,"Q",quarter,"')",sep="")))
    eval(parse(text =paste( "fitModel = fitModelCod",year,"Q",quarter,sep="")))

    eval(parse(text =paste( "data('designMatrixForReportCod",year,"Q",quarter,"')",sep="")))
    eval(parse(text =paste( "designMatrixForReport = designMatrixForReportCod",year,"Q",quarter,sep="")))
  }else if(species == "Pollachius virens"){
    eval(parse(text =paste( "data('saithe",year,"Q",quarter,"')",sep="")))
    eval(parse(text =paste( "fitModel = fitModelSaithe",year,"Q",quarter,sep="")))

    eval(parse(text =paste( "data('designMatrixForReportSaithe",year,"Q",quarter,"')",sep="")))
    eval(parse(text =paste( "designMatrixForReport = designMatrixForReportSaithe",year,"Q",quarter,sep="")))
  }

  jointPrec = fitModel$jointPrecision

  mean = rep(0,dim(jointPrec)[1])
  cholPrec = Cholesky(jointPrec)

  simulateFit = rmvn.sparse(1, mean, CH = cholPrec, prec = TRUE)

  namesOrder = names(fitModel$jointPrecision[,1])
  simFitModel = fitModel
  simFitModel$par.fixed[which(names(simFitModel$par.fixed)=="beta0")] = simFitModel$par.fixed[which(names(simFitModel$par.fixed)=="beta0")] + simulateFit[which(namesOrder=="beta0")]

  simFitModel$par.fixed[which(names(simFitModel$par.fixed)=="logTau")] = simFitModel$par.fixed[which(names(simFitModel$par.fixed)=="logTau")] + simulateFit[which(namesOrder=="logTau")]

  simFitModel$par.random[which(names(simFitModel$par.random)=="betaLength")] = simFitModel$par.random[which(names(simFitModel$par.random)=="betaLength")] + simulateFit[which(namesOrder=="betaLength")]
  simFitModel$value[names(simFitModel$value)=="repLength"] = designMatrixForReport%*%simFitModel$par.random[which(names(simFitModel$par.random)=="betaLength")]

  if(species=="Gadus morhua" |species=="Pollachius virens")
  {
    if(quarter==1){
      simFitModel$par.random[which(names(simFitModel$par.random)=="x1")] = simFitModel$par.random[which(names(simFitModel$par.random)=="x1")] + simulateFit[which(namesOrder=="x1")]
      simFitModel$par.random[which(names(simFitModel$par.random)=="x2")] = simFitModel$par.random[which(names(simFitModel$par.random)=="x2")] + simulateFit[which(namesOrder=="x2")]
      simFitModel$par.random[which(names(simFitModel$par.random)=="x3")] = simFitModel$par.random[which(names(simFitModel$par.random)=="x3")] + simulateFit[which(namesOrder=="x3")]
      simFitModel$par.random[which(names(simFitModel$par.random)=="x4")] = simFitModel$par.random[which(names(simFitModel$par.random)=="x4")] + simulateFit[which(namesOrder=="x4")]
      simFitModel$par.random[which(names(simFitModel$par.random)=="x5")] = simFitModel$par.random[which(names(simFitModel$par.random)=="x5")] + simulateFit[which(namesOrder=="x5")]

    }
    if(quarter==3){
#      simFitModel$par.random[which(names(simFitModel$par.random)=="x0")] = simFitModel$par.random[which(names(simFitModel$par.random)=="x0")] + simulateFit[which(namesOrder=="x0")]
#      simFitModel$par.random[which(names(simFitModel$par.random)=="x1")] = simFitModel$par.random[which(names(simFitModel$par.random)=="x1")] + simulateFit[which(namesOrder=="x1")]
#      simFitModel$par.random[which(names(simFitModel$par.random)=="x2")] = simFitModel$par.random[which(names(simFitModel$par.random)=="x2")] + simulateFit[which(namesOrder=="x2")]
#      simFitModel$par.random[which(names(simFitModel$par.random)=="x3")] = simFitModel$par.random[which(names(simFitModel$par.random)=="x3")] + simulateFit[which(namesOrder=="x3")]
#      simFitModel$par.random[which(names(simFitModel$par.random)=="x4")] = simFitModel$par.random[which(names(simFitModel$par.random)=="x4")] + simulateFit[which(namesOrder=="x4")]
#      simFitModel$par.random[which(names(simFitModel$par.random)=="x5")] = simFitModel$par.random[which(names(simFitModel$par.random)=="x5")] + simulateFit[which(namesOrder=="x5")]

#      simFitModel$par.random[which(names(simFitModel$par.random)=="x0")] = simFitModel$par.random[which(names(simFitModel$par.random)=="x0")] + simulateFit[which(namesOrder=="x0")]
#      simFitModel$par.random[which(names(simFitModel$par.random)=="x1")] = simFitModel$par.random[which(names(simFitModel$par.random)=="x1")] + simulateFit[which(namesOrder=="x1")]
#      simFitModel$par.random[which(names(simFitModel$par.random)=="x2")] = simFitModel$par.random[which(names(simFitModel$par.random)=="x2")] + simulateFit[which(namesOrder=="x2")]
      simFitModel$par.random[which(names(simFitModel$par.random)=="x3")] = simFitModel$par.random[which(names(simFitModel$par.random)=="x3")] + simulateFit[which(namesOrder=="x3")]
#      simFitModel$par.random[which(names(simFitModel$par.random)=="x4")] = simFitModel$par.random[which(names(simFitModel$par.random)=="x4")] + simulateFit[which(namesOrder=="x4")]
      simFitModel$par.random[which(names(simFitModel$par.random)=="x5")] = simFitModel$par.random[which(names(simFitModel$par.random)=="x5")] + simulateFit[which(namesOrder=="x5")]

    }
  }
  simALK = calculateALKModel(RFA = RFA, species = species, year = year, quarter = quarter,hh = hh,simFitModel = simFitModel,doSimulate = TRUE)
  #Return the list with ALKs------
  return(simALK)
  #-------------------------------
}
