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

    if(dim(caInterest)[1]==0)return("No observations in period given")

    #Define variables used in the construction of the ALK--
    maxAge = NULL
    minLength = NULL
    maxLength = NULL
    lengthClassIntervallLengths = NULL
    #----------------------------------------------------

    if(species == "Gadus morhua")
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

      alk = matrix(0,(maxLength-minLength + 1)/lengthClassIntervallLengths, maxAge+2)
      alk[,1] = seq(minLength,maxLength,by = lengthClassIntervallLengths)


    }else{
      #TODO: see Annex 1 in datras procedure document for informatiopn regarding ALK for different species amd quarters
    }

    caInterest$Age[caInterest$Age > maxAge] = maxAge


    #Construct the parts of the ALK were we have data-----
    if(species=="Gadus morhua")
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
  #Estimate CPUEs with uncertainty
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
    print(i)
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
#' @param dfLength The length of the pooled length class. Default is 5, e.g. 5 length classes in each pooled length class.
#' @export
#' @return Returns a list with ALK for each trawl haul
#' @examples
calculateALKNew = function(RFA, species, year, quarter,data,data_hl,dfLength = 5){

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
  #---------------------------------------------------

  #Find distance between trawl locations--------------
  id1 = as.character(caInterest$haul.id)
  id2 = as.character(hlInterest$haul.id)
  uniqueId = unique(c(id1,id2))
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
  d <- gDistance(loc, byid=T) #This is a matrix with distances between the relevant trawl hauls
  #-----------------------------------------------------

  #Abort the calculations if no age information is given in the RFA----
  if(dim(caInterest)[1]==0)return("No observations in period given")
  #-----------------------------------------------------


  #Define variables used in the construction of the ALK--
  maxAge = NULL
  minLength = NULL
  maxLength = NULL
  lengthClassIntervallLengths = NULL
  #----------------------------------------------------

  #Define the skelleton of the ALK---------------------
  if(species == "Gadus morhua")
  {
    maxAge = 6
    minLength = 7
    maxLength = 110
    lengthClassIntervallLengths = dfLength
    if(quarter == 1)
    {
      minLength = 15
      maxLength = 90
    }

    alk = matrix(0,(maxLength-minLength)/lengthClassIntervallLengths +1, maxAge+3)
    alk[,2] = seq(minLength,maxLength,by = lengthClassIntervallLengths)


  }else{
    #TODO: see Annex 1 in datras procedure document for informatiopn regarding ALK for different species amd quarters
  }
  #----------------------------------------------------

  #Set old fish to belong to the pluss group-----------
  caInterest$Age[caInterest$Age > maxAge] = maxAge
  #----------------------------------------------------


  #Construct each element of the ALK-list----------------------------------------------------------------------
  haulId = uniqueId
  neste = 1
  for(id in haulId){

    #Construct the parts of the ALK were we have data--------------------
    if(species=="Gadus morhua")
    {
      idTmp = as.character(id)
      alkThis = as.data.frame(alk)
      names(alkThis) = c("ID","Length","0","1","2","3","4","5","6")
      alkThis$ID[1] = idTmp

      dataThisTrawl = caInterest[caInterest$haul.id == id,]

      if(dim(dataThisTrawl)[1]>0){
        for(i in 1:dim(dataThisTrawl)[1])
        {
          if(floor(dataThisTrawl$LngtCm[i])< (minLength+dfLength-1))
          {
            alkThis[1,floor(dataThisTrawl$Age[i])+3] =
              alkThis[1,floor(dataThisTrawl$Age[i])+3] +dataThisTrawl$NoAtALK[i] #!!! SEEMS THAT IT CAN BE MORE THAN ONE FISH PER ROW!!! THIS IS NOT REPORTED ANYWHERE AS I OLAV SEE
          }else if(floor(dataThisTrawl$LngtCm[i])> maxLength)
          {
            alkThis[dim(alkThis)[1],floor(dataThisTrawl$Age[i])+3] =
              alkThis[dim(alkThis)[1],floor(dataThisTrawl$Age[i])+3] +dataThisTrawl$NoAtALK[i]
          }else{
            hvilke = min(which(alkThis[,2]> floor(dataThisTrawl$LngtCm[i]-dfLength+1)))

            alkThis[hvilke,floor(dataThisTrawl$Age[i])+3] =
              alkThis[hvilke,floor(dataThisTrawl$Age[i])+3] +dataThisTrawl$NoAtALK[i]
          }
        }
      }
    }
    #------------------------------------------------------

    #Extrapolate the ALK to length calsses were we do not have data-----------------------------------
    whichIsMissing = rep(FALSE, dim(alkThis)[1])
    for(i in 1:dim(alkThis)[1])
    {
      if(sum(alkThis[i,-c(1,2)]) == 0)whichIsMissing[i] = TRUE
    }
    #Routine for filling the not observed length classes
    if(quarter ==1)start = 3
    if(quarter >1)start = 2

    for(i in 1:dim(alkThis)[1])
    {
      if(whichIsMissing[i])
      {
        sortedShortestDist = order(d[,i])[-1]

        closestSorted = haulId[sortedShortestDist]
        foundAge = FALSE
        nesteHal = 1
        closesId = closestSorted[nesteHal]
        while(!foundAge){
          closestData = caInterest[caInterest$haul.id == closesId,]
          if(i==dim(alkThis)[1]){
            hvilke = which(closestData$LngtCm >= alkThis[i,2])
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
          }else{
            nesteHal = nesteHal+1
            if(nesteHal>length(closestSorted)){
              row[3] = -1 #Set the sum to 0 so that the age composition is owerrited after this for loop
              row[4] = 1
              alkThis[i,3:dim(alkThis)[2]] = row
              foundAge = TRUE
              nesteHal = 1
              }
            closesId = closestSorted[nesteHal]
            #Did not information in this trawl haul, go to next trawl haul.
          }
        }
      }
    }
    #------------------------------------------------------


    #Check if there are zero observed ages in the pooled length class.
    #In that case we fill them as datras suggest---------------------------
    whichIsMissing2 = rep(FALSE, dim(alkThis)[1])
    for(i in 1:dim(alkThis)[1])
    {
      if(sum(alkThis[i,-c(1,2)]) == 0)whichIsMissing2[i] = TRUE
    }

    #Set the smallest length groops to age 0 or 1 if there are no observations of them-----------------
    first = which(!whichIsMissing2)[1]
    if(first>1)
    {
      if(quarter==1)
      {
        alkThis[1:(first-1),4] = 1
      }else if(quarter>1)
      {
        alkThis[1:(first-1),3] = 1
      }
      whichIsMissing2[1:first] = FALSE
    }
    #------------------------------------------------------

    distToNext = which(!whichIsMissing2)[1]
    distToPrevious = 99999999
    nextValue = NA

    if(quarter ==1)start = 3
    if(quarter >1)start = 2

    for(j in start:dim(alkThis)[2])
    {
      for(i in 1:dim(alkThis)[1])
      {
        if(whichIsMissing2[i])
        {
          if(distToPrevious<distToNext)
          {
            alkThis[i,j]= alkThis[i-1,j]
          }else if(distToPrevious == distToNext)
          {
            alkThis[i,j]= (alkThis[i-1,j] + nextValue)/2
          }else if(distToPrevious > distToNext)
          {
            alkThis[i,j]= nextValue
          }
          distToNext  = distToNext -1
          distToPrevious =distToPrevious +1

        }else{
          distToPrevious = 1
          distToNext = which(!whichIsMissing2[i:length(whichIsMissing2)])[2]-2
          if(is.na(distToNext))
          {
            distToNext = 999999999
            nextValue = -999999999
          }else{
            nextValue = alkThis[i + distToNext + 1,j]
          }
        }
      }
      #------------------------------------------------------
    }
    #--------------------------------------------------------------------------------------------




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
#' @param data The CA needed for calculating the ALKs.
#' @param data_hl The HL needed for calculating the ALKs (since there can be trawl hauls without age information).
#' @param dfLength The length of the pooled length class. Default is 5, e.g. 5 length classes in each pooled length class.
#' @export
#' @return Returns a list with ALK for each trawl haul
#' @examples
calculateALKModel = function(RFA, species, year, quarter,hh,fitModel,keyIdMeshHaul){

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
  if(species == "Gadus morhua")
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
    #TODO: see Annex 1 in datras procedure document for informatiopn regarding ALK for different species amd quarters
  }
  #----------------------------------------------------


  #Extract the spatial latent fields---------------------------
  if(species=="Gadus morhua")
  { #TODO, 0 year old cod is not included
    x1 = which(names(fitModel$par.random)=="x1")
    x2 = which(names(fitModel$par.random)=="x2")
    x3 = which(names(fitModel$par.random)=="x3")
    x4 = which(names(fitModel$par.random)=="x4")
    x5 = which(names(fitModel$par.random)=="x5")

    field1 = fitModel$par.random[x1]/exp(fitModel$par.fixed[which(names(fitModel$par.fixed)=="logTau")])
    field2 = fitModel$par.random[x2]/exp(fitModel$par.fixed[which(names(fitModel$par.fixed)=="logTau")])
    field3 = fitModel$par.random[x3]/exp(fitModel$par.fixed[which(names(fitModel$par.fixed)=="logTau")])
    field4 = fitModel$par.random[x4]/exp(fitModel$par.fixed[which(names(fitModel$par.fixed)=="logTau")])
    field5 = fitModel$par.random[x5]/exp(fitModel$par.fixed[which(names(fitModel$par.fixed)=="logTau")])
  }
  #----------------------------------------------------


  #Construct each element of the ALK-list----------------------------------------------------------------------
  haulId = unique(hh$haul.id)
  neste = 1
  for(id in haulId){

    omr = keyIdMeshHaul$meshID[which(keyIdMeshHaul$haulID==as.character(id))]
    #Construct the parts of the ALK were we have data-------------------
    if(species=="Gadus morhua")
    {
      reportLengthStart = 10-1 #TODO change to zero in the model part and here.
      idTmp = as.character(id)
      alkThis = as.data.frame(alk)
      names(alkThis) = c("ID","Length","0","1","2","3","4","5","6")
      alkThis$ID[1] = idTmp

      for(l in 1:length(minLength:maxLength))
      {
        #TODO, 0 year old cod is not included
        length = (minLength:maxLength)[l]
        test1 = exp(fitModel$par.fixed[1]+ fitModel$value[names(fitModel$value)=="repLength1"][length-reportLengthStart] +field1[omr])
        test2 = exp(fitModel$par.fixed[2]+ fitModel$value[names(fitModel$value)=="repLength2"][length-reportLengthStart] +field2[omr])
        test3 = exp(fitModel$par.fixed[3]+ fitModel$value[names(fitModel$value)=="repLength3"][length-reportLengthStart] +field3[omr])
        test4 = exp(fitModel$par.fixed[4]+ fitModel$value[names(fitModel$value)=="repLength4"][length-reportLengthStart] +field4[omr])
        test5 = exp(fitModel$par.fixed[5]+ fitModel$value[names(fitModel$value)=="repLength5"][length-reportLengthStart] +field5[omr])
        sum2 = test1 + test2 + test3 + test4 + test5


        probField1 = test1/(1 + sum2)
        probField2 = test2/(1 + sum2)
        probField3 = test3/(1 + sum2)
        probField4 = test4/(1 + sum2)
        probField5 = test5/(1 + sum2)
        probField6 = 1/(1+sum2)

        alkThis$'0'[l] = 0
        alkThis$'1'[l] = round(probField1,digits = 2)
        alkThis$'2'[l] = round(probField2,digits = 2)
        alkThis$'3'[l] = round(probField3,digits = 2)
        alkThis$'4'[l] = round(probField4,digits = 2)
        alkThis$'5'[l] = round(probField5,digits = 2)
        alkThis$'6'[l] = round(probField6,digits = 2)
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
#' @param dfLength The length of the pooled length class. Default is 5, e.g. 5 length classes in each pooled length class.
#' @export
#' @return Returns a list with simulated model based ALK for each trawl haul
#' @examples
simALKModel = function(RFA, species, year, quarter,hh,fitModel,keyIdMeshHaul){

  jointPrec = fitModel$jointPrecision

  mean = rep(0,dim(jointPrec)[1])
  cholPrec = Cholesky(jointPrec)

  simulateFit = rmvn.sparse(1, mean, CH = cholPrec, prec = TRUE)

  namesOrder = names(fitModel$jointPrecision[,1])
  simFitModel = fitModel
  simFitModel$par.fixed[which(names(simFitModel$par.fixed)=="beta0")] = simFitModel$par.fixed[which(names(simFitModel$par.fixed)=="beta0")] + simulateFit[which(namesOrder=="beta0")]

  simFitModel$par.fixed[which(names(simFitModel$par.fixed)=="lambdaLength1")] = simFitModel$par.fixed[which(names(simFitModel$par.fixed)=="lambdaLength1")] + simulateFit[which(namesOrder=="lambdaLength1")]
  simFitModel$par.fixed[which(names(simFitModel$par.fixed)=="lambdaLength2")] = simFitModel$par.fixed[which(names(simFitModel$par.fixed)=="lambdaLength2")] + simulateFit[which(namesOrder=="lambdaLength2")]
  simFitModel$par.fixed[which(names(simFitModel$par.fixed)=="lambdaLength3")] = simFitModel$par.fixed[which(names(simFitModel$par.fixed)=="lambdaLength3")] + simulateFit[which(namesOrder=="lambdaLength3")]
  simFitModel$par.fixed[which(names(simFitModel$par.fixed)=="lambdaLength4")] = simFitModel$par.fixed[which(names(simFitModel$par.fixed)=="lambdaLength4")] + simulateFit[which(namesOrder=="lambdaLength4")]
  simFitModel$par.fixed[which(names(simFitModel$par.fixed)=="lambdaLength5")] = simFitModel$par.fixed[which(names(simFitModel$par.fixed)=="lambdaLength5")] + simulateFit[which(namesOrder=="lambdaLength5")]
  simFitModel$par.fixed[which(names(simFitModel$par.fixed)=="logTau")] = simFitModel$par.fixed[which(names(simFitModel$par.fixed)=="logTau")] + simulateFit[which(namesOrder=="logTau")]

  simFitModel$par.random[which(names(simFitModel$par.random)=="x1")] = simFitModel$par.random[which(names(simFitModel$par.random)=="x1")] + simulateFit[which(namesOrder=="x1")]
  simFitModel$par.random[which(names(simFitModel$par.random)=="x2")] = simFitModel$par.random[which(names(simFitModel$par.random)=="x2")] + simulateFit[which(namesOrder=="x2")]
  simFitModel$par.random[which(names(simFitModel$par.random)=="x3")] = simFitModel$par.random[which(names(simFitModel$par.random)=="x3")] + simulateFit[which(namesOrder=="x3")]
  simFitModel$par.random[which(names(simFitModel$par.random)=="x4")] = simFitModel$par.random[which(names(simFitModel$par.random)=="x4")] + simulateFit[which(namesOrder=="x4")]
  simFitModel$par.random[which(names(simFitModel$par.random)=="x5")] = simFitModel$par.random[which(names(simFitModel$par.random)=="x5")] + simulateFit[which(namesOrder=="x5")]

  simALK = calculateALKModel(RFA = RFA, species = species, year = year, quarter = quarter,hh = hh,fitModel = simFitModel,keyIdMeshHaul= keyIdMeshHaul)
  simALK
  #Return the list with ALKs------
  return(simALK)
  #-------------------------------
}
