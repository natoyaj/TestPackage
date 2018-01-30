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

