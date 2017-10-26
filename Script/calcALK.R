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
    #-----------------------------------------------------

    #Define variables used in the construction of the ALK--
    maxAge = NULL
    minLength = NULL
    maxLength = NULL
    lengthClassIntervallLengths = NULL
    #------------------------------------------------------

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

      alk = matrix(0,(maxLength-minLength + 1)/lengthClassIntervallLengths, maxAge+1)
      alk[,1] = seq(minLength,maxLength,by = lengthClassIntervallLengths)


    }else{
      #TODO: see Annex 1 in datras procedure document for informatiopn regarding ALK for different species amd quarters
    }


    #Construct the parts of the ALK were we have data-----
    if(species=="Gadus morhua")
    {
      for(i in 1:dim(caInterest)[1])
      {
        if(floor(caInterest$LngtCm[i])< minLength)
        {
          alk[1,floor(caInterest$Age[i])+1] =
          alk[1,floor(caInterest$Age[i])+1] +1
        }else if(floor(caInterest$LngtCm[i])> maxLength)
        {
          alk[dim(alk)[1],floor(caInterest$Age[i])+1] =
          alk[dim(alk)[1],floor(caInterest$Age[i])+1] +1
        }else{
          alk[which(alk[,1]==floor(caInterest$LngtCm[i])),floor(caInterest$Age[i])+1] =
          alk[which(alk[,1]==floor(caInterest$LngtCm[i])),floor(caInterest$Age[i])+1] +1
        }

      }
    }
    #------------------------------------------------------

    #Extrapolate the ALK to length calsses were we do not have data----
    for(i in 1:dim(alk)[1])
    {
      for(j in 1:dim(alk)[2])
      {
        if(is.na(alk[i,j]))
        {
          #TODO: Fill in the ALK
        }
      }
    }
    #------------------------------------------------------------------


    alk = as.data.frame(alk)
    names(alk) = c("length", c(1:maxAge))
    return(alk)
}
