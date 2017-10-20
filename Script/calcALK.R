#' calculateALK
#' @description Calculates the ALK-key-matrix for a given year, quarter, species and RFA.
#' @param RFA Roundfish area number.
#' @param species The species of interest.
#' @param year The year which the ALK is calculated.
#' @param quarter The quarter of the year which the ALK is calculated.
#' @param data The data needed for calculating the ALK.
#' @export
#' @return Returns a matrix with the ALK
#' @examples calculateALK(RFA = 2,species = "Gadus morhua", year = 2016, quarter = 1)
calculateALK = function(RFA,species,year,quarter,data)
{
    #TODO,

    #Extract the data of interest----------------------
    caInterest = data[which(data$Roundfish==RFA & data$Year==year &
                              data$Quarter == quarter & data$Species == species),]

    caInterest = caInterest[which(!is.na(caInterest$Age) & !is.na(caInterest$LngtCm)),]
    #-----------------------------------------------------

    #Construct the parts of the ALK were we have data-----
    if(species=="Gadus morhua") #TODO: Differentiate between species since some of them are divided into finer lenght scale in the ALK.
    {
      alk = matrix(0,max(floor(caInterest$LngtCm)), max(caInterest$Age) + 1)
      alk[,1] = 1:dim(alk)[1]

      for(i in 1:dim(caInterest)[1])
      {
        alk[floor(caInterest$LngtCm[i]),floor(caInterest$Age[i])+1] =
          alk[floor(caInterest$LngtCm[i]),floor(caInterest$Age[i])+1] +1
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
          #fill in the ALK
        }
      }
    }
    #------------------------------------------------------------------
    return(alk)
}
