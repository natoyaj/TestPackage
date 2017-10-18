calculateALK = function(RFA,species,year,quarter,ca_hh)
{
    caInterest = ca_hh[which(ca_hh$Roundfish==RFA & ca_hh$Year==year &
                               ca_hh$Quarter == quarter & ca_hh$Species == species),]

    caInterest = caInterest[which(!is.na(caInterest$Age) & !is.na(caInterest$LngtCm)),]

    if(species=="Gadus morhua")
    {
      alk = matrix(0,max(floor(caInterest$LngtCm)), max(caInterest$Age) + 1)
      alk[,1] = 1:dim(alk)[1]


      for(i in 1:dim(caInterest)[1])
      {
        alk[floor(caInterest$LngtCm[i]),floor(caInterest$Age[i])+1] =
          alk[floor(caInterest$LngtCm[i]),floor(caInterest$Age[i])+1] +1
      }
    }
}
