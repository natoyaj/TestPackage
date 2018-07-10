species = "Gadus morhua"; #species = "Pollachius virens"


#number of hauls
hh = dat$hh
hh = hh[hh$Year==2018 &hh$Quarter==1,]
print(paste("number of hauls in year ", year,"and quarter ", quarter,":", dim(hh)[1]))

hl = dat$hl
hl = hl[hl$Year==2018 &hl$Quarter==1 &hl$Species == species,]
print(paste("number of hauls in year ", year,"and quarter ", quarter," with length:", length(unique(hl$haul.id))))

ca = dat$ca
ca = ca[ca$Year==2018 &ca$Quarter==1 &ca$Species == species,]
print(paste("number of hauls in year ", year,"and quarter ", quarter," with age:", length(unique(ca$haul.id))))

print(paste("number of hauls in year ", year,"and quarter ", quarter,":", length(unique(hl$haul.id))))



#Age span
