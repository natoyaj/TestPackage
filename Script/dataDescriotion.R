species  =  "Gadus morhua" # = "Gadus morhua"; #species = "Pollachius virens"
year = 2017
quarter = 3
dat = readIBTSData(survey = "NS-IBTS", year = year, quarter = quarter)


#number of hauls
hh = dat$hh
hh = hh[hh$Year==year &hh$Quarter==quarter,]
print(paste("number of hauls in year ", year,"and quarter ", quarter,":", dim(hh)[1]))

hl = dat$hl
hl = hl[hl$Year==year &hl$Quarter==quarter &hl$Species == species,]
print(paste("number of hauls in year ", year,"and quarter ", quarter," with length:", length(unique(hl$haul.id))))

ca = dat$ca
ca = ca[ca$Year==year &ca$Quarter==quarter &ca$Species == species,]
print(paste("number of hauls in year ", year,"and quarter ", quarter," with age:", length(unique(ca$haul.id))))

print(paste("number of hauls in year ", year,"and quarter ", quarter,":", length(unique(hl$haul.id))))


#+++++++++++++++++++++++++++++++++++++++++++++++++++++


#Age span
for(i in 0:max(dat$ca_hh$Age[dat$ca_hh$Species ==species  &!is.na(dat$ca_hh$Species)])){
  caAge = dat$ca_hh
  caAge = caAge[caAge$Year==year &caAge$Quarter==quarter &caAge$Species == species & caAge$Age ==i,]
  print(paste("age", i ," in years ", year,"and quarter ", quarter, length(caAge$Age)))
}
max(dat$ca_hh$Age)


#++++++++++++++++++++++++++++++++++++++++++++++++++++


#Length span
LengthSpecies = sort(unique(hl$LngtCm))
c(min(LengthSpecies), max(LengthSpecies))
print(paste("Length in cm ", year,"and quarter ", quarter,":",  c(min(LengthSpecies), max(LengthSpecies))))


#+++++++++++++++++++++++++++++++++++++++++++++++++++

#Duration of trawl hauls

AllHaulDuration = sort(hh$HaulDur)
RemoveCalibrationHaul =  AllHaulDuration[AllHaulDuration>5]       # calibration hauls have durations around 5 minutes or less
MeanHaulDuration = mean(RemoveCalibrationHaul)
print(paste("mean haul duration in year ", year,"and quarter ", quarter,":", MeanHaulDuration))


