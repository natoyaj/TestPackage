#Function to Select hauls: 50, 100, 200, 500, etc

#Read IBTS-data-----------------------------------------
year = 2018
quarter = 1
species = "Gadus morhua";
dataDir <- system.file("extdata", package = "IBTSindices")
d<- readExchangeDir(dataDir)
#-------------------------------------------------------

#Extract the data frames from IBTS-data and merge them---
ca <- d[["CA"]]
hl <- d[["HL"]]
hh <- d[["HH"]]

remove = c("RecordType", "GearExp", "DoorType", "SpecCode","AreaType","Valid_Aphia","Survey",
           "Stratum","HydroStNo","StdSpecRecCode","BycSpecRecCode","Rigging",
           "Tickler","Warplngt", "Warpdia","WarpDen","DoorSurface","DoorSpread","WingSpread",
           "Buoyancy","KiteDim","WgtGroundRope","TowDir","SurCurDir","SpeedWater","SurCurSpeed","BotCurDir","BotCurSpeed",
           "WindDir","WindSpeed","SwellDir","SwellHeight","SurTemp","BotTemp","SurSal","BotSal",
           "timeOfYear","DateofCalculation","ThermoCline","ThClineDepth","DoorWgt","GroundSpeed","Distance","Netopening","Depth","abstime",
           "SweepLngt","Maturity","Ship","Gear","StNo","HaulNo",
           "SpecCodeType","PlusGr","CatCatchWgt","Sex","DayNight","HaulLong","HaulLat","ShootLong","ShootLat",
           "TimeShot","TimeShotHour")
ca = ca[,which(!(names(ca) %in% remove))]
hl = hl[,which(!(names(hl) %in% remove))]
hh = hh[,which(!(names(hh) %in% remove))]

#Remove hauls with haulVal == "I"
idsToRemove = hh$haul.id[which(hh$HaulVal =="I")]
ca = ca[which(! (ca$haul.id %in% idsToRemove)), ]
hl = hl[which(! (hl$haul.id %in% idsToRemove)), ]
hh = hh[which(! (hh$haul.id %in% idsToRemove)), ]

#Remove data without the speceis of interest, not we include one line of teh hl-data if there are zero observation of the speceis in the haul---
ca = ca[!is.na(ca$Year) & ca$Year == year&
          !is.na(ca$Quarter) & ca$Quarter == quarter&
          !is.na(ca$Species) & ca$Species == species,]
hl = hl[!is.na(hl$Year) & hl$Year == year&
          !is.na(hl$Quarter) & hl$Quarter == quarter&
          !is.na(hl$Species),]

for(id in unique(hl$haul.id)){
  if(sum(!is.na(hl$Species[hl$haul.id==id])& hl$Species[hl$haul.id==id]==species)==0){
    hvilke = which(hl$haul.id==id);
    if(length(hvilke)>1)hl = hl[-hvilke[-1],]
  }else{
    if(length(which(!is.na(hl$haul.id)&!is.na(hl$Species)&hl$haul.id==id &hl$Species!=species))>1){
      hl = hl[-which(!is.na(hl$haul.id)&!is.na(hl$Species)&hl$haul.id==id &hl$Species!=species),]
    }
  }
}

hh = hh[!is.na(hh$Year) & hh$Year == year&
          !is.na(hh$Quarter) & hh$Quarter == quarter,]
#-------------------------

#There seems to be some missing lengths in the HL-data, removes those
hl$SubFactor[is.na(hl$LngtCm)] = rep(0,length(hl$LngtCm[is.na(hl$LngtCm)]))
hl$Count[is.na(hl$LngtCm)] = rep(0,length(hl$LngtCm[is.na(hl$LngtCm)]))
hl$Species[is.na(hl$LngtCm) ] = as.factor(rep(NA,length(hl$LngtCm[is.na(hl$LngtCm)])))
hl$LngtCm[is.na(hl$LngtCm)] = rep(-1,length(hl$LngtCm[is.na(hl$LngtCm)]))

ca = ca[!is.na(ca$Age),]

#Duplicate rows with NoAtALK>1 and set NoAtALK = 1--------
ca =  ca[rep(row.names(ca), ca$NoAtALK),]
ca$NoAtALK = rep(1,dim(ca)[1])


#Function to resample hauls-------------------------------------------------------------------------------

#hh_Data: data at the haul level, in this case it is hh. As a test hh was shortened e.g., hh[1:20,]. If the
#Reduced data set of hh
#hh_Data <- hh[1:20,]
#length(unique(hh_Data$haul.id))
#unique(hh_Data$Roundfish)


randomSampleHH <- function(Data, NoHaulsToSample){

#computing probability of hauls in a given RFA occurring in hh dataframe
  prob_of_Hauls_RFA <- Data%>%group_by(Roundfish)%>%summarise(Hauls_in_RFA=n())%>%
                       mutate(prob_of_Hauls_RFA=Hauls_in_RFA/sum(Hauls_in_RFA))


#Resampling a given number of hauls, e.g., 50, 100, 200 in the survey-----------------------------------------

#Selecting Number of Hauls based on probability of occurence in a RFA
 prob_of_Hauls_RFA <- prob_of_Hauls_RFA%>%mutate(Hauls_resampled_in_RFA=round(NoHaulsToSample*prob_of_Hauls_RFA))


#Randomly selecting Number of Hauls from RFA according to probability of occurence of RFA
hh_Data <- Data %>%
           nest(-Roundfish) %>%
           left_join(prob_of_Hauls_RFA, by = "Roundfish") %>%
           mutate(Sample = map2(data, Hauls_resampled_in_RFA, sample_n, replace=T)) %>%
           unnest(Sample)%>%arrange(Roundfish)

hh_Data <- data.frame(hh_Data)

      return (hh_Data)
}
#----------------------------------------------------------------------------------------


##the resampled hauls, hh_Data, must now be merged to ca and hl
#Merge data-------------------------------------------------------
hh_Data_keys <- c("haul.id")
ca_hh_Data    <- merge(ca,hh_Data, by=hh_Data_keys, suffixes=c(".CA", ""))
hl_hh_Data    <- merge(hl,hh_Data, by=hh_Data_keys, suffixes=c(".HL", ""))
nrow(ca_hh_Data)
nrow(hl_hh_Data)

