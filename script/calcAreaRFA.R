dataDir <<- system.file("Data", package = "TestPackage")
d<- readExchangeDir(dataDir)

hh <- d[["HH"]]

remove = c("RecordType", "GearExp", "DoorType", "SpecCode","AreaType","Valid_Aphia","Survey",
           "Stratum","HaulVal","HydroStNo","StdSpecRecCode","BycSpecRecCode","Rigging",
           "Tickler","Warplngt", "Warpdia","WarpDen","DoorSurface","DoorSpread","WingSpread",
           "Buoyancy","KiteDim","WgtGroundRope","TowDir","SurCurDir","SpeedWater","SurCurSpeed","BotCurDir","BotCurSpeed",
           "WindDir","WindSpeed","SwellDir","SwellHeight","SurTemp","BotTemp","SurSal","BotSal",
           "timeOfYear","DateofCalculation","ThermoCline","ThClineDepth","DoorWgt","GroundSpeed","Distance","Netopening","Depth","abstime",
           "SweepLngt","Maturity","Ship","Gear","StNo","HaulNo",
           "SpecCodeType","PlusGr","CatCatchWgt","Sex","DayNight","HaulLong","HaulLat","ShootLong","ShootLat",
           "TimeShot","TimeShotHour")
hh = hh[,which(!(names(hh) %in% remove))]

weightsDir <- system.file("weightsSaithe", package = "TestPackage")
weights = readRDS(paste(weightsDir,"/WeightsStatRecHerringSpratSaithe.Rda",sep = ""))

rfa <-
  readOGR(file.path(
    system.file("shapefiles", package = "TestPackage"),
    "Roundfish_shapefiles"
  ),verbose = FALSE)
rfa$areas.sqm<-areaPolygon(rfa)
rfa$areas.sqkm<-rfa$areas.sqm/(1000*1000)
#---------------------------------------------------------------

areaRFA = data.frame(1:9,1:9,1:9)
names(areaRFA) = c("RFA","areaSaithe","areaCod")

for(RFA in 1:9){
  areaThisRFA = rfa@data$areas.sqkm[which( as.numeric(as.character(rfa@data$AreaName)) == RFA)]
  if(RFA ==5){#WARNING! By some reason the rfa 5 and 10 are merged in the  datras data
    areaThisRFA = areaThisRFA + rfa@data$areas.sqkm[which( as.numeric(as.character(rfa@data$AreaName)) == 10)]
    }
  areaRFA$areaCod[RFA] = areaThisRFA



  statRec = unique(hh$StatRec[hh$Roundfish==RFA])
  statRec = statRec[!is.na(statRec)]
  sumWeight = 0


  for(i in 1:length(statRec)){
    sumWeight = sumWeight + weights$Weight[which(weights$StatRec==statRec[i])]
  }

  areaThisRFA = areaThisRFA*sumWeight/length(statRec)
  areaRFA$areaSaithe[RFA] = areaThisRFA
}

save(areaRFA,file = "areaRFA.RData")


