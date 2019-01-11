#' readIBTSData
#' @description
#' @param year year of survey
#' @param quarter quarter of survey
#' @param species species of interest
#' @export
#' @return
#' @examples
#'
readIBTSData = function(survey = "NS-IBTS", year, quarter,species)
{
  #Read IBTS-data-----------------------------------------
  dataDir <- system.file("extdata", package = "IBTSindices")
  d<- readExchangeDir(dataDir)
  #-------------------------------------------------------

#  ca = getDATRAS(record = "CA", "NS-IBTS", year, quarter)
#  hl = getDATRAS(record = "HL", "NS-IBTS", year, quarter)
#  hh = getDATRAS(record = "HH", "NS-IBTS", year, quarter)

  #Extract the data frames from IBTS-data and merge them---
  ca <- d[["CA"]]
  hl <- d[["HL"]]
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
  ca = ca[,which(!(names(ca) %in% remove))]
  hl = hl[,which(!(names(hl) %in% remove))]
  hh = hh[,which(!(names(hh) %in% remove))]

  #Remove test hauls without observations----------------------------------
  hh = hh[hh$HaulDur>5,] #Is there a way to know it is a test haul?
  #-------------------------------------------------------------------------


  #Remove data without the speceis of interest, not we include one line of teh hl-data if there are zero observation of the speceis in the haul---
  ca = ca[!is.na(ca$Year) & ca$Year == year&
            !is.na(ca$Quarter) & ca$Quarter == quarter&
            !is.na(ca$Species) & ca$Species == species,]
  hl = hl[!is.na(hl$Year) & hl$Year == year&
            !is.na(hl$Quarter) & hl$Quarter == quarter,]

  for(id in unique(hl$haul.id)){
    if(sum(!is.na(hl$Species[hl$haul.id==id])& hl$Species[hl$haul.id==id]==species)==0){
      hvilke = which(hl$haul.id==id);
      if(length(hvilke)>1)hl = hl[-hvilke[-1],]
    }else{
      hl = hl[-which(!is.na(hl$haul.id)&!is.na(hl$Species)&hl$haul.id==id &hl$Species!=species),]
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
  #---------------------------------------------------------

  #Merge data-----------------------------------------------
  hh_keys <- c("haul.id")
  ca_hh    <- merge(ca,hh, by=hh_keys, suffixes=c(".CA", ""))
  hl_hh    <- merge(hl,hh, by=hh_keys, suffixes=c(".HL", ""))
  #---------------------------------------------------------

  ca_hh$Roundfish = as.character(ca_hh$Roundfish)
  ca_hh$StatRec = as.character(ca_hh$StatRec)

  hl_hh$Roundfish = as.character(hl_hh$Roundfish)
  hl_hh$StatRec = as.character(hl_hh$StatRec)

  hh$haul.id = as.character(hh$haul.id)
  ca_hh$haul.id = as.character(ca_hh$haul.id)
  hl_hh$haul.id = as.character(hl_hh$haul.id)


#  downloadExchange("NS-IBTS", years = 2015)
#  ca = getDATRAS(record = "CA", survey = "NS-IBTS", year = 2015, quarter = 1)

  #Read weights describing the proportion of statrecs of interest-----
  weightsDir <<- system.file("weightsSaithe", package = "IBTSindices")
  weightStatRec = readRDS(paste(weightsDir,"/WeightsStatRecHerringSpratSaithe.Rda",sep = ""))
  weightStatRec$StatRec = as.character(weightStatRec$StatRec)
  #-------------------------------------------------------------------
#  hl_hh$haul.idReal = hl_hh$haul.id



  removeHL = c("Quarter.HL","Country.HL","Year.HL")
  hl_hh = hl_hh[,which(!(names(hl_hh) %in% removeHL))]
  removeCA = c("Quarter.CA","Country.CA","Year.CA")
  ca_hh = ca_hh[,which(!(names(ca_hh) %in% removeCA))]



  toReturn = list()
  toReturn$hh = hh
  toReturn$ca_hh = ca_hh
  toReturn$hl_hh = hl_hh

  toReturn$weightStatRec = weightStatRec


  toReturn$hl_hh$LngtCm = floor(toReturn$hl_hh$LngtCm) #This must be changed if length groups are more detailed than 1 cm
  toReturn$ca_hh$LngtCm = floor(toReturn$ca_hh$LngtCm)

  return(toReturn)
}
