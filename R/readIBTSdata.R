#'
#' @description .
#' @param year year of survey
#' @param quarter quarter of survey
#' @export
#' @return
#' @examples
readIBTSData = function(survey = "NS-IBTS", year, quarter)
{
  #Read IBTS-data-----------------------------------------
  dataDir <<- system.file("Data", package = "TestPackage")
  dat<- readExchangeDir(dataDir)
  #-------------------------------------------------------

#  ca = getDATRAS(record = "CA", "NS-IBTS", year, quarter)
#  hl = getDATRAS(record = "HL", "NS-IBTS", year, quarter)
#  hh = getDATRAS(record = "HH", "NS-IBTS", year, quarter)

  #Extract the data frames from IBTS-data and merge them---
  ca <- dat[["CA"]]
  hl <- dat[["HL"]]
  hh <- dat[["HH"]]

  remove = c("RecordType", "GearExp", "DoorType", "SpecCode","AreaType","Valid_Aphia","Survey",
             "Stratum","HaulVal","HydroStNo","StdSpecRecCode","BycSpecRecCode","Rigging",
             "Tickler","Warplngt", "Warpdia","WarpDen","DoorSurface","DoorSpread","WingSpread",
             "Buoyancy","KiteDim","WgtGroundRope","TowDir","SurCurDir","SpeedWater","SurCurSpeed","BotCurDir","BotCurSpeed",
             "WindDir","WindSpeed","SwellDir","SwellHeight","SurTemp","BotTemp","SurSal","BotSal",
             "timeOfYear","DateofCalculation","ThermoCline","ThClineDepth","DoorWgt","GroundSpeed","Distance","Netopening","Depth","abstime",
             "SweepLngt","Maturity","Ship","Gear","StNo","HaulNo",
             "SpecCodeType","PlusGr","CatCatchWgt","Sex","DayNight","HaulLong","TimeShotHour")
  ca = ca[,which(!(names(ca) %in% remove))]
  hl = hl[,which(!(names(hl) %in% remove))]
  hh = hh[,which(!(names(hh) %in% remove))]

  #There seems to be some missing lengths in the HL-data, removes those
#  hl = hl[!is.na(hl$LngtCm),]
#  ca = ca[!is.na(ca$Age),]

  hh_keys <- c("haul.id")
  hl_keys <- c(hh_keys, c("LngtClas", "Species")) #
  ca_hh    <- merge(ca,hh, by=hh_keys, suffixes=c(".CA", ""))
#  ca_hl    <- merge(ca,hl, by=hl_keys, suffixes=c(".CA", ""))
  hl_hh    <- merge(hl,hh, by=hh_keys, suffixes=c(".HL", ""))
  #---------------------------------------------------------

#  downloadExchange("NS-IBTS", years = 2015)
#  ca = getDATRAS(record = "CA", survey = "NS-IBTS", year = 2015, quarter = 1)

  #Read weights describing the proportion of statrecs of interest-----
  weightsDir <<- system.file("weightsSaithe", package = "TestPackage")
  weightStatRec = readRDS(paste(weightsDir,"/WeightsStatRecHerringSpratSaithe.Rda",sep = ""))
  #-------------------------------------------------------------------

  toReturn = list()
  toReturn$ca = ca
  toReturn$hl = hl
  toReturn$hh = hh
  toReturn$ca_hh = ca_hh
  toReturn$hl_hh = hl_hh

  toReturn$weightStatRec = weightStatRec

  return(toReturn)
}
