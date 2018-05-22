
library(TestPackage)
library(geosphere)
library(DATRAS)
library(TMB)
library(Matrix)
library(sp)
library(dplyr)
library(sparseMVN)
#Read IBTS-data-----------------------------------------
dataDir <<- system.file("Data", package = "TestPackage")
dat<- readExchangeDir(dataDir)
#-------------------------------------------------------

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
hl = hl[!is.na(hl$LngtCm),]
ca = ca[!is.na(ca$Age),]

hh_keys <- c("haul.id")
hl_keys <- c(hh_keys, c("LngtClas", "Species")) #
ca_hh    <- merge(ca,hh, by=hh_keys, suffixes=c(".CA", ""))
ca_hl    <- merge(ca,hl, by=hl_keys, suffixes=c(".CA", ""))
hl_hh    <- merge(hl,hh, by=hh_keys, suffixes=c(".HL", ""))
#ca_hl_hh <- merge(ca, hl_hh, by=hl_keys, suffixes=c(".CA", ""))
#---------------------------------------------------------

#Read weights describing the proportion of statrecs of interest-----
weightsDir <<- system.file("weightsSaithe", package = "TestPackage")
weightStatRec = readRDS(paste(weightsDir,"/WeightsStatRecHerringSpratSaithe.Rda",sep = ""))
#-------------------------------------------------------------------


#Reproduce CPUEs with C.I.-----------------------------------------

#Choose the time and RFA
year = 2015
RFA = 1
quarter = 3
species = "Gadus morhua"
species = "Pollachius virens"

bootstrapProcedure = "stratified"
bootstrapProcedure = "simple"

#Rprof()
cpue = getEstimatesCPUElength(RFA = RFA, species = species, year = year, quarter = quarter,dataHL = hl_hh,bootstrapProcedure = bootstrapProcedure, weightStatRec)
#Rprof(NULL)
#summaryRprof()
#--------------------------------------------------------------

#Reproduce CPUEs on age-level-----------------------------------------
year = 2015
RFA = 1
quarter = 3
species = "Gadus morhua"
species = "Pollachius virens"

##three ALK estimators and 3 bootstrap procedures: datras, stratified (haul-based, model-based) and hierarchical

n=3 #Number of samples in the bootstrap.

#DATRAS ALK estimator
bootstrapProcedure = "datras"
cpueDatras = getEstimatesCPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dataHL = hl_hh, dataCA = ca_hh,
                                 bootstrapProcedure = bootstrapProcedure, B = n,weightStatRec = weightStatRec)
bootstrapProcedure = "stratified"
cpueStratified = getEstimatesCPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dataHL = hl_hh, dataCA = ca_hh,
                                     bootstrapProcedure = bootstrapProcedure, B = n, weightStatRec = weightStatRec)


#Haul based ALK estimator
bootstrapProcedure = "hierarchical"
cpueHaulBasedHierarchical = getEstimatesCPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dataHL = hl_hh, dataCA = ca_hh,
                                                bootstrapProcedure = bootstrapProcedure, B = n, procedure = "haulBased", weightStatRec = weightStatRec)

bootstrapProcedure = "stratifiedNewALK"
cpueHaulBasedStratifiedNew = getEstimatesCPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dataHL = hl_hh, dataCA = ca_hh,
                                    bootstrapProcedure = bootstrapProcedure, B = n, procedure = "haulBased", weightStatRec= weightStatRec)


bootstrapProcedure = "stratified"
cpueHaulBasedStratified = getEstimatesCPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dataHL = hl_hh, dataCA = ca_hh,
                                    bootstrapProcedure = bootstrapProcedure, B = n, procedure = "haulBased", weightStatRec= weightStatRec)


#Model-based ALK estimator
#Load data, currently only estimated for cod in year 2015
modelDir <<- system.file("modelFit", package = "TestPackage")
if(species == "Gadus morhua"){
  load(paste(modelDir,"/keyIdMeshHaulCod2015.rda",sep = ""))
  load(paste(modelDir,"/cod2015.rda",sep = ""))
}else if(species == "Pollachius virens"){
  load(paste(modelDir,"/keyIdMeshHaulSaithe2015.rda",sep = ""))
  load(paste(modelDir,"/saithe2015.rda",sep = ""))
}
bootstrapProcedure = "stratified"
cpueModelBased = getEstimatesCPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dataHL = hl_hh, dataCA = ca_hh,
                                     bootstrapProcedure = bootstrapProcedure, B = n, procedure = "modelBased", weightStatRec= weightStatRec)

bootstrapProcedure = "hierarchical"
cpueModelBasedHierarchical = getEstimatesCPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dataHL = hl_hh, dataCA = ca_hh,
                                                 bootstrapProcedure = bootstrapProcedure, B = n, procedure = "modelBased", weightStatRec= weightStatRec)

bootstrapProcedure = "stratifiedNewALK"
cpueModelBasedstratifiedNewALK = getEstimatesCPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dataHL = hl_hh, dataCA = ca_hh,
                                                 bootstrapProcedure = bootstrapProcedure, B = n, procedure = "modelBased", weightStatRec= weightStatRec)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




#Calcualtes teh CPUE in the whole North Sea
year = 2015
quarter = 3
species = "Gadus morhua"
n = 5;
bootstrapProcedure = "datras"
mCPUEdatras = calcMCPUEwholeNorthSea(species = species, year = year, quarter = quarter,dataHL = hl_hh, dataCA = ca_hh,
                       bootstrapProcedure = bootstrapProcedure, B = n, weightStatRec= weightStatRec)





