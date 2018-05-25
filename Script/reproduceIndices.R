#Read data----------
year = 2015
quarter = 1
dat = readIBTSData(survey = "NS-IBTS", year = year, quarter = quarter)
#-------------------


#Set some additional settings--------
RFA = 1
species = "Gadus morhua"; #species = "Pollachius virens"
n=3 #Number of bootstrap samples
#------------------------------------

#Reproduce CPUEs on length level---------------------------------------
bootstrapProcedure = "stratified"
bootstrapProcedure = "simple"
cpue = getEstimatesCPUElength(RFA = RFA, species = species, year = year, quarter = quarter,dataHL = dat$hl_hh,bootstrapProcedure = bootstrapProcedure, B = n, weightStatRec = dat$weightStatRec)
#--------------------------------------------------------------


#Reproduce CPUEs on age-level-----------------------------------------
#DATRAS ALK estimator
bootstrapProcedure = "datras"
cpueDatras = getEstimatesCPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dataHL = dat$hl_hh, dataCA = dat$ca_hh,
                                 bootstrapProcedure = bootstrapProcedure, B = n,weightStatRec = dat$weightStatRec)
bootstrapProcedure = "stratified"
cpueStratified = getEstimatesCPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dataHL = dat$hl_hh, dataCA = dat$ca_hh,
                                     bootstrapProcedure = bootstrapProcedure, B = n, weightStatRec = dat$weightStatRec)

#Haul based ALK estimator
bootstrapProcedure = "hierarchical"
cpueHaulBasedHierarchical = getEstimatesCPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dataHL = dat$hl_hh, dataCA = dat$ca_hh,
                                                bootstrapProcedure = bootstrapProcedure, B = n, procedure = "haulBased", weightStatRec = dat$weightStatRec)

bootstrapProcedure = "stratifiedNewALK"
cpueHaulBasedStratifiedNew = getEstimatesCPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dataHL = dat$hl_hh, dataCA = dat$ca_hh,
                                    bootstrapProcedure = bootstrapProcedure, B = n, procedure = "haulBased", weightStatRec= dat$weightStatRec)


bootstrapProcedure = "stratified"
cpueHaulBasedStratified = getEstimatesCPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dataHL = dat$hl_hh, dataCA = dat$ca_hh,
                                    bootstrapProcedure = bootstrapProcedure, B = n, procedure = "haulBased", weightStatRec= dat$weightStatRec)


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
cpueModelBased = getEstimatesCPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dataHL = dat$hl_hh, dataCA = dat$ca_hh,
                                     bootstrapProcedure = bootstrapProcedure, B = n, procedure = "modelBased", weightStatRec= dat$weightStatRec)

bootstrapProcedure = "hierarchical"
cpueModelBasedHierarchical = getEstimatesCPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dataHL = dat$hl_hh, dataCA = dat$ca_hh,
                                                 bootstrapProcedure = bootstrapProcedure, B = n, procedure = "modelBased", weightStatRec= dat$weightStatRec)

bootstrapProcedure = "stratifiedNewALK"
cpueModelBasedstratifiedNewALK = getEstimatesCPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dataHL = dat$hl_hh, dataCA = dat$ca_hh,
                                                 bootstrapProcedure = bootstrapProcedure, B = n, procedure = "modelBased", weightStatRec= dat$weightStatRec)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Calcualtes teh CPUE in the whole North Sea
species = "Gadus morhua"
n = 5;
bootstrapProcedure = "datras"
mCPUEdatras = calcMCPUEwholeNorthSea(species = species, year = year, quarter = quarter,dataHL = dat$hl_hh, dataCA = dat$ca_hh,
                       bootstrapProcedure = bootstrapProcedure, B = n, weightStatRec= weightStatRec)





