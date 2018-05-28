#Read data----------
year = 2015
quarter = 1
dat = readIBTSData(survey = "NS-IBTS", year = year, quarter = quarter)
#-------------------


#Set some additional settings--------
RFA = 1
species = "Gadus morhua"; #species = "Pollachius virens"
n=1 #Number of bootstrap samples
#------------------------------------


#Reproduce CPUEs on length level---------------------------------------
bootstrapProcedure = "stratified"
bootstrapProcedure = "simple"
cpue = getEstimatesCPUElength(RFA = RFA, species = species, year = year, quarter = quarter,dat = dat,bootstrapProcedure = bootstrapProcedure, B = n)
#--------------------------------------------------------------


#Calculates CPUEs on age-level in a given RFA-----------------------------------------
bootstrapProcedure = "datras"
cpueDatras = getEstimatesCPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dat = dat,
                                 bootstrapProcedure = bootstrapProcedure, B = n)
bootstrapProcedure = "stratified"
cpueStratified = getEstimatesCPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dat = dat,
                                     bootstrapProcedure = bootstrapProcedure, B = n)

bootstrapProcedure = "stratifiedNewALK"
cpueHaulBasedStratifiedNew = getEstimatesCPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dat = dat,
                                    bootstrapProcedure = bootstrapProcedure, B = n, ALKprocedure = "haulBased")

#Model-based ALK estimator
bootstrapProcedure = "stratifiedNewALK"
cpueModelBasedstratifiedNewALK = getEstimatesCPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dat = dat,
                                                 bootstrapProcedure = bootstrapProcedure, B = n, ALKprocedure = "modelBased")
#--------------------------------------------------------------


#Calculates CPUEs on age-level in the whole North Sea---------
bootstrapProcedure = "datras"
mCPUEdatras = calcMCPUEwholeNorthSea(species = species, year = year, quarter = quarter,dat = dat,
                       bootstrapProcedure = bootstrapProcedure, B = n)
#--------------------------------------------------------------
