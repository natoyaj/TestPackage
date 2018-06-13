#Read data----------
year = 2013
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
cpue = CPUElength(RFA = RFA, species = species, year = year, quarter = quarter,dat = dat,bootstrapProcedure = bootstrapProcedure, B = n)
#--------------------------------------------------------------


#Calculates CPUEs on age-level in a given RFA-----------------------------------------
bootstrapProcedure = "datras"
ALKprocedure = "datras"
cpueDatras = CPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dat = dat,
                                 bootstrapProcedure = bootstrapProcedure, B = n, ALKprocedure = ALKprocedure)


bootstrapProcedure = "stratifiedHLdatrasCA"
ALKprocedure = "datras"
cpueStratifiedHL = CPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dat = dat,
                                     bootstrapProcedure = bootstrapProcedure, B = n, ALKprocedure = ALKprocedure)

bootstrapProcedure = "stratifiedHLandCA"
ALKprocedure = "haulBased"
cpueHaulBasedStratifiedHLandCA = CPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dat = dat,
                                    bootstrapProcedure = bootstrapProcedure, B = n, ALKprocedure = ALKprocedure)

bootstrapProcedure = "stratifiedHLandCA"
ALKprocedure = "modelBased"
cpueHaulBasedStratifiedHLmodelALK= CPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dat = dat,
                                                 bootstrapProcedure = bootstrapProcedure, B = n, ALKprocedure = ALKprocedure)
#--------------------------------------------------------------


#Calculates CPUEs on age-level in the whole North Sea---------
bootstrapProcedure = "datras"
ALKprocedure = "datras"
mCPUEdatras = CPUEnorthSea(species = species, year = year, quarter = quarter,dat = dat,
                       bootstrapProcedure = bootstrapProcedure, B = n, ALKprocedure = ALKprocedure)

bootstrapProcedure = "stratifiedHLdatrasCA"
ALKprocedure = "datras"
mCPUEStratifiedHL = CPUEnorthSea(species = species, year = year, quarter = quarter,dat = dat,
                           bootstrapProcedure = bootstrapProcedure, B = n, ALKprocedure = ALKprocedure)

bootstrapProcedure = "stratifiedHLandCA"
ALKprocedure = "haulBased"
mCPUEHaulBasedStratifiedHLandCA = CPUEnorthSea(species = species, year = year, quarter = quarter,dat = dat,
                           bootstrapProcedure = bootstrapProcedure, B = n, ALKprocedure = ALKprocedure)

bootstrapProcedure = "stratifiedHLandCA"
ALKprocedure = "modelBased" #TODO: currently we do not simulate the ALK for each RFA simultaniously, we simulate the seperatly now (29.5.2018)
mCPUEBasedStratifiedHLmodelALK = CPUEnorthSea(species = species, year = year, quarter = quarter,dat = dat,
                           bootstrapProcedure = bootstrapProcedure, B = n, ALKprocedure = ALKprocedure)
#--------------------------------------------------------------






#Remove parts of the data and see what happens-----------------
removeProcedure = "random" #removeProcedure =  "stratified"
doNotRemoveAbove = 999 #Do not remove fish which is meshured longer than this (in cm)
propRemove = 0.5#Proportion to remove, every sample is removed with this probability
nSim = 30
whatToInvestigate = "mean" #whatToInvestigate = "" #See ?investigateRemoval for details
#typeOfAreaToInvestigate = "RFA"
typeOfAreaToInvestigate = "wholeNorthSea"
whatToRemove = "CA"

bootstrapProcedure = "stratifiedHLdatrasCA"
ALKprocedure = "datras"
removeDatras = investigateRemoval(RFA = RFA, species = species, year = year, quarter = quarter,dat = dat,
                           bootstrapProcedure = bootstrapProcedure, B = n, ALKprocedure = ALKprocedure,
                           removeProcedure = removeProcedure, propRemove = propRemove,
                           nSim = nSim,
                           whatToInvestigate = whatToInvestigate,whatToRemove = whatToRemove,typeOfAreaToInvestigate = typeOfAreaToInvestigate,
                           doNotRemoveAbove = doNotRemoveAbove)
#--------------------------------------------------------------




k = dat$ca_hh[which(dat$ca_hh$LngtCm<=doNotRemoveAbove &
                      dat$ca_hh$Species==species &
                      dat$ca_hh$Quarter==quarter &
                      dat$ca_hh$Year==year),]
if(typeOfAreaToInvestigate == "RFA"){
  k = k[k$Roundfish==RFA,]
}

print(paste("we removed on average ",dim(k)[1]*propRemove, " otholits"))













