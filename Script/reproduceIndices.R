#Read data----------
year = 2018
quarter = 1
#species = "Pollachius virens"
species = "Gadus morhua";

dat = readIBTSData(survey = "NS-IBTS", year = year, quarter = quarter,species = species)
#-------------------

#Set some additional settings--------
RFA = 1

n=20 #Number of bootstrap samples
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
nSim = 4
whatToInvestigate = "" #whatToInvestigate = "mean" #See ?investigateRemoval for details
removeProcedure = "edvin"
lengthDivision = c(seq(0,max(round(dat$ca_hh$LngtCm)) + 1,by = 2)) #Currently simulate that we select only one otholit in each of these intervals in each trawl.
#typeOfAreaToInvestigate = "RFA"
typeOfAreaToInvestigate = "wholeNorthSea"
whatToRemove = "CA"

bootstrapProcedure = "stratifiedHLdatrasCA"
ALKprocedure = "haulBased"
Rprof()
removeDatras = investigateRemoval(RFA = RFA, species = species, year = year, quarter = quarter,dat = dat,
                           bootstrapProcedure = bootstrapProcedure, B = n, ALKprocedure = ALKprocedure,
                           removeProcedure = removeProcedure,
                           nSim = nSim,
                           whatToInvestigate = whatToInvestigate,whatToRemove = whatToRemove,typeOfAreaToInvestigate = typeOfAreaToInvestigate,
                           doNotRemoveAbove = 999,
                           lengthDivision = lengthDivision)
Rprof(NULL)
summary = summaryRprof()

#Do the same with several cores
nCores = detectCores()
cl<-makeCluster(nCores)
registerDoParallel(cl)
Rprof()
removeDatrasParallel = investigateRemovalParallel(RFA = RFA, species = species, year = year, quarter = quarter,dat = dat,
                                  bootstrapProcedure = bootstrapProcedure, B = n, ALKprocedure = ALKprocedure,
                                  removeProcedure = removeProcedure,
                                  nSim = nSim,
                                  whatToInvestigate = whatToInvestigate,whatToRemove = whatToRemove,typeOfAreaToInvestigate = typeOfAreaToInvestigate,
                                  doNotRemoveAbove = 999,
                                  lengthDivision = lengthDivision)
Rprof(NULL)
summaryWithParallel = summaryRprof()
stopCluster(cl)
#--------------------------------------------------------------







