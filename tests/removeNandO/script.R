#Read data----------
library(IBTSindices)
year = 2018
quarter = 1
species = "Gadus morhua";
dat = readIBTSData(survey = "NS-IBTS", year = year, quarter = quarter,species = species)

n=1
bootstrapProcedure = "datrasHLstratifiedCA"
ALKprocedure = "datras"
dl=5
N = 100
run = list()
counter = 1
for(samplesWithinEachIntervall in c(1,5)){
  set.seed(1455)
  lengthDivision = c(seq(0,max(round(dat$ca_hh$LngtCm)) + dl,by = dl))
  mCPUEStratifiedHL = CPUEnorthSea(species = species, year = year, quarter = quarter,dat = dat,
                                   bootstrapProcedure = bootstrapProcedure, B = n, ALKprocedure = ALKprocedure,
                                   onlySimulate = TRUE,nSimHauls = N,samplesWithinEachIntervall = samplesWithinEachIntervall)
  run[[counter]] = mCPUEStratifiedHL
  counter = counter+ 1
}

#runExp = run
#save(runExp, file = "tests/removeNandO/runExpected.RData")




