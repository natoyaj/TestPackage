library(IBTSindices)
year = 2018
quarter = 1
species = "Gadus morhua";
dat = readIBTSData(survey = "NS-IBTS", year = year, quarter = quarter,species = species)
n=2

bootstrapProcedure = "datras"
ALKprocedure = "datras"
set.seed(1455)
run = CPUEnorthSea(species = species, year = year, quarter = quarter,dat = dat,
                                 bootstrapProcedure = bootstrapProcedure, B = n, ALKprocedure = ALKprocedure)

#runExp = run
#save(runExp, file = "runExpected.RData")
