library(IBTSindices)
year = 2018
quarter = 1
species = "Gadus morhua";
dat = readIBTSData(survey = "NS-IBTS", year = year, quarter = quarter,species = species)

ALKprocedure = "haulBased"
set.seed(1455)
run = CPUEnorthSea(species = species, year = year, quarter = quarter,
                   dat = dat, ALKprocedure = ALKprocedure,doBootstrap = FALSE)

#runExp = run
#save(runExp, file = "runExpected.RData")
