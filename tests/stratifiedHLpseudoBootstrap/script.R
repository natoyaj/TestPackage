library(IBTSindices)
year = 2018
quarter = 1
species = "Gadus morhua";
dat = readIBTSData(survey = "NS-IBTS", year = year, quarter = quarter,species = species)

ALKprocedure = "haulBased"
bootstrapProcedure = "stratifiedHLandCA"
n = 1
set.seed(1455)
run = CPUEnorthSea(species = species, year = year, quarter = quarter,
                   bootstrapProcedure = bootstrapProcedure,B = n,
                   dat = dat, ALKprocedure = ALKprocedure)

#runExp = run
#save(runExp, file = "tests/stratifiedHLpseudoBootstrap/runExpected.RData")
