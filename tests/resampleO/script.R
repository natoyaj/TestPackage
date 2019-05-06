library(IBTSindices)
year = 2018
quarter = 1
species = "Gadus morhua";
dat = readIBTSData(survey = "NS-IBTS", year = year, quarter = quarter,species = species)

dl = 5
n = 1
ALKprocedure =  "datras"
samplesWithinEachIntervall = 2
lengthDivision = c(seq(0,max(round(dat$ca_hh$LngtCm)) + dl,by = dl))
set.seed(1455)
run = investigateRemoval(species = species, year = year, quarter = quarter,dat = dat,B = n,
                                    lengthDivision = lengthDivision,
                                    samplesWithinEachIntervall = samplesWithinEachIntervall)

#runExp = run
#save(runExp, file = "tests/resampleO/runExpected.RData")

