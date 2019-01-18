library(xlsx)
quarter = 3
species = "Pollachius virens"
n=100

yearStart = 1992
yearStop = 2018
minAge = 0
maxAge = 10

ageIntervall  = seq(minAge,maxAge,by = 1)
yearIntervall = seq(yearStart,yearStop,by = 1)
estimatesMean = matrix(-1,length(ageIntervall),length(yearIntervall) )
estimatesSD = estimatesMean


counterYear = 1
for(year in yearStart:yearStop){
  dat = readIBTSData(survey = "NS-IBTS", year = year, quarter = quarter,species = species)
 # dat$weightStatRec$Weight = rep(1,dim(dat$weightStatRec)[1])
  #-------------------

  #Calculates CPUEs on age-level in the whole North Sea---------
  bootstrapProcedure = "stratifiedHLdatrasCA"
  ALKprocedure = "datras"
  set.seed(131455)
  mCPUEStratifiedHL = CPUEnorthSea(species = species, year = year, quarter = quarter,dat = dat,
                                   bootstrapProcedure = bootstrapProcedure, B = n, ALKprocedure = ALKprocedure)

  estimatesMean[,counterYear] = mCPUEStratifiedHL$mCPUE
  estimatesSD[,counterYear] = mCPUEStratifiedHL$sd
  counterYear = counterYear+1

  print(year)
}


estimatesMean = as.data.frame(estimatesMean)
estimatesSD = as.data.frame(estimatesSD)
names(estimatesMean) = seq(yearStart,yearStop,by = 1)
names(estimatesSD) = seq(yearStart,yearStop,by = 1)
rownames(estimatesMean) = seq(minAge,maxAge)
rownames(estimatesSD) = seq(minAge,maxAge)

write.xlsx(estimatesMean, "meanToJenniferWithWeights.xlsx")
write.xlsx(estimatesSD, "sdToJenniferWithWeights.xlsx")
