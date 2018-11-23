#Read data----------
library(IBTSindices)
year = 2018
quarter = 1
#species = "Pollachius virens"
species = "Gadus morhua";
dat = readIBTSData(survey = "NS-IBTS", year = year, quarter = quarter,species = species)
#dat$weightStatRec$Weight = rep(1,dim(dat$weightStatRec)[1])
#-------------------

#Set some additional settings--------
RFA = 1
n=10

#path to save results
path = "Papers/manuscript/results/olav/"

#Calculates CPUEs on age-level in the whole North Sea---------
bootstrapProcedure = "stratifiedHLdatrasCA"
ALKprocedure = "datras"
set.seed(131455)
mCPUEStratifiedHL = CPUEnorthSea(species = species, year = year, quarter = quarter,dat = dat,
                           bootstrapProcedure = bootstrapProcedure, B = n, ALKprocedure = ALKprocedure)


bootstrapProcedure = "stratifiedHLandCA"
ALKprocedure = "haulBased"
set.seed(131455)
mCPUEHaulBasedStratifiedHLandCA = CPUEnorthSea(species = species, year = year, quarter = quarter,dat = dat,
                           bootstrapProcedure = bootstrapProcedure, B = n, ALKprocedure = ALKprocedure)


bootstrapProcedure = "stratifiedHLandCA"
ALKprocedure = "modelBased"
set.seed(131455)
mCPUEBasedStratifiedHLmodelALK = CPUEnorthSea(species = species, year = year, quarter = quarter,dat = dat,
                                              bootstrapProcedure = bootstrapProcedure, B = n, ALKprocedure = ALKprocedure)


#Save work
if(species=="Gadus morhua"){
  saveRDS(mCPUEBasedStratifiedHLmodelALK, file = paste(path,"cod",year,"Q",quarter,"n", n,"modelBased", sep= ""))
  saveRDS(mCPUEHaulBasedStratifiedHLandCA, file = paste(path,"cod",year,"Q",quarter,"n", n,"haulBased", sep= ""))
  saveRDS(mCPUEStratifiedHL, file = paste(path,"cod",year,"Q",quarter,"n", n,"datras", sep= ""))
}else if(species=="Pollachius virens"){
  saveRDS(mCPUEBasedStratifiedHLmodelALK, file = paste(path,"Saithe",year,"Q",quarter,"n", n,"modelBased", sep = ""))
  saveRDS(mCPUEHaulBasedStratifiedHLandCA, file = paste(path,"Saithe",year,"Q",quarter,"n", n,"haulBased", sep = ""))
  saveRDS(mCPUEStratifiedHL, file = paste(path,"Saithe",year,"Q",quarter,"n", n,"datras", sep = ""))
}

#--------------------------------------------------------------------------

#Remove parts of the data and see what happens-----------------
set.seed(131455)
for(dl in c(1:5,10,20,30,40,50,60,70,80,90,100)){
  lengthDivision = c(seq(0,max(round(dat$ca_hh$LngtCm)) + 1,by = dl))
  samplesWithinEachIntervall = 5

  removeDatras = investigateRemoval(species = species, year = year, quarter = quarter,dat = dat,B = n,
                                                lengthDivision = lengthDivision,
                                                samplesWithinEachIntervall = samplesWithinEachIntervall)
  #Save work
  if(species=="Gadus morhua"){
    saveRDS(removeDatras, file = paste(path,"RemovalCodDl",dl,"year",year,"Q",quarter,"n", n,ALKprocedure, sep= ""))
  }else if(species=="Pollachius virens"){
    saveRDS(removeDatras, file = paste(path,"RemovalSaitheDl",dl,"year",year,"Q",quarter,"n", n,ALKprocedure, sep = ""))
  }
}
