#Read data----------
library(IBTSindices)
year = 2018
quarter = 1
#species = "Pollachius virens"
species = "Gadus morhua";
dat = readIBTSData(survey = "NS-IBTS", year = year, quarter = quarter,species = species)
#-------------------

#Set some additional settings--------
n=400

#path to save results
path = "Papers/manuscript/results/olav/"

#Calculates CPUEs on age-level in the whole North Sea---------
bootstrapProcedure = "datras"
ALKprocedure = "datras"
set.seed(1455)
mCPUEStratifiedHL = CPUEnorthSea(species = species, year = year, quarter = quarter,dat = dat,
                                 bootstrapProcedure = bootstrapProcedure, B = n, ALKprocedure = ALKprocedure)

attributes(mCPUEStratifiedHL)$nNotFoundWithinAllData/(attributes(mCPUEStratifiedHL)$nNotFoundWithinAllData+ attributes(mCPUEStratifiedHL)$nFoundWithinAllData)

bootstrapProcedure = "stratifiedHLandCA"
ALKprocedure = "datras"
set.seed(1455)
mCPUEStratifiedHL = CPUEnorthSea(species = species, year = year, quarter = quarter,dat = dat,
                           bootstrapProcedure = bootstrapProcedure, B = n, ALKprocedure = ALKprocedure)


bootstrapProcedure = "stratifiedHLandCA"
ALKprocedure = "haulBased"
set.seed(1455)
mCPUEHaulBasedStratifiedHLandCA = CPUEnorthSea(species = species, year = year, quarter = quarter,dat = dat,
                           bootstrapProcedure = bootstrapProcedure, B = n, ALKprocedure = ALKprocedure)
attributes(mCPUEHaulBasedStratifiedHLandCA)$nNotFoundWithinAllData/(attributes(mCPUEHaulBasedStratifiedHLandCA)$nNotFoundWithinAllData+ attributes(mCPUEHaulBasedStratifiedHLandCA)$nFoundWithinAllData)


#Save work
if(species=="Gadus morhua"){
  saveRDS(mCPUEHaulBasedStratifiedHLandCA, file = paste(path,"cod",year,"Q",quarter,"n", n,"haulBased", sep= ""))
  saveRDS(mCPUEStratifiedHL, file = paste(path,"cod",year,"Q",quarter,"n", n,"datras", sep= ""))
}else if(species=="Pollachius virens"){
  saveRDS(mCPUEHaulBasedStratifiedHLandCA, file = paste(path,"Saithe",year,"Q",quarter,"n", n,"haulBased", sep = ""))
  saveRDS(mCPUEStratifiedHL, file = paste(path,"Saithe",year,"Q",quarter,"n", n,"datras", sep = ""))
}
#--------------------------------------------------------------------------

#Remove parts of the data and see what happens-----------------
for(dl in c(1:5)){
  set.seed(1455)
  lengthDivision = c(seq(0,max(round(dat$ca_hh$LngtCm)) + dl,by = dl))
  samplesWithinEachIntervall = 1
  ALKprocedure =  "haulBased"

  removeOtoliths = investigateRemoval(species = species, year = year, quarter = quarter,dat = dat,B = n,
                                                lengthDivision = lengthDivision,
                                                samplesWithinEachIntervall = samplesWithinEachIntervall)
  print(dl)
  print(removeOtoliths)
  #Save work
  if(species=="Gadus morhua"){
    saveRDS(removeOtoliths, file = paste(path,"RemovalCodDl",dl,"year",year,"Q",quarter,"n", n,ALKprocedure,samplesWithinEachIntervall, sep= ""))
  }else if(species=="Pollachius virens"){
    saveRDS(removeOtoliths, file = paste(path,"RemovalSaitheDl",dl,"year",year,"Q",quarter,"n", n,ALKprocedure,samplesWithinEachIntervall, sep = ""))
  }
}




#Remove parts of the data and see what happens-----------------
dl = 5
for(samplesWithinEachIntervall in c(5:2,999)){
  set.seed(1455)
  lengthDivision = c(seq(0,max(round(dat$ca_hh$LngtCm)) + dl,by = dl))
  ALKprocedure =  "haulBased"

  removeOtoliths = investigateRemoval(species = species, year = year, quarter = quarter,dat = dat,B = n,
                                      lengthDivision = lengthDivision,
                                      samplesWithinEachIntervall = samplesWithinEachIntervall)
  print(dl)
  print(removeOtoliths)
  #Save work
  if(species=="Gadus morhua"){
    saveRDS(removeOtoliths, file = paste(path,"RemovalCodDl",dl,"year",year,"Q",quarter,"n", n,ALKprocedure,samplesWithinEachIntervall, sep= ""))
  }else if(species=="Pollachius virens"){
    saveRDS(removeOtoliths, file = paste(path,"RemovalSaitheDl",dl,"year",year,"Q",quarter,"n", n,ALKprocedure,samplesWithinEachIntervall, sep = ""))
  }
}


#Resample different number of hauls and ages.
library(doParallel)
cl <- makeCluster(8)
registerDoParallel(cl)
bootstrapProcedure = "datrasHLstratifiedCA"
#ALKprocedure = "datras"
ALKprocedure =  "haulBased"
dl=5
n = 300
start_time <- Sys.time()
k = list()
counter1 = 1
for(year in 2015:2018){
  dat = readIBTSData(survey = "NS-IBTS", year = year, quarter = quarter,species = species)
  k[[counter1]]  = foreach(N = c(50,75,100,150,200,250,300,350)) %dopar% {
    library(IBTSindices)
    runs = list()
    counter = 1
    for(samplesWithinEachIntervall in c(1,5)){
      set.seed(1455)
      lengthDivision = c(seq(0,max(round(dat$ca_hh$LngtCm)) + dl,by = dl))
      mCPUEStratifiedHL = CPUEnorthSea(species = species, year = year, quarter = quarter,dat = dat,
                                       bootstrapProcedure = bootstrapProcedure, B = n, ALKprocedure = ALKprocedure,
                                       onlySimulate = TRUE,nSimHauls = N,samplesWithinEachIntervall = samplesWithinEachIntervall)
      runs[[counter]] = mCPUEStratifiedHL
      counter = counter+ 1

      saveRDS(mCPUEStratifiedHL, file = paste(path,"resampleCodDl",dl,"N",N,"year",year,"Q",quarter,"n", n,ALKprocedure,samplesWithinEachIntervall, sep= ""))
      print(mCPUEStratifiedHL)
    }
    runs
  }
  counter1 = counter1 + 1
  print(year)
}
end_time <- Sys.time()
end_time - start_time




