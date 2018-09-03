#Read data----------

set.seed(201145)
year = 2017
quarter = 3
#species = "Pollachius virens"
species = "Gadus morhua";

dat = readIBTSData(survey = "NS-IBTS", year = year, quarter = quarter,species = species)

#dat$weightStatRec$Weight = rep(1,dim(dat$weightStatRec)[1])
#-------------------

#Set some additional settings--------
RFA = 1



n=5 #Number of bootstrap samples

n=100 #Number of bootstrap samples
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


#save.image(file="cod2017Q3.rda")
#load("cod2017Q3.rda")

#save.image(file="cod2018Q1.rda")
#load("cod2018Q1.rda")

#save.image(file="saithe2017Q3.rda")
#load("saithe2017Q3.rda")

#save.image(file="saithe2018Q1.rda")
#load("saithe2018Q1.rda")


bootstrapProcedure = "stratifiedHLandCA"
ALKprocedure = "modelBased"
mCPUEBasedStratifiedHLmodelALK = CPUEnorthSea(species = species, year = year, quarter = quarter,dat = dat,
                           bootstrapProcedure = bootstrapProcedure, B = n, ALKprocedure = ALKprocedure)

#--------------------------------------------------------------------------

library(Publish)
require(plotrix)
as.data.frame(mCPUEdatras)
as.data.frame(mCPUEStratifiedHL)
as.data.frame(mCPUEHaulBasedStratifiedHLandCA)
as.data.frame(mCPUEBasedStratifiedHLmodelALK)

a=c(0,1,2,3,4,5,"6+")
Species = rep("cod",14)
#Species = rep("saithe",14)
ALK = rep(c("Datras", "Haul base"), each=7)
BootstrapProcedure = rep(c("Datras", "Stratified"), each=7)

#datras
mCPUEdatras= cbind(mCPUEdatras,(mCPUEdatras[,7]/mCPUEdatras[,1])*100)
colnames(mCPUEdatras) = c("mCPUE", "boostrapMean", "Q025", "Q975", "BiasCQ025", "BiasCQ975", "sd", "RSE%")

#datrasStratified
mCPUEStratifiedHL= cbind(mCPUEStratifiedHL,(mCPUEStratifiedHL[,7]/mCPUEStratifiedHL[,1])*100)
colnames(mCPUEStratifiedHL) = c("mCPUE", "boostrapMean", "Q025", "Q975", "BiasCQ025", "BiasCQ975", "sd", "RSE%")

#haulbase
mCPUEHaulBasedStratifiedHLandCA= cbind(mCPUEHaulBasedStratifiedHLandCA,
                                  (mCPUEHaulBasedStratifiedHLandCA[,7]/mCPUEHaulBasedStratifiedHLandCA[,1])*100)
colnames(mCPUEHaulBasedStratifiedHLandCA) = c("mCPUE", "boostrapMean", "Q025", "Q975", "BiasCQ025", "BiasCQ975", "sd", "RSE%")


# combined ALKs.............................
DatrasHaulDataframe = as.data.frame(cbind(ALK, Species, a, rbind(mCPUEStratifiedHL, mCPUEHaulBasedStratifiedHLandCA)))
colnames(DatrasHaulDataframe) = c("ALK", "Species", "a", "lambda.hat_a", "mean",
                              "PercentileQ025", "PercentileQ975","BiascorrectedQ025", "BiascorrectedQ975", "SE(Estimate)", "RSE%")

# combined bootstrap procedures...................................
DatrasDataframe = as.data.frame(cbind(Procedure, Species, a, rbind(mCPUEdatras,mCPUEStratifiedHL)))

colnames(DatrasDataframe) = c("Procedure", "Species", "a", "lambda.hat_a", "mean",
                              "PercentileQ025", "PercentileQ975","BiascorrectedQ025", "BiascorrectedQ975", "SE(Estimate)", "RSE%")


tab<-DatrasHaulDataframe[,c("lambda.hat_a", "BiascorrectedQ025", "BiascorrectedQ975")]
names(tab)<-c("lambda.hat_a","lower","upper")




# plots-----------------------------------------------------

# ALKs plot..................
plotConfidence(x=tab,
               labels=format(DatrasHaulDataframe[,c("ALK", "a","Species", "mean","SE(Estimate)", "RSE%")],digits=3))

# or dividing the dataframe
labels <- split(format(DatrasHaulDataframe[,c("a","Species", "mean","SE(Estimate)", "RSE%")], digits=3),
           DatrasHaulDataframe[,c("ALK")])

dev.new(width=8, height=6)
par(mar=c(5,5,2,2))
plotConfidence(x=tab,
              labels=labels,leftmargin = 0.010, rightmargin = 0.010, cex=3, pch=20 , col="red")



# Bootstrap procedure plot......................................
plotConfidence(x=tab,
               labels=format(DatrasDataframe[,c("Procedure", "a","Species", "mean","SE(Estimate)", "RSE%")],digits=3))

# or dividing the dataframe
labels <- split(format(DatrasDataframe[,c("a","Species", "mean","SE(Estimate)", "RSE%")], digits=3),
            DatrasDataframe[,c("Procedure")])

dev.new(width=8, height=6)
par(mar=c(5,5,2,2))
plotConfidence(x=tab,
               labels=labels,leftmargin = 0.010, rightmargin = 0.010, cex=3, pch=20 )



#--------------------------------------------------------------

#Remove parts of the data and see what happens-----------------


nSim = 4
whatToInvestigate = "mean" #whatToInvestigate = "mean" #See ?investigateRemoval for details

nSim = 50
whatToInvestigate = "mean" #whatToInvestigate = "" #See ?investigateRemoval for details

removeProcedure = "edvin"
#removeProcedure = "stratified"
lengthDivision = c(seq(0,max(round(dat$ca_hh$LngtCm)) + 1,by = 1)) #Currently simulate that we select only one otholit in each of these intervals in each trawl.
#typeOfAreaToInvestigate = "RFA"
propRemove = 0.25
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
                           doNotRemoveAbove = 999,propRemove = propRemove,
                           lengthDivision = lengthDivision)
Rprof(NULL)
summary = summaryRprof()

#Do the same with several cores
nCores = detectCores()
cl<-makeCluster(nCores)
registerDoParallel(cl)
Rprof()
removeDatrasParallel2 = investigateRemovalParallel(RFA = RFA, species = species, year = year, quarter = quarter,dat = dat,
                                  bootstrapProcedure = bootstrapProcedure, B = n, ALKprocedure = ALKprocedure,
                                  removeProcedure = removeProcedure,
                                  nSim = nSim,
                                  whatToInvestigate = whatToInvestigate,whatToRemove = whatToRemove,typeOfAreaToInvestigate = typeOfAreaToInvestigate,
                                  doNotRemoveAbove = 999,propRemove = propRemove,
                                  lengthDivision = lengthDivision)
Rprof(NULL)
summaryWithParallel = summaryRprof()
stopCluster(cl)


fulldata =cbind(removeDatrasParallel$WithFullData[,1])
removeddata =removeDatrasParallel$mCPUE
removalAll = cbind(fulldata, removeddata)
xtable(removalAll)
#--------------------------------------------------------------

#combine all estimates from 2cm, 3cm, 4cm, 5cm,.....15cm with a plot

plotCI(age,removeDatrasParallel$mCPUE$mean, ui=removeDatrasParallel$mCPUE$Q975, li =removeDatrasParallel$mCPUE$Q025)

#---------------------------------------------------------------


#save.image(file="mean.2cm.rda")
#load("mean.2cm.rda")



