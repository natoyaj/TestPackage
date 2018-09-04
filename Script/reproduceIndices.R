#Read data----------

set.seed(201145)
year = 2018
quarter = 1
#species = "Pollachius virens"
species = "Gadus morhua";

dat = readIBTSData(survey = "NS-IBTS", year = year, quarter = quarter,species = species)

#dat$weightStatRec$Weight = rep(1,dim(dat$weightStatRec)[1])
#-------------------

#Set some additional settings--------
RFA = 1

n=200 #Number of bootstrap samples

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
ALKprocedure = "modelBased"
mCPUEBasedStratifiedHLmodelALK = CPUEnorthSea(species = species, year = year, quarter = quarter,dat = dat,
                                              bootstrapProcedure = bootstrapProcedure, B = n, ALKprocedure = ALKprocedure)


#save.image(file="cod2017Q3.rda")
#load("cod2017Q3.rda")

#save.image(file="cod2018Q1.rda")
#load("cod2018Q1.rda")

#save.image(file="saithe2017Q3.rda")
#load("saithe2017Q3.rda")

#save.image(file="saithe2018Q1.rda")
#load("saithe2018Q1.rda")


#--------------------------------------------------------------------------

library(Publish)
require(plotrix)
as.data.frame(mCPUEdatras)
as.data.frame(mCPUEStratifiedHL)
as.data.frame(mCPUEHaulBasedStratifiedHLandCA)
as.data.frame(mCPUEBasedStratifiedHLmodelALK)

#include age 0 for Q3
a=c(1,2,3,4,5,"6+")
#Species = rep("cod",18)
Species = rep("saithe",18)
ALK = rep(c("Datras", "Haul base", "Model base"), each=6)
BootstrapProcedure = rep(c("Datras", "Stratified"), each=7)

rseDatrasDatras = (mCPUEdatras[,7]/mCPUEdatras[,1])*100
rseDatras = (mCPUEStratifiedHL[,7]/mCPUEStratifiedHL[,1])*100
rseHaul = (mCPUEHaulBasedStratifiedHLandCA[,7]/mCPUEHaulBasedStratifiedHLandCA[,1])*100
rseModel = (mCPUEBasedStratifiedHLmodelALK[,7]/mCPUEBasedStratifiedHLmodelALK[,1])*100


#datras
mCPUEdatras= cbind(mCPUEdatras[-1,],rseDatrasDatras[-1])
colnames(mCPUEdatras) = c("mCPUE", "boostrapMean","median", "Q025", "Q975", "BiasCQ025", "BiasCQ975", "sd", "RSE%")


#datrasStratified
mCPUEStratifiedHL= cbind(mCPUEStratifiedHL[-1,],rseDatras[-1])
colnames(mCPUEStratifiedHL) = c("mCPUE", "boostrapMean","median", "Q025", "Q975", "BiasCQ025", "BiasCQ975", "sd", "RSE%")

#haulbase
mCPUEHaulBasedStratifiedHLandCA= cbind(mCPUEHaulBasedStratifiedHLandCA[-1,],rseHaul[-1])
colnames(mCPUEHaulBasedStratifiedHLandCA) = c("mCPUE", "boostrapMean","median", "Q025", "Q975", "BiasCQ025", "BiasCQ975", "sd", "RSE%")

#modelbase
mCPUEBasedStratifiedHLmodelALK= cbind(mCPUEBasedStratifiedHLmodelALK[-1,],rseModel[-1])
colnames(mCPUEBasedStratifiedHLmodelALK) = c("mCPUE", "boostrapMean","median", "Q025", "Q975", "BiasCQ025", "BiasCQ975", "sd", "RSE%")

# combined ALKs.............................
DatrasHaulDataframe = as.data.frame(cbind(ALK, Species, a, rbind(mCPUEStratifiedHL, mCPUEHaulBasedStratifiedHLandCA, mCPUEBasedStratifiedHLmodelALK)))
colnames(DatrasHaulDataframe) = c("ALK", "Species", "a", "lambda.hat_a", "mean","median",
                              "PercentileQ025", "PercentileQ975","BiascorrectedQ025", "BiascorrectedQ975", "SE", "RSE%")

# combined bootstrap procedures...................................
DatrasDataframe = as.data.frame(cbind(Procedure, Species, a, rbind(mCPUEdatras,mCPUEStratifiedHL)))

colnames(DatrasDataframe) = c("Procedure", "Species", "a", "lambda.hat_a", "mean",
                              "PercentileQ025", "PercentileQ975","BiascorrectedQ025", "BiascorrectedQ975", "SE", "RSE%")


tab<-DatrasHaulDataframe[,c("lambda.hat_a", "BiascorrectedQ025", "BiascorrectedQ975")]
names(tab)<-c("lambda.hat_a","lower","upper")




# plots-----------------------------------------------------

# ALKs plot..................
plotConfidence(x=tab,
               labels=format(DatrasHaulDataframe[,c("ALK", "a","Species", "mean","median", "SE", "RSE%")],digits=3))

# or dividing the dataframe
labels <- split(format(DatrasHaulDataframe[,c("a","Species", "mean","median", "SE", "RSE%")], digits=3),
           DatrasHaulDataframe[,c("ALK")])

dev.new(width=8, height=6)
par(mar=c(5,5,2,2))
plotConfidence(x=tab,
              labels=labels,leftmargin = 0.0003, rightmargin = 0.00030, cex=3.5, pch=20, xlim = c(0,10), lwd =2 )



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

nSim = 200
whatToInvestigate = "mean" #whatToInvestigate = "" #See ?investigateRemoval for details

removeProcedure = "edvin"
#removeProcedure = "stratified"
lengthDivision = c(seq(0,max(round(dat$ca_hh$LngtCm)) + 1,by = 4)) #Currently simulate that we select only one otholit in each of these intervals in each trawl.
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

#save.image(file="mean.1cmDat.rda")
#load("mean.1cmDat.rda")


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


fulldata =cbind(removeDatrasParallel2$WithFullData[,1])
removeddata =removeDatrasParallel2$mCPUE
removalAll = cbind(fulldata, removeddata)

#---2018
#save.image(file="mean.1cm.rda")  #231 otoliths from 1600
#load("mean.1cm.rda")

#save.image(file="mean.2cm.rda")
#load("mean.2cm.rda")

#save.image(file="mean.3cm.rda")
#load("mean.3cm.rda")

#save.image(file="mean18.4cm.rda")
#load("mean18.4cm.rda")

#save.image(file="mean.5cm.rda")
#load("mean.5cm.rda")
#xtable(removalAll)

#save.image(file="mean.7cm.rda")
#load("mean.7cm.rda")
#xtable(removalAll)

#save.image(file="mean.10cm.rda")
#load("mean.10cm.rda")
#xtable(removalAll)

#save.image(file="mean.5cm500.rda")    #500 bootstrap samples
#load("mean.5cm.rda")
#xtable(removalAll)


#----2017
#save.image(file="mean17.1cm.rda")  #231 otoliths from 1600
#load("mean17.1cm.rda")

#save.image(file="mean17.2cm.rda")
#load("mean17.2cm.rda")

#save.image(file="mean17.3cm.rda")
#load("mean17.3cm.rda")

#save.image(file="mean.4cm.rda")
#load("mean.4cm.rda")

#save.image(file="mean17.5cm.rda")
#load("mean17.5cm.rda")
#xtable(removalAll)

#save.image(file="mean17.7cm.rda")
#load("mean17.7cm.rda")
#xtable(removalAll)




a=rep(c("age 1","age 2","age 3","age 4","age 5","age 6+"),  6)
Length_group = rep(c("1 cm", "2 cm", "3 cm","4 cm", "5 cm", "7 cm"), each = 6)
# combined ALKs.............................
RemoveHaulDataframe = as.data.frame(cbind(Length_group, a, rbind(removalAll1[-1,], removalAll2[-1,], removalAll3[-1,],
                                         removalAll4[-1,], removalAll5[-1,], removalAll7[-1,])))



#option 1------------------------------------------------
tab<-RemoveHaulDataframe[,c("fulldata" ,"mean", "BiasCQ025", "BiasCQ075")]
names(tab)<-c("fulldata" ,"mean","lower","upper")


plotConfidence(x=tab,
               labels=format(RemoveHaulDataframe[,c("Length_group", "a", "mean", "sd")],digits=3))


# or dividing the dataframe
labels <- split(format(RemoveHaulDataframe[,c("Length_group","fulldata","mean", "sd")], digits=3),
                RemoveHaulDataframe[,c("a")])

dev.new(width=8, height=6)
par(mar=c(5,5,2,2))
plotConfidence(x=tab,
               labels=labels,leftmargin = 0.0003, rightmargin = 0.00030,
               cex=1.0, pch=20, xlim = c(0,10), lwd =2, order=c(1,3,2))


# option 2-----------------------------------------------

p = ggplot(RemoveHaulDataframe ,
           aes(x = Length_group,y = mean, ymin = BiasCQ025, ymax = BiasCQ075 ))+
  geom_pointrange(aes(col=Length_group))+
  geom_hline(aes(fill=Length_group),yintercept =1, linetype=2)+
  xlab("Age")+ ylab("Mean CPUE per age (95% Confidence Interval)")+
  geom_errorbar(aes(ymin=BiasCQ025, ymax=BiasCQ075,col=Length_group),width=0.5,cex=1)+
  facet_wrap(~a,strip.position="left",nrow=6,scales = "free_y") +
  theme(plot.title=element_text(size=16,face="bold"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(face="bold"),
        axis.title=element_text(size=12,face="bold"),
        strip.text.y = element_text(hjust=0,vjust = 1,angle=180,face="bold"))+
  coord_flip()
p

