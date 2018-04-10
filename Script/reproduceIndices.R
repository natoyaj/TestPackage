
library(TestPackage)
library(DATRAS)
library(TMB)
library(Matrix)
library(sp)
library(dplyr)
library(sparseMVN)
#Read IBTS-data-----------------------------------------
dataDir <<- system.file("Data", package = "TestPackage")
dat<- readExchangeDir(dataDir)
#-------------------------------------------------------

#Path to fitted model-----------------------------------------
modelDir <<- system.file("modelFit", package = "TestPackage")
#-------------------------------------------------------

#Extract the data frames from IBTS-data and merge them---
ca <- dat[["CA"]]
hl <- dat[["HL"]]
hh <- dat[["HH"]]

remove = c("RecordType", "GearExp", "DoorType", "SpecCode","AreaType","Valid_Aphia","Survey",
           "Stratum","HaulVal","HydroStNo","StdSpecRecCode","BycSpecRecCode","Rigging",
           "Tickler","Warplngt", "Warpdia","WarpDen","DoorSurface","DoorSpread","WingSpread",
           "Buoyancy","KiteDim","WgtGroundRope","TowDir","SurCurDir","SpeedWater","SurCurSpeed","BotCurDir","BotCurSpeed",
           "WindDir","WindSpeed","SwellDir","SwellHeight","SurTemp","BotTemp","SurSal","BotSal",
           "timeOfYear","DateofCalculation","ThermoCline","ThClineDepth","DoorWgt","GroundSpeed","Distance","Netopening","Depth","abstime",
           "SweepLngt","Maturity","Ship","Gear","StNo","HaulNo",
           "SpecCodeType","PlusGr","CatCatchWgt","Sex","DayNight","HaulLong","TimeShotHour")
ca = ca[,which(!(names(ca) %in% remove))]
hl = hl[,which(!(names(hl) %in% remove))]
hh = hh[,which(!(names(hh) %in% remove))]

hh_keys <- c("haul.id")
hl_keys <- c(hh_keys, c("LngtClas", "Species")) #
ca_hh    <- merge(ca,hh, by=hh_keys, suffixes=c(".CA", ""))
ca_hl    <- merge(ca,hl, by=hl_keys, suffixes=c(".CA", ""))
hl_hh    <- merge(hl,hh, by=hh_keys, suffixes=c(".HL", ""))
#ca_hl_hh <- merge(ca, hl_hh, by=hl_keys, suffixes=c(".CA", ""))
#---------------------------------------------------------

#Read weights describing the proportion of statrecs of interest-----
#weightStatRec = readRDS(paste(dataDir,"/WeightsStatRecHerringSpratSaithe.Rda",sep = ""))
#-------------------------------------------------------------------


#Reproduce CPUEs with C.I.-----------------------------------------

#Choose the time and RFA
year = 2015
RFA = 1
quarter = 1
species = "Gadus morhua"
bootstrapProcedure = "stratified"
bootstrapProcedure = "simple"

#Rprof()
cpue = getEstimatesCPUElength(RFA = RFA, species = species, year = year, quarter = quarter,dataHL = hl_hh,bootstrapProcedure = bootstrapProcedure)
#Rprof(NULL)
#summaryRprof()
#--------------------------------------------------------------

#Reproduce CPUEs on age-level-----------------------------------------
year = 2015
RFA = 1
quarter = 1
species = "Gadus morhua"
bootstrapProcedure = "stratified"

load(paste(modelDir,"/keyIdMeshHaulCod2015.rda",sep = ""))
load(paste(modelDir,"/cod2015.rda",sep = ""))

##three ALK estimators and 3 bootstrap procedures: datras, stratified (haul-based, model-based) and hierarchical
n=25

#DATRAS ALK estimator
bootstrapProcedure = "almost the datras procedure"
cpueDatras = getEstimatesCPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dataHL = hl_hh, dataCA = ca_hh,
                                 bootstrapProcedure = bootstrapProcedure, B = n)
bootstrapProcedure = "stratified"
cpueStratified = getEstimatesCPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dataHL = hl_hh, dataCA = ca_hh,
                                     bootstrapProcedure = bootstrapProcedure, B = n)


#Haul based ALK estimator
bootstrapProcedure = "hierarchical"
cpueHaulBasedHierarchical = getEstimatesCPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dataHL = hl_hh, dataCA = ca_hh,
                                                bootstrapProcedure = bootstrapProcedure, B = n, procedure = "haulBased")
#Error in d[, i] : subscript out of bounds: restrict sample size?

bootstrapProcedure = "stratifiedNewALK"
cpueHaulBased = getEstimatesCPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dataHL = hl_hh, dataCA = ca_hh,
                                    bootstrapProcedure = bootstrapProcedure, B = n, procedure = "haulBased")

#Model-based ALK estimator
bootstrapProcedure = "stratified"
cpueModelBased = getEstimatesCPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dataHL = hl_hh, dataCA = ca_hh,
                                     bootstrapProcedure = bootstrapProcedure, B = n, procedure = "modelBased")

bootstrapProcedure = "hierarchical"
cpueModelBasedHierarchical = getEstimatesCPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dataHL = hl_hh, dataCA = ca_hh,
                                                 bootstrapProcedure = bootstrapProcedure, B = n, procedure = "modelBased")



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++





procedure = "modelBased"
cpueModel = getEstimatesCPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dataHL = hl_hh, dataCA = ca_hh,
                              bootstrapProcedure = bootstrapProcedure, B = 10,procedure = procedure)


bootstrapProcedure = "stratifiedNewALK"
procedure = "haulBased"
cpueNew = getEstimatesCPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dataHL = hl_hh, dataCA = ca_hh,
                              bootstrapProcedure = bootstrapProcedure, B = 10,procedure = procedure)

newProcedure = TRUE
Rprof()

cpue = getEstimatesCPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dataHL = hl_hh, dataCA = ca_hh,
                                     bootstrapProcedure = bootstrapProcedure, B = 10, newProcedure = newProcedure)
Rprof(NULL)
summaryRprof()


bootstrapProcedure = "simple"
cpueSimpleNew = getEstimatesCPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dataHL = hl_hh, dataCA = ca_hh,
                                     bootstrapProcedure = bootstrapProcedure, B = 20, procedure="haulBased")

cpueSimpleOld = getEstimatesCPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dataHL = hl_hh, dataCA = ca_hh,
                                 bootstrapProcedure = bootstrapProcedure, B = 20, procedure="")


bootstrapProcedure = "simple2"
cpueSimple2 = getEstimatesCPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dataHL = hl_hh, dataCA = ca_hh,
                                 bootstrapProcedure = bootstrapProcedure, B = 10)
bootstrapProcedure = "simple3"
cpueSimple3 = getEstimatesCPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dataHL = hl_hh, dataCA = ca_hh,
                                  bootstrapProcedure = bootstrapProcedure, B = 10)


#++++++++++++++++++++++++++++++++++++++++++++







#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Extracting data to plot overlapping age length compositions
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Subset data in R
#PineTreeGrade3Data<-subset(StudentData, SchoolName=="Pine Tree Elementary" & Grade==3)
#Gadus morhua

age1.1 <- ca_hl[ca_hl$Species=="Pollachius virens" &  ca_hl$Age =="1"  & ca_hl$Quarter=="1",]
age1.1
sort(unique(age1.1$LngtCm))

sum(!is.na(ca_hl[ca_hl$Species=="Pollachius virens" &  ca_hl$Age =="2" & ca_hl$Quarter=="1",]$NoAtALK))



age1.1 <- ca[ca$Species=="Gadus morhua" &  ca$Age =="8"  & ca$Quarter=="1",]
age1.1
sort(unique(age1.1$LngtCm))
sum(!is.na(ca[ca$Species=="Gadus morhua" &  ca$Age =="8" & ca$Quarter=="1",]$NoAtALK))



age1.1 <- ca[ca$Species=="Gadus morhua" &  ca$Age =="1"  & ca$Year =="2015" & ca$Quarter=="1",]
age1.1
sort(unique(age1.1$LngtCm))
sum(!is.na(ca[ca$Species=="Gadus morhua" &  ca$Age =="1"  & ca$Year =="2015" & ca$Quarter=="1",]$NoAtALK))




sum(!is.na(ca_hl[ca_hl$Species=="Pollachius virens" &  ca_hl$Age =="18" & ca_hl$Year =="2015" & ca_hl$Quarter=="1",]$NoAtALK))
age1.1 <- ca[ca$Species=="Pollachius virens" &  ca$Age =="18" & ca$Year =="2015" & ca$Quarter=="1",]
age1.1
sort(unique(age1.1$LngtCm))




length(unique(hh[hh$Roundfish=="10",]$StatRec))


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
sum(!is.na(ca_hl[ca_hl$Species=="Pollachius virens",]$NoMeas))
sum(!is.na(ca_hl[ca_hl$Species=="Pollachius virens" &  ca_hl$Age =="1" & ca_hl$Year =="2015" & ca_hl$Quarter=="1",]$NoAtALK))
sum(!is.na(ca_hl[ca_hl$Species=="Pollachius virens" &  ca_hl$Age =="1" & ca_hl$Year =="2015" & ca_hl$Quarter=="1",]$NoMeas))

unique(ca-hl[ca_hl$Species == "Gadus morhua" & ca_hl$Age =="1" & ca_hl$Year== "2015" & ca$Quarter=="1",])


!is.na(ca_hl[ca_hl$Species=="Gadus morhua" &  ca_hl$Age =="1" & ca_hl$Year =="2015" & ca_hl$Quarter=="1",])


age1 <- as.data.frame(table(ca_hl[ca_hl$Species=="Gadus morhua" & ca_hl$Age==1,]$LngtCm))
age2 <- as.data.frame(table(ca_hl[ca_hl$Species=="Gadus morhua" & ca_hl$Age==2,]$LngtCm))
age3 <- as.data.frame(table(ca_hl[ca_hl$Species=="Gadus morhua" & ca_hl$Age==3,]$LngtCm))
age4 <- as.data.frame(table(ca_hl[ca_hl$Species=="Gadus morhua" & ca_hl$Age==4,]$LngtCm))
age5 <- as.data.frame(table(ca_hl[ca_hl$Species=="Gadus morhua" & ca_hl$Age==5,]$LngtCm))
age6 <- as.data.frame(table(ca_hl[ca_hl$Species=="Gadus morhua" & ca_hl$Age==6,]$LngtCm))
age7 <- as.data.frame(table(ca_hl[ca_hl$Species=="Gadus morhua" & ca_hl$Age==7,]$LngtCm))

#Reduce(function(x, y) merge(x, y, all=TRUE), list(age1,age2,age3,age4,age5,age6,age7))

age1.2 <- merge.data.frame(age1,age2, by=c("Var1"))
age1.2 <- cbind(age1.2, rowSums(age1.2[, c("Freq.x", "Freq.y")]))
age1.2.3 <- merge.data.frame(age1.2,age3, by=c("Var1"))
cbind(age1.2, rowSums(age1.2[, c("Freq.x", "Freq.y")]))
 #do the same for other ages


age3.4 <- do.call(rbind,list(age3,age4))
sum(duplicated(age3.4))

sap <- sapply(seq(nrow(age3)), function(i){
  all(age4[i, ] %in% age3[i, ])})
sum(sap)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#--------------------------------------------------------------








#Work in progress
#Reproduce CPUEs on age-level-----------------------------------------
year = 2015
RFA = 7
quarter = 1
species = "Gadus morhua"
#Rprof()
CAFill = fillCA(ca_hh)
cpue = getEstimatesCPUEageOurProcedure(RFA = RFA, species = species, year = year, quarter = quarter,dataHL = hl_hh, dataCA = CAFill,bootstrapProcedure = "simple")
#Rprof(NULL)
#summaryRprof()
#--------------------------------------------------------------












######################################################################

#Estaimating CPUE and mCPUE and Confidence intervals for Length data-----------------------------------------------------------------------------------

######################################################################


#Function For Length Class----------------------------------
#Function computes CPUE for dataType R or C and also mCPUE in a subarea (statrec) in Roundfish area, that is CPUE/no hauls in subarea (statistical rectangle)


testLengA <- function(species, statrec){
  test   <- final[!is.na(final$Species) & final$Species==species & !is.na(final$StatRec) & final$StatRec==statrec,]

  rtypes <- !is.na(test$DataType) & test$DataType=="R"
  test[rtypes,"CPUE_pr_line"] <- test[rtypes,]$Count*60/test[rtypes,]$HaulDur

  ctypes <- !is.na(test$DataType) & test$DataType=="C"
  test[ctypes,"CPUE_pr_line"] <- test[ctypes,]$HLNoAtLngt

  NoHauls <- unique(subset(final[c("haul.id", "LngtCm")], final$StatRec==statrec))

  #some stat rect have zero elements for some species, hence dataframe with zero rows
  if(NROW(test) >0)
    cp <- aggregate(test$CPUE_pr_line ~ test$LngtCm + test$haul.id, FUN= sum)
  else cp <- data.frame(cbind(0,0,0))

  cp1 <- cp[,3]/length(NoHauls)
  cp <- cbind(cp, cp1)

  names(cp) <- c("LngthClas", "haul.id", "CPUE_pr_lngtClas", "mcpue_pr_lngtClas")

  haulinfo <- test[,c("haul.id", "StatRec")]
  haulinfo <- haulinfo[!duplicated(haulinfo$haul.id),]
  return(merge(cp, haulinfo, by.x="haul.id", by.y="haul.id"))
}



# Extracting subarea-------------------------------------------

Iarea.All <- as.data.frame(unique(hl_hh[c("Species", "StatRec", "Roundfish", "LngtCm")]))
nrow(Iarea.All)

#unique(subset(Iarea.All, Roundfish =="2"))

Iarea2 <- subset(Iarea.All,   Roundfish =="2")
nrow(Iarea2)
head(Iarea2)


a3 = as.data.frame(subset(hl_hh, Roundfish == 2 & Year==2017 & Quarter==1, select = c("haul.id" , "StatRec", "LngtCm", "SubFactor",
                                                                             "HLNoAtLngt", "NoMeas","Count", "TotalNo", "DataType", "HaulDur","Species")))


#############################################################################################

# TWO-LEVEL DATAt BOOTSTRAP METHOD (Statrec - with replacement and hauls without replacemnt)---------------------------------------
# Nonparametric bootstrap for hierarchical data - Ren et al 2010

#############################################################################################

B = 100
mCpueArealist = list()

ct <- a3

for (i in seq(1:B)) {

  species = "Gadus morhua"

  ###   sampling with replacement from the clustering variable: sampling frame
  sf <- ct[!duplicated(ct[,c("StatRec", "haul.id")]),c("StatRec", "haul.id")]

  ### sampling with replacement from "cluster"- statistical rectangle. choose random sample size from 1: no.statrec, inclusive
  resample_size <- sample(1:length(unique(sf$StatRec)), size=1)

  cls <- sample(unique(sf$StatRec), size=resample_size, replace=TRUE)
  cls.col <- data.frame(StatRec=cls)

  ### reconstructing the overall simulated cluster sample
  cls.resample <- merge(cls.col, sf, by="StatRec")


  ###sampling hauls in each statrec and choosing 1: no.hauls inclusive as sample size
  d =list()
  j <- 1
  for (rect in cls.col$StatRec){
    rectsample <- sf[sf$StatRec==rect,]
    resample_size <- sample(1:nrow(rectsample), size=1)
    resample_indices <- sample(1:nrow(rectsample), size=resample_size, replace=F)

    ### Estimate for each subsample (hauls from a statistical rectangle), add result to d
    final <- merge(rectsample[resample_indices,], ct, by=c("StatRec", "haul.id"))

    cpsub = as.data.frame(testLengA(species, rect))
    d[j] <- list((cpsub[order(cpsub$LngthClas),]))
    j <- j+1
  }

  d1 <-  plyr::ldply(d, data.frame)
  if(nrow(d1)>0){

    d2 <-  aggregate(cbind(CPUE_pr_lngtClas, mcpue_pr_lngtClas) ~ LngthClas, data=d1, FUN=sum)

    # divid by no.statrec in seletced sample for RF area
    d3 <- cbind(d2, d2$mcpue_pr_lngtClas/length(cls))
    names(d3) <- c("LngthClas", "CPUE_pr_lngtClas", "mcpue_pr_lngtClas", "mcpue_pr_lngt_Roundfish_area")
    d3$i <- i
    mCpueArealist[[i]] <-  d3
  }

}

# TO DO------------------------------------------------------------
# Make sure all length classes are listed in all iterations (or change how ag is calculated below.)
# No observation means 0 catch pr unit effort.


# RESULTS---------------------------------------
RF_area_result =  dplyr::bind_rows(mCpueArealist)

ag <- aggregate(mcpue_pr_lngt_Roundfish_area ~  LngthClas, RF_area_result, function(x) c(mean = sum(x)/B, sd = sd(x), median = median(x), sd.log = sd(log(x)),
                                                                                         CI = quantile(x, c(0.025,0.975)), CI.log = quantile(log(x), c(0.025,0.975))))

#mCPPUE per roundfish area standard error and CI normal and log-bassed estimates

ag1 <- do.call(data.frame, ag)
ag2 = data.frame(ag)


#log-based confidence intervals
LogCI.2.5 <- cbind(exp(log(ag1[,2])  -  1.96*(ag1[,3]/ag1[,2])))
LogCI.9.75 <- cbind(exp(log(ag1[,2]) +  1.96*(ag1[,3]/ag1[,2])))


#Burnham et al log-based confidence interval (Distance sampling pages 115-116)
Co = exp(1.96*sqrt(log(1+ (ag1[,3]/ag1[,2])^2)))
BurLogCI.2.5 <- cbind(ag1[,2]/Co)
BurLogCI.9.75 <- cbind(ag1[,2]*Co)


#includes normal way of confidence interval, log of estimates confidence interval, log-based CI and Burnham log-based CI
ag1 <- do.call(data.frame, cbind(ag, LogCI.2.5, LogCI.9.75, BurLogCI.2.5, BurLogCI.9.75))

names(ag1) <- c("LngtClas", "MEAN.mcpueRF","SD.mcpueRF", "MEDIAN.mcpueRF", "SDLOG.mcpueRF", "CI.2.5%", "CI.97.5%",
                "CIlog.2.5%", "CIlog.97.5%",  "ExpLogCI.2.5%", "ExpLogCI.97.5%","BurLogCI.2.5%", "BurLogCI.97.5%")
ag1










#+++++++++++++++++++++++++++++++++++++++++
# Plots
#example filtering by info in CA or HL

#Pleuronectes platessa = plaice,        Clupea harengus = herring
#Scomber scombrus = atlantic mackerel   Gadus morhua   = atlantic cod
#Merlangius merlangus  = whiting        Trisopterus esmarkii = norway pout
#Melanogrammus aeglefinus = haddock     Sprattus sprattus = sprats
#Pollachius virens = saithe             Scyliorhinus canicula = morgay
#Myxine glutinosa  = hagfish            Galeus melastomus =  blackmouth catshark

#aggregate(b1[,names(b1)!=" mcpue_pr_lngtClas"], by=list(b1$LngthClas), FUN =sum)
#data.frame(unlist(lapply(b, function(x) sum(x))))
#aggregate(b1, by = list(b1$LngthClas, b1$mcpue_pr_lngtClas), sum)

#++++++++++++++++++++++++++++++++++++++++++

#Assumption: 1)length samples are collected from all hauls
#            2)if age samples are collected length is also collected

country.names <- unique(dat$Country)
year.no       <- unique(dat$Year)
roundfish     <- unique(dat$Roundfish[!is.na(dat$Roundfish)])[order(unique(dat$Roundfish[!is.na(dat$Roundfish)]))]
haulsAge      <- length(roundfish)
haulsLength   <- length(roundfish)
quarter       <- unique(dat$Quarter)[order(unique(dat$Quarter))]

yearvec <- c("2004", "2005","2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015","2016", "2017")
RFvec <- c("1", "2","3", "4", "5", "6", "7", "8", "9", "10")
c = 1
y = 14
q = 1
#R = 1
q1 <- c("Q1", "Q3")

windows(height = 19, width = 25.5)
par(mfrow=c(3,4), tcl=-0.9, family="serif", omi=c(0.3,0.2,0.2,0.3)) #, mar = c(5,7,4,2) + 0.1) #, oma=c(4.5, 4, 4, 2.5), mar=rep(.1, 4), cex=1, las=1)



for(R in 1:length(roundfish))
{

  species = "Gadus morhua"

  print(R)

  #Extract the haul.id s you want. Here selection by species--------------------------
  haulselection <- unlist(unique(dat[["HL"]][!is.na(dat[["HL"]]$Species) & dat[["HL"]]$Species==species,"haul.id"]))


  #.. species and age samples-------------------------------
  haulselection_age <- unlist(unique(dat[["CA"]][!is.na(dat[["CA"]]$Species) & dat[["CA"]]$Species==species & !is.na(dat[["CA"]]$Age),"haul.id"]))


  #extract haul id s with age and length for species-----------------------------------
  haulselection_age_length <- unlist(unique(dat[["CA"]][!is.na(dat[["CA"]]$Species) & dat[["CA"]]$Species==species & !is.na(dat[["CA"]]$Age) & !is.na(dat[["CA"]]$LngtCm),"haul.id"]))



  #if all age dont have lengths return warning-------------------------------

  if (!all(haulselection_age %in% haulselection_age_length)){
    warning("Not all age readings have length")
  }


  #.. species and length samples------------------------------------------
  haulselection_length <- unlist(unique(dat[["HL"]][!is.na(dat[["HL"]]$Species) & dat[["HL"]]$Species==species & !is.na(dat[["HL"]]$LngtCm),"haul.id"]))


  #filter dat on haul.id (and country and year) and plot
  country.haul_species <- subset(dat,
                                 #                              Country ==country.names[c],
                                 Roundfish ==roundfish[R],
                                 Year == year.no[y],
                                 Quarter == quarter[q],
                                 haul.id %in% unlist(haulselection))
  plot(country.haul_species)



  country.haul_species_length <- subset(dat,
                                        #                                     Country ==country.names[c],
                                        Roundfish ==roundfish[R],
                                        Year == year.no[y],
                                        Quarter == quarter[q],
                                        haul.id %in% unlist(haulselection_length))
  plot(country.haul_species_length, add=T, col="blue")

  #Number of hauls in the above plot:--------------------------------
  haulsLength[R] <- nrow(country.haul_species_length[["HH"]])



  country.haul_species_age <- subset(dat,
                                     #                                  Country ==country.names[c],
                                     Roundfish ==roundfish[R],
                                     Year == year.no[y],
                                     Quarter == quarter[q],
                                     haul.id %in% unlist(haulselection_age))

  plot(country.haul_species_age, add=T, col="red" )

  #Number of hauls in the above plot:------------------------------------
  haulsAge[R] <-  nrow(country.haul_species_age[["HH"]])



  title(main=paste(yearvec[y], q1[1], ":", species, "in RFA", RFvec[R]), line = 2.5)# , sub = paste(haulsLength[R],  haulsAge[R]), cex.sub=1)
  mtext(paste("HWL =", haulsLength[R], " ", " ",  "HWA =", haulsAge[R]), side=1, line=4, cex =0.6, font = 1)


  #"2017 Q1:"
  ###labels on axes
  labelsZ=parse(text=paste(seq(30,62,1), sep=""))
  axis(side = 4, at = seq(30, 62, by =1), labels =labelsZ)

  axis(side = 3, at = seq(-4, 13, by =1)-0.35, labels=c("E5", "E6", "E7", "E8", "E9", "F0",
                                                        "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "G0","G1", "G2"), tick =FALSE)
  #axis(side = 3, at = seq(-4, 13, by =1), labels=FALSE, tick =TRUE)


  # axis(side = 3, at = seq(-5, 13, by =1))

  ticks=parse(text=paste(abs(seq(-4, 13, 1)), "^o ", sep=""))
  # axis(side = 1, at = seq(-4, 12, 2), labels = ticks)

  labelsY=parse(text=paste(seq(30,62,5), "^o ", sep=""))
  # axis(side = 2, at = seq(50, 62, by =2),  labels =  labelsY)

}

plot_colors <- c("red","blue", "white", "white")
text <- c("Hauls with age and length data","Hauls with length data", "HWL: No. Hauls with length data", "HWA: No. Hauls with age data")
plot.new()
par(xpd=TRUE)
legend("center", legend = text,col=plot_colors, pch=16, cex=1,horiz = FALSE)
par(xpd=FALSE)

dev.print(pdf, file="Gadus morhua1.pdf");
#dev.off()
