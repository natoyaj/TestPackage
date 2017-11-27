library(DATRAS)
library(TestPackage)
library(dplyr)
library(sp)
library(rgeos)
rm(list = ls())

#Read IBTS-data-----------------------------------------
dataDir <<- system.file("Data", package = "TestPackage")
dat<- readExchangeDir(dataDir)
#-------------------------------------------------------

#Extract the data frames from IBTS-data and merge them---
ca <- dat[["CA"]]
hl <- dat[["HL"]]
hh <- dat[["HH"]]

hh_keys <- c("haul.id")
hl_keys <- c(hh_keys, c("LngtClas", "Species")) #
ca_hh    <- merge(ca,hh, by=hh_keys, suffixes=c(".CA", ""))
#ca_hl    <- merge(ca,hl, by=hl_keys, suffixes=c(".CA", ""))
hl_hh    <- merge(hl,hh, by=hh_keys, suffixes=c(".HL", ""))
#ca_hl_hh <- merge(ca, hl_hh, by=hl_keys, suffixes=c(".CA", ""))
#---------------------------------------------------------


#Reproduce CPUEs with C.I.-----------------------------------------

#Choose the time and RFA
year = 2005
RFA = 1
quarter = 1
species = "Gadus morhua"
#Rprof()
cpue = getEstimatesCPUElength(RFA = RFA, species = species, year = year, quarter = quarter,dataHL = hl_hh,bootstrapProcedure = "simple",B = 10)
#Rprof(NULL)
#summaryRprof()
#--------------------------------------------------------------



#Reproduce CPUEs on age-level-----------------------------------------
year = 2005
RFA = 1
quarter = 3
species = "Gadus morhua"
#Rprof()
B = 20

cpueSimple = getEstimatesCPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dataHL = hl_hh, dataCA = ca_hh,bootstrapProcedure = "simple", B = B)
cpueStratified = getEstimatesCPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dataHL = hl_hh, dataCA = ca_hh,bootstrapProcedure = "stratified", B = B)
#Rprof(NULL)
#summaryRprof()
#--------------------------------------------------------------



#Investigate the sensitivity of removing parts of the HL-data and the CA-data---------
year = 2005
RFA = 1
quarter = 3
species= "Gadus morhua"
B = 20 #Number of bootsrap samples in the outer bootstrap (simulating removal of observations)
B2 = 10 #Number of bootsrap samples in the inner bootstrap (simulating P.I. given removal of observations)
BFirst = 3*B #Number of bootsrap samples in the bootstrap for calculating the P.I. of CPUEs for comparison
maxRemove = 0.9 #Max proportion to remove
by = 0.9 #Step in the removal direction

n1 = 7 #Number og age groups
n2 = length(seq(by,maxRemove,by = by)) #number of unique proportions to remove


CA = array(NA,dim= c(n1,4,B2*n2)) #Three dimesional arrays with the simulations of CPUEs
HL = array(NA,dim= c(n1,4,B2*n2))
CAHL = array(NA,dim= c(n1,4,B2*n2))

cpueStratified = getEstimatesCPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dataHL = hl_hh, dataCA = ca_hh,bootstrapProcedure = "stratified", B = BFirst)
neste = 1
for(i in 1:length(seq(by,maxRemove,by = by)))
{
  remove = seq(by,maxRemove,by = by)[i]

  dataCAoriginal = ca_hh[!is.na(ca_hh$Year) & ca_hh$Year == year&
                                  !is.na(ca_hh$Quarter) & ca_hh$Quarter == quarter&
                                  !is.na(ca_hh$Roundfish) & ca_hh$Roundfish == RFA ,]

  dataHLoriginal = hl_hh[!is.na(hl_hh$Year) & hl_hh$Year == year&
                                  !is.na(hl_hh$Quarter) & hl_hh$Quarter == quarter&
                                  !is.na(hl_hh$Roundfish) & hl_hh$Roundfish == RFA ,]

  haulsToSimulateRemovalOf = hh[!is.na(hh$Year) & hh$Year == year&
                                  !is.na(hh$Quarter) & hh$Quarter == quarter&
                                  !is.na(hh$Roundfish) & hh$Roundfish == RFA ,]

  uniqueID = unique(haulsToSimulateRemovalOf$haul.id)
  removeID = rep(FALSE,length(uniqueID))

  for(ii in 1:B2)
  {
    #Simulate HL-data----------------
    q = runif(length(removeID))
    uniqueIDKeep = uniqueID[which(q>remove)]
    dataHL = dataHLoriginal
    keep = rep(FALSE,dim(dataHL)[1])
    for(kk in 1:length(uniqueIDKeep))
    {
      indeks = grepl(uniqueIDKeep[kk],dataHL$haul.id)
      keep[indeks] = TRUE
    }
    dataHL = dataHL[which(keep),]
    #----------------------------------

    #Simulate CA-data------------------
    q = runif(dim(dataCAoriginal)[1])
    dataCA = dataCAoriginal[which(q>remove),]
    #----------------------------------

    #Calculate the CPUE per age estimates with simulated portions of the data-------------
    cpueStratifiedRemovePercentCA = getEstimatesCPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dataHL = dataHLoriginal, dataCA = dataCA,bootstrapProcedure = "stratified", B = B)
    cpueStratifiedRemovePercentHL = getEstimatesCPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dataHL = dataHL, dataCA = dataCAoriginal,bootstrapProcedure = "stratified", B = B)
    cpueStratifiedRemovePercentCAHL = getEstimatesCPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dataHL = dataHL, dataCA = dataCA,bootstrapProcedure = "stratified", B = B)
    #----------------------------------

    #Store the calulations of CPUEs----
    CA[,,neste] = as.matrix(cpueStratifiedRemovePercentCA)
    HL[,,neste] = as.matrix(cpueStratifiedRemovePercentHL)
    CAHL[,,neste] = as.matrix(cpueStratifiedRemovePercentCAHL)
    neste = neste + 1
    #----------------------------------

    print(cpueStratifiedRemovePercentHL)
  }
}
#----------------------------------------------------------------------

#Some plots regarding the senisitivy----------------
par(mfrow=c(2,3))
age = 3

limit = 3
plot(CA[age,limit,])
abline(h = cpueStratified[age,limit])

plot(HL[age,limit,])
abline(h = cpueStratified[age,limit])

plot(CAHL[age,limit,])
abline(h = cpueStratified[age,limit])

limit = 4
plot(CA[age,limit,])
abline(h = cpueStratified[age,limit])

plot(HL[age,limit,])
abline(h = cpueStratified[age,limit])

plot(CAHL[age,limit,])
abline(h = cpueStratified[age,limit])
#----------------------------------------------------------------------











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




