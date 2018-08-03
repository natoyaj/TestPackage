



#' calculateALKNew
#' @description
#' @param RFA Roundfish area number.
#' @param species The species of interest.
#' @param year The year which the ALKs are calculated.
#' @param quarter The quarter of the year which the ALKs are calculated.
#' @param data The CA needed for calculating the ALKs.
#' @param data_hl The HL needed for calculating the ALKs (since there can be trawl hauls without age information).
#' @param dfLength The length of the pooled length class. Default is 1, e.g. 1 length classes in each pooled length class.
#' @export
#' @return Returns a list with ALK for each trawl haul
#' @examples
calculateALKNew = function(RFA, species, year, quarter,data,data_hl,dfLength = 1){

  #Define the list which shall be filled with the ALKs and returned-----
  alkToReturn = list()
  #----------------------------------------------------

  #Extract the data of interest----------------------
  caInterest = data[which(data$Roundfish==RFA & data$Year==year &
                            data$Quarter == quarter & data$Species == species),]

  caInterest = caInterest[which(!is.na(caInterest$Age) & !is.na(caInterest$LngtCm)),]

  hlInterest = data_hl[!is.na(data_hl$Year) & data_hl$Year == year&
                         !is.na(data_hl$Quarter) & data_hl$Quarter == quarter&
                         !is.na(data_hl$Roundfish) & data_hl$Roundfish == RFA ,]

  hlInterest = hlInterest[which(!is.na(hlInterest$LngtCm)),]
  hlInterest = hlInterest[which(!is.na(hlInterest$Species)),]
  #---------------------------------------------------


  #Find the configurations needed for the ALK---------
  conf = confALK(species = species,quarter = quarter)
  maxAge = conf$maxAge
  minLength = conf$minLength
  maxLength = conf$maxLength
  lengthClassIntervallLengths = conf$lengthClassIntervallLengths
  #----------------------------------------------------

  #Create the sceleton of the ALK----------------------
  alk = matrix(0,(maxLength-minLength)/lengthClassIntervallLengths +1, maxAge+3)
  alk[,2] = seq(minLength,maxLength,by = lengthClassIntervallLengths)
  #----------------------------------------------------

  #Investigate if zero data, if so return the sceleton-
  if(dim(caInterest)[1]==0){
    idHaul = unique(c(as.character(hlInterest$haul.id),as.character(caInterest$haul.id)))
    neste=1
    for(id in idHaul){
      idTmp = as.character(id)
      alkThis = as.data.frame(alk)
      names(alkThis) = c("ID","Length","0","1","2","3","4","5","6")
      alkThis$ID[1] = idTmp
      alkToReturn[[neste]] = alkThis
      neste = neste+1
    }
    warning(paste("No observations in period given in RFA: " ,RFA,sep = ""))
    return(alkToReturn)
  }
  #----------------------------------------------------

  #Find distance between trawl locations--------------
  id1 = as.character(caInterest$haul.id)
  id2 = as.character(hlInterest$haul.id)
  uniqueId = unique(c(id1,id2)) #Need ALK for every trawl haul. TODO: should use hh-data instead.
  loc = data.frame(uniqueId)
  loc$lat = rep(-999,dim(loc)[1])
  loc$lon = rep(-999,dim(loc)[1])

  for(i in 1:length(uniqueId))
  {
    id = uniqueId[i]
    indeks = which(caInterest$haul.id== id)[1]
    if(!is.na(indeks)){
      loc$lat[i] = caInterest$lat[indeks]
      loc$lon[i] = caInterest$lon[indeks]
    }else{
      indeks = which(hlInterest$haul.id== id)[1]
      loc$lat[i] = hlInterest$lat[indeks]
      loc$lon[i] = hlInterest$lon[indeks]
    }

  }
  coordinates(loc) <- ~lon+lat
  proj4string(loc) ="+proj=longlat"
  d = spDists(loc)
  #-----------------------------------------------------


  #Set old fish to belong to the pluss group-----------
  caInterest$Age[caInterest$Age > maxAge] = maxAge
  #----------------------------------------------------


  #Construct each element of the ALK-list----------------------------------------------------------------------
  print("We are now calculating the haul based ALK, this takes some time due to that there are many missing ages")
  haulId = uniqueId
  neste = 1
  ALKnormal = calculateALK(RFA = RFA, species = species, year = year, quarter = quarter,data = caInterest)

  for(id in haulId){

    #Extract which lengts that are of interest (i.e. observed in HL-data by this trawl), it is time consuming to calculate the ALK for those not observed. Note that we also check CA-data, that is because a few times a fish is meshured by 10.9 in CA and 11 in HL.
    whichLengtsAreInteresting = unique(c(hlInterest$LngtCm[hlInterest$haul.id==id & hlInterest$Species==species],
                                         caInterest$LngtCm[caInterest$haul.id==id & caInterest$Species==species]))



    if(species=="Gadus morhua" | species=="Pollachius virens"){
      whichLengtsAreInteresting = unique(floor(whichLengtsAreInteresting))
    }
    if(length(whichLengtsAreInteresting)>0){
      if(min(whichLengtsAreInteresting)<minLength){
        whichLengtsAreInteresting = c(whichLengtsAreInteresting,minLength)
        whichLengtsAreInteresting = whichLengtsAreInteresting[-which(whichLengtsAreInteresting<minLength)]
      }
      if(max(whichLengtsAreInteresting)>maxLength){
        whichLengtsAreInteresting = c(whichLengtsAreInteresting,maxLength)
        whichLengtsAreInteresting = whichLengtsAreInteresting[-which(whichLengtsAreInteresting>maxLength)]

      }
    }
    whichLengtsAreInteresting = unique(whichLengtsAreInteresting)
    whichIsMissing = rep(TRUE, dim(alk)[1])


    #Construct the parts of the ALK were we have data--------------------
    if(species=="Gadus morhua" | species=="Pollachius virens")
    {
      idTmp = as.character(id)
      alkThis = as.data.frame(alk)
      names(alkThis) = c("ID","Length","0","1","2","3","4","5","6")
      alkThis$ID[1] = idTmp

      dataThisTrawl = caInterest[caInterest$haul.id == id,]

      if(dim(dataThisTrawl)[1]>0){
        for(i in 1:dim(dataThisTrawl)[1])
        {
          if(floor(dataThisTrawl$LngtCm[i])< (minLength+dfLength))
          {
            alkThis[1,floor(dataThisTrawl$Age[i])+3] =
              alkThis[1,floor(dataThisTrawl$Age[i])+3] +dataThisTrawl$NoAtALK[i]
            whichIsMissing[1] = FALSE
          }else if(floor(dataThisTrawl$LngtCm[i])>= maxLength)
          {
            alkThis[dim(alkThis)[1],floor(dataThisTrawl$Age[i])+3] =
              alkThis[dim(alkThis)[1],floor(dataThisTrawl$Age[i])+3] +dataThisTrawl$NoAtALK[i]
            whichIsMissing[dim(alkThis)[1]] = FALSE

          }else{
            hvilke = min(which(alkThis[,2]> floor(dataThisTrawl$LngtCm[i]-dfLength)))

            alkThis[hvilke,floor(dataThisTrawl$Age[i])+3] =
              alkThis[hvilke,floor(dataThisTrawl$Age[i])+3] +dataThisTrawl$NoAtALK[i]
            whichIsMissing[hvilke] = FALSE

          }
        }
      }
    }
    #------------------------------------------------------


    #Extrapolate the ALK to length calsses were we do not have data-----------------------------------
    #    for(i in 1:dim(alkThis)[1])
    #    {
    #     if(sum(alkThis[i,-c(1,2)]) == 0)whichIsMissing[i] = TRUE

    #  if(!is.element(alkThis$Length[i], whichLengtsAreInteresting)) whichIsMissing[i] = FALSE
    #    }
    if(length(whichLengtsAreInteresting)==0){
      whichIsMissing = rep(FALSE, dim(alkThis)[1])
    }else{
      tmpIndeks = 1:dim(alkThis)[1]
      tmpIndeks = tmpIndeks[-(whichLengtsAreInteresting-alkThis$Length[1]+1)]
      whichIsMissing[tmpIndeks] = FALSE
    }




    #Routine for filling the not observed length classes
    if(quarter ==1)start = 3
    if(quarter >1)start = 2


    whichIsMissing2 = whichIsMissing

    for(i in 1:dim(alkThis)[1])
    {
      if(whichIsMissing[i])
      {
        sortedShortestDist = order(d[,which(loc$uniqueId== id)[1]])[-1]

        closestSorted = haulId[sortedShortestDist]
        foundAge = FALSE
        nesteHal = 1
        closesId = closestSorted[nesteHal]
        while(!foundAge){
          closestData = caInterest[caInterest$haul.id == closesId,]
          if(i==dim(alkThis)[1]){
            hvilke = which(closestData$LngtCm >= alkThis[i,2])
          }else if(i==1){
            hvilke = which(closestData$LngtCm < alkThis[2,2])
          }else if(i< dim(alkThis)[1]){
            hvilke = which(closestData$LngtCm >= alkThis[i,2] & closestData$LngtCm < alkThis[i+1,2] )
          }

          if(length(hvilke)>0)
          {
            row = rep(0,maxAge+1)
            for(l in hvilke)
            {
              row[closestData$Age[l]+1] = row[closestData$Age[l]+1] +closestData$NoAtALK[l]
            }
            alkThis[i,3:dim(alkThis)[2]] = row
            foundAge = TRUE
            whichIsMissing2[i] = FALSE
          }else{
            nesteHal = nesteHal+1
            if(nesteHal>length(closestSorted)){#We have now looked in all hauls in the RFA
              foundAge = TRUE#The age will be filled with datars-procedure
              nesteHal = 1
            }
            closesId = closestSorted[nesteHal]
            #Did not information in this trawl haul, go to next trawl haul.
          }
        }
      }
    }
    #------------------------------------------------------

    #Check if there are zero observed ages in the length class close by.
    #In that case we fill them as datras suggest-----------------------------------
    #    whichIsMissing2 = rep(FALSE, dim(alkThis)[1])
    #    for(i in 1:dim(alkThis)[1])
    #    {
    #      if(sum(alkThis[i,-c(1,2)]) == 0)whichIsMissing2[i] = TRUE
    #
    #      if(!is.element(alkThis$Length[i], whichLengtsAreInteresting)) whichIsMissing2[i] = FALSE
    #
    #    }


    #Set those we do not find any age  equal the original ALK from datras, this works only if dfLength = 1
    alkThis[whichIsMissing2,2:(maxAge+3)] = ALKnormal[whichIsMissing2,]

    #Store the ALK for this trawl haul in the list to be returned
    alkToReturn[[neste]] = alkThis
    neste = neste+1
  }

  #------------------------------------------------------------------------------------------------------------------------

  #Return the list with ALKs------
  return(alkToReturn)
  #-------------------------------
}




ids = unique(tmp$haul.id)
fangst = matrix(NA,length(ids),200)
for(i in 1:length(ids)){
  tmp2 = tmp[which(tmp$haul.id==ids[i]),]
  for(j in 1:200){
    fangst[i,j] = sum(tmp2$HLNoAtLngt[floor(tmp2$LngtCm)==j] * )
  }
}



if(dataWithTheSpecies$DataType[i]=="R")
{
  CPUE =  CPUE + (dataWithTheSpecies$Count[i]*60/dataWithTheSpecies$HaulDur[i])*subfactor[i] * ALK[lineInAlkToUse,-c(1,2)]/sum(ALK[lineInAlkToUse,-c(1,2)])
}else if(dataWithTheSpecies$DataType[i]=="C")
{
  CPUE  =  CPUE + dataWithTheSpecies$HLNoAtLngt[i]*subfactor[i] * ALK[lineInAlkToUse,-c(1,2)]/sum(ALK[lineInAlkToUse,-c(1,2)])
}



if(bootstrapProcedure =="stratifiedHLdatrasCA"| bootstrapProcedure =="stratifiedHLandCA"){
  #Find shortest distance to a neigbour trawl location---
  uniqueId = unique(dataToSimulateFromHL$haul.id)
  loc = data.frame(uniqueId)
  loc$lat = rep(-999,dim(loc)[1])
  loc$lon = rep(-999,dim(loc)[1])

  for(i in 1:length(uniqueId))
  {
    id = uniqueId[i]
    indeks = which(dataToSimulateFromHL$haul.id== id)[1]
    loc$lat[i] = dataToSimulateFromHL$lat[indeks]
    loc$lon[i] = dataToSimulateFromHL$lon[indeks]
  }

  coordinates(loc) <- ~lon+lat
  proj4string(loc) ="+proj=longlat"
  d = spDists(loc)
  min.d <- apply(d, 1, function(x) order(x, decreasing=F)[2])
  loc$shortesDist = uniqueId[min.d]
  #-----------------------------------------------------
}
if(FALSE){ #Olav suggest to remove this part. This part extrapolates ages of no observed similar as the datras procedure, but on haul level.
  #Set the smallest length groops to age 0 or 1 if there are no observations of them
  first = which(!whichIsMissing2)[1]
  if(!is.na(first) &first>1)
  {
    if(quarter==1)
    {
      alkThis[1:(first-1),4] = 1
    }else if(quarter>1)
    {
      alkThis[1:(first-1),3] = 1
    }
    whichIsMissing2[1:first] = FALSE
  }


  distToNext = which(!whichIsMissing2)[1]
  distToPrevious = 99999999
  nextValue = NA

  if(quarter ==1)start = 3
  if(quarter >1)start = 2

  for(j in start:dim(alkThis)[2])
  {
    for(i in 1:dim(alkThis)[1])
    {
      if(whichIsMissing2[i])
      {
        if(distToPrevious<distToNext)
        {
          alkThis[i,j]= alkThis[i-1,j]
        }else if(distToPrevious == distToNext)
        {
          alkThis[i,j]= (alkThis[i-1,j] + nextValue)/2
        }else if(distToPrevious > distToNext)
        {
          alkThis[i,j]= nextValue
        }
        distToNext  = distToNext -1
        distToPrevious =distToPrevious +1

      }else{
        distToPrevious = 1
        distToNext = which(!whichIsMissing2[i:length(whichIsMissing2)])[2]-2
        if(is.na(distToNext))
        {
          distToNext = 999999999
          nextValue = -999999999
        }else{
          nextValue = alkThis[i + distToNext + 1,j]
        }
      }
    }
    #------------------------------------------------------
  }
}
#--------------------------------------------------------------------------------------------






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

bootstrapProcedure = "hiearchical"
cpueHierachical = getEstimatesCPUEage(RFA = RFA, species = species, year = year, quarter = quarter,dataHL = hl_hh, dataCA = ca_hh,
                                      bootstrapProcedure = bootstrapProcedure, B = 40)

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
