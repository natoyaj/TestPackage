#' CPUErfa
#' @description .
#' @param RFA The roundfish area of interest.
#' @param species The species of interest.
#' @param year The year of interest.
#' @param quarter The quarter of interest.
#' @param bootstrapProcedure The bootstrap procedure ("simple", "stratisfied", ...)
#' @param B The number of simulations in the selected bootstrap procedure
#' @param ALKprocedure Either datras or haulbased
#' @export
#' @return Returns the mCPUE per age class in the given RFA with uncertainty
#' @examples
CPUErfa = function(RFA, species, year, quarter,dat,
                               bootstrapProcedure="datras", B = 10,
                               ALKprocedure = "datras",lengthDivision, useICESindexArea = FALSE){


  #Extract the data of interest-------------
  dataCA = dat$ca_hh[!is.na(dat$ca_hh$Year) & dat$ca_hh$Year == year&
                                 !is.na(dat$ca_hh$Quarter) & dat$ca_hh$Quarter == quarter&
                                 !is.na(dat$ca_hh$Roundfish) & dat$ca_hh$Roundfish == RFA ,]


  dataHL = dat$hl_hh[!is.na(dat$hl_hh$Year) & dat$hl_hh$Year == year&
                                  !is.na(dat$hl_hh$Quarter) & dat$hl_hh$Quarter == quarter&
                                  !is.na(dat$hl_hh$Roundfish) & dat$hl_hh$Roundfish == RFA ,]

  #------------------------------------------

  #Estimate CPUEs----------------------------
  if(ALKprocedure == "haulBased"){
    ALK = calculateALKHaulbased(RFA = RFA, species = species, year = year, quarter = quarter,ca = dataCA, hl = dataHL,lengthDivision = lengthDivision,dat = dat)
    cpueEst = calcmCPUErfaHaulbasedALK(RFA = RFA,species = species, year = year, quarter = quarter, data = dataHL,ALK = ALK, weightStatRec = dat$weightStatRec, useICESindexArea = useICESindexArea)
  }else if(ALKprocedure == "datras"){
    ALK = calculateALKDatras(RFA = RFA, species = species, year = year, quarter = quarter,ca = dataCA,dat = dat,lengthDivision = lengthDivision)
    cpueEst = calcmCPUErfaAreaBasedALK(RFA = RFA,species = species, year = year, quarter = quarter, data = dataHL,ALK = ALK,weightStatRec = dat$weightStatRec, useICESindexArea = useICESindexArea)
  }else{
    stop("Unkown ALKprocedure")
  }
  #------------------------------------------



  #Investigate if it is observed zero data---
  tmp = dataCA[!is.na(dataCA$Roundfish) & dataCA$Roundfish == RFA&
                               !is.na(dataCA$Species) & dataCA$Species == species,]
  if(sum(!is.na(tmp))==0){
    return(data.frame(cpueEst))
  }
  #------------------------------------------

  #Construct a data.frame with estimates of CPUEs per age----
  cpue = data.frame(cpueEst)
  names(cpue) = "mCPUE"

  #Attach some attributes for internal investigation----
  nWithDatras = attributes(cpueEst)$nWithDatras
  nWithoutDatras = attributes(cpueEst)$nWithoutDatras
  nFoundWithin = attributes(cpueEst)$nFoundWithin
  nNotFoundWithin = attributes(cpueEst)$nNotFoundWithin
  attributes(cpue)$nWithDatras = nWithDatras
  attributes(cpue)$nWithoutDatras = nWithoutDatras
  attributes(cpue)$nFoundWithin = nFoundWithin
  attributes(cpue)$nNotFoundWithin = nNotFoundWithin
  #------------------------------------------

  return(cpue)
}





#' calcCPUEwholeNorthSea
#' @description
#' @export
#' @return Returns the mCPUE per age class in the whole North Sea
#' @examples
CPUEnorthSea = function(species, year, quarter,dat, bootstrapProcedure,
                        B = 10, ALKprocedure = "",doBootstrap = TRUE,
                        onlySimulate = FALSE,lengthDivision =1:150,samplesWithinEachIntervall = NULL,nSimHauls = NULL, useICESindexArea = FALSE){

  #Defines the matrix with cpue to be returned--------------------
  maxAge = confALK(species = species, quarter = quarter)$maxAge
  mCPUE = matrix(0,maxAge+1,B+1)
  nOtolithsTotal = dim(dat$ca_hh)[1]
  nOtolithsRemoved = 0
  #---------------------------------------------------------------

  #Help variable for investigating how larg proportion og ages are calculated with different procedures
  nWithDatras = 0
  nWithoutDatras = 0
  nFoundWithin = 0
  nNotFoundWithin = 0
  nFoundWithinAllData = 0
  nNotFoundWithinAllData = 0
  nWithDatrasAllData = 0
  nWithoutDatrasAllData = 0


  #Calcualte the mCPUE for each RFA and calcualtes scaled average w.r.t. area----------------
  if(!onlySimulate){
  tmp = calcmCPUEnorthSea(species= species,year =year, quarter = quarter,
                          dat = dat,ALKprocedure = ALKprocedure,B = B,
                          dimCPUE = dim(mCPUE),lengthDivision = lengthDivision,useICESindexArea = useICESindexArea)
  mCPUE[,1] = tmp[[1]]
  nFoundWithinAllData = attributes(tmp[[1]])$nFoundWithin
  nNotFoundWithinAllData = attributes(tmp[[1]])$nNotFoundWithin
  nWithDatrasAllData = attributes(tmp[[1]])$nWithDatras
  nWithoutDatrasAllData = attributes(tmp[[1]])$nWithoutDatras
  #-----------------------------------------------------------------------------------------
  }

  #Simulate data and calculates mCPUE for estimation of unceratinaty------------------------
  if(doBootstrap){
    for(i in 1:B){
      CA = dat$ca_hh[1,] #This line in data frame is removed later, but introduced for convenience here
      HL = dat$hl_hh[1,]
      HH = dat$hh[1,]
      HH$originalIdAtThisLocation = 0; #Removed later

      for(RFA in 1:9){
        if(dim(dat$hh[which(dat$hh$Roundfish==RFA & !is.na(dat$hh$haul.id)),])[1]>0){
          loc = findLoc(dat=dat,quarter=quarter,year = year,RFA = RFA)#Find the closest nabour (used in bootstrap procedure when only one haul is present in a statistical rectangle)
          if(bootstrapProcedure =="datras"){
            simHauls = simTrawlHaulsDatras(RFA,year,quarter, hl_hh = dat$hl_hh, ca_hh = dat$ca_hh)
            simDataHL = simHauls$hl_hh
            simDataCA = simCAdatras(RFA,year,quarter, data = simHauls$ca_hh, species = species)
          }else if(bootstrapProcedure =="datrasHLstratifiedCA"){
            nSimHaulsThisRFA = round(mean(dat$hh$Roundfish[!is.na(dat$hh$Roundfish)]==RFA)*nSimHauls)
            if(nSimHaulsThisRFA<2){
              nSimHaulsThisRFA = 2
              warning(paste("In RFA ", RFA , " it is sampled ", nSimHaulsThisRFA , " hauls instead of fewer"))
            }
            simHauls = simTrawlHaulsDatras(RFA,year,quarter, hl_hh = dat$hl_hh, ca_hh = dat$ca_hh,nSimHauls = nSimHaulsThisRFA)
            simDataHL = simHauls$hl_hh
            simDataCA = simHauls$ca_hh #CA data is later sampled with the pseaudo bootstrap procedure
          }else if(bootstrapProcedure =="stratifiedHLandCA" | bootstrapProcedure =="stratifiedHLdatrasCA"){
            simHauls = simHaulsStratified(RFA,year,quarter, dataHH = dat$hh,loc = loc)
            simDataCA = dat$ca_hh[1,]#Define the structure in the data, this row is removed later.
            simDataHL = dat$hl_hh[1,]
            simDataHH = dat$hh[1,]
            simDataHH$originalIdAtThisLocation = 0;
            for(j in 1:dim(simHauls)[1]){ #Go trough the simulated hauls and modify them such that they points to the right statistical rectangle, coordinates, etc.
              tmpCA = dat$ca_hh[which(dat$ca_hh$haul.id== simHauls$haul.id[j]),]
              tmpHL = dat$hl_hh[which(dat$hl_hh$haul.id== simHauls$haul.id[j]),]
              tmpHH = dat$hh[which(dat$hh$haul.id== simHauls$haul.id[j]),]

              if(dim(tmpCA)[1]>0){
                tmpCA$StatRec = simHauls$StatRec[j] #statrec may have changed if less than one haul in the statrec
                tmpCA$lon = simHauls$lon[j] #Needed for a model based approach
                tmpCA$lat = simHauls$lat[j]
                tmpCA$haul.id = paste(simHauls$haul.id[j],j,sep = "") #Set an unique ID to the haul
                simDataCA = rbind(simDataCA,tmpCA)
              }
              if(dim(tmpHL)[1]>0){
                tmpHL$StatRec = simHauls$StatRec[j]
                tmpHL$lon = simHauls$lon[j]
                tmpHL$lat = simHauls$lat[j]
                tmpHL$haul.id = paste(simHauls$haul.id[j],j,sep = "")
                simDataHL = rbind(simDataHL,tmpHL)
              }
              if(dim(tmpHH)[1]>0){
                tmpHH$StatRec = simHauls$StatRec[j]
                tmpHH$lon = simHauls$lon[j]
                tmpHH$lat = simHauls$lat[j]
                tmpHH$haul.id = paste(simHauls$haul.id[j],j,sep = "")
                tmpHH$originalIdAtThisLocation = simHauls$originalIdAtThisLocation[j]
                simDataHH = rbind(simDataHH,tmpHH)
              }
            }
            simDataCA = simDataCA[-1,]#Removes the first line which was created for defining the structure of the data
            simDataHL = simDataHL[-1,]
            simDataHH = simDataHH[-1,]
            HH = rbind(HH,simDataHH)

            if(bootstrapProcedure =="stratifiedHLdatrasCA"){
              #Sample ages with DATRAS-procedure
              simDataCA = simCAdatras(RFA,year,quarter, data = dat$ca_hh, species = species)
            }else{
              #Sample ages with the pseeudo bootstrap procedure, this is done after the for-loop.
            }
          }else{
            return("Select a valid bootstrap procedure.")
          }
          CA = rbind(CA,simDataCA)
          HL = rbind(HL,simDataHL)
        }
      }

      CA = CA[-1,]#Removes the first line which was created for defining the structure of the data
      HL = HL[-1,]
      datTmp = dat
      datTmp$ca_hh = CA
      datTmp$hl_hh = HL

      if(bootstrapProcedure =="stratifiedHLandCA"| bootstrapProcedure =="stratifiedHLdatrasCA"){
        HH = HH[-1,]
        datTmp$hh = HH
      }


      #Sample ages within hauls-----------------
      print("Sampling otoliths...")
      nOtolithsTotal = dim(datTmp$ca_hh)[1]
      if(bootstrapProcedure =="stratifiedHLandCA" | bootstrapProcedure =="datrasHLstratifiedCA"){
          datTmp$ca_hh = sampleCA(ca_hh = datTmp$ca_hh,species,
                                  quarter, lengthDivision = lengthDivision,samplesWithinEachIntervall = samplesWithinEachIntervall,
                                  hl_hh = datTmp$hl_hh)
      }else{
        #Sample ages stratified with wrt length, was done in the for-loop above.
      }
      nOtolithsRemoved = nOtolithsTotal - dim(datTmp$ca_hh)[1]
      print(paste("Removed ",nOtolithsRemoved, " out of ", nOtolithsTotal," otoliths.",sep = ""))
      #------------------------------------------

      mCPUEThisSimulation = calcmCPUEnorthSea(species= species,year =year, quarter = quarter,
                              dat = datTmp,ALKprocedure = ALKprocedure,B = B,
                              dimCPUE = dim(mCPUE),lengthDivision = lengthDivision, useICESindexArea = useICESindexArea)
      if(is.na(sum(mCPUEThisSimulation[[1]]))){
        print("Somethings wrong, save the simulated data in the working directory and the program will soon terminate...")
        saveRDS(datTmp,file = paste("dataResultedInNAcpueDF",lengthDivision[2]-lengthDivision[1],sep = ""))
      }
      mCPUE[,i+1] = mCPUEThisSimulation[[1]]
      nWithDatras = attributes(mCPUEThisSimulation[[1]])$nWithDatras
      nWithoutDatras = attributes(mCPUEThisSimulation[[1]])$nWithoutDatras
      nFoundWithin = attributes(mCPUEThisSimulation[[1]])$nFoundWithin
      nNotFoundWithin = attributes(mCPUEThisSimulation[[1]])$nNotFoundWithin

    }
  }
  #-----------------------------------------------------------------------------------------


  #Define a summary that we return (mCPUE, quantiles and sd)--------------------------------
  mCPUEsummary = data.frame(mCPUE[,1],mCPUE[,1],mCPUE[,1], mCPUE[,1],mCPUE[,1],mCPUE[,1],mCPUE[,1],mCPUE[,1])
  names(mCPUEsummary) = c("mCPUE","bootstrapMean","median", "Q025","Q975","BiasCQ025","BiasCQ075", "sd")

  if(onlySimulate){
    mCPUEsummary$mCPUE = mCPUE[,2]
  }

  for(i in 1:dim(mCPUEsummary)[1]){
    mCPUEsummary$bootstrapMean[i] = mean(mCPUE[i,2:(B+1)])
    mCPUEsummary$median[i] = median(mCPUE[i,2:(B+1)])
    quantile = quantile(mCPUE[i,2:(B+1)],c(0.025,0.975))
    mCPUEsummary$Q025[i] = quantile[1]
    mCPUEsummary$Q975[i] = quantile[2]
    mCPUEsummary$sd[i] = sd(mCPUE[i,2:(B+1)])

    # bias corrected confidence intervals (b is not correct)
    b= qnorm((sum(mCPUE[i,2:(B+1)] > mCPUE[i,1])+ sum(mCPUE[i,2:(B+1)]==mCPUE[i,1])/2)/length(mCPUE[i,2:(B+1)]))

    alph      = 0.05                              # 95% limits
    z         = qnorm(c(alph/2,1-alph/2))         # Std. norm. limits
    p         = pnorm(z-2*b)                      # bias-correct & convert to proportions
    qq        = quantile(mCPUE[i,2:(B+1)],p=p)    # Bias-corrected percentile lims.
    mCPUEsummary$BiasCQ025[i] = qq[1]
    mCPUEsummary$BiasCQ075[i] = qq[2]

  }
  #-----------------------------------------------------------------------------------------

  attributes(mCPUEsummary)$nWithDatras = round(nWithDatras) #May not be an integer because of data type C
  attributes(mCPUEsummary)$nWithoutDatras = round(nWithoutDatras)
  attributes(mCPUEsummary)$nFoundWithin = round(nFoundWithin)
  attributes(mCPUEsummary)$nNotFoundWithin = round(nNotFoundWithin)
  attributes(mCPUEsummary)$nOtolithsRemoved = round(nOtolithsRemoved)
  attributes(mCPUEsummary)$nOtolithsTotal = round(nOtolithsTotal)
  attributes(mCPUEsummary)$nFoundWithinAllData = round(nFoundWithinAllData)
  attributes(mCPUEsummary)$nNotFoundWithinAllData = round(nNotFoundWithinAllData)
  attributes(mCPUEsummary)$nWithDatrasAllData = round(nWithDatrasAllData)
  attributes(mCPUEsummary)$nWithoutDatrasAllData = round(nWithoutDatrasAllData)
  return(mCPUEsummary)
}


