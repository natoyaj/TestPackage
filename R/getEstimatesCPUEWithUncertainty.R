#' getEstimateCPUElength
#' @description .
#' @param RFA The roundfish area of interest.
#' @param species The species of interest.
#' @param year The year of interest.
#' @param quarter The quarter of interest.
#' @param dataHL datras-data with length
#' @param percentOfAreaRepresentative the percentage of the statical recangle within sea depth intervall
#' @param bootstrapProcedure The bootstrap procedure ("simple", ...)
#' @param B The number of simulations in the selected bootstrap procedure
#' @export
#' @return Returns the estimated mCPUE per length class in the given RFA with uncertainty
#' @examples
getEstimatesCPUElength = function(RFA, species, year, quarter,dataHL, percentOfAreaRepresentative = NULL, bootstrapProcedure="simple", B = 10)
{
  dataToSimulateFrom = dataHL[!is.na(dataHL$Year) & dataHL$Year == year&
                                !is.na(dataHL$Quarter) & dataHL$Quarter == quarter&
                                !is.na(dataHL$Roundfish) & dataHL$Roundfish == RFA ,]

  #Estimate CPUEs with uncertainty
  cpueEst = calcmCPUErfa(RFA = RFA, species = species, year = year, quarter = quarter, data = dataToSimulateFrom)
  simCPUEs = matrix(NA,length(cpueEst),B)
  for(i in 1:B)
  {
    if(bootstrapProcedure=="simple")
    {
      data = simTrawlHaulsHLSimple(RFA,year,quarter, data = dataToSimulateFrom)
    }else{
      return("Select a valid bootstrap procedure.")
    }
    sim = calcmCPUErfa(RFA = RFA, species = species, year = year, quarter = quarter, data = data)

    if(length(sim)!= dim(simCPUEs)[1]) sim = c(sim,rep(0, dim(simCPUEs)[1] - length(sim))) #TODO: define the length classes and include them such that we can remove this line.

    simCPUEs[,i] = sim
    print(i)
  }

  #Construct a data.frame with estimates and C.I.
  cpue = data.frame(cpueEst)
  cpue$lQ = rep(0,length(cpueEst))
  cpue$uQ = rep(0,length(cpueEst))
  for(i in 1:length(cpueEst))
  {
    quantile = quantile(simCPUEs[i,],c(0.025,0.975))
    cpue$lQ[i] = quantile[1]
    cpue$uQ[i] = quantile[2]
  }
  return(cpue)
}




#' getEstimateCPUEage
#' @description .
#' @param RFA The roundfish area of interest.
#' @param species The species of interest.
#' @param year The year of interest.
#' @param quarter The quarter of interest.
#' @param dataHL datras-data with length
#' @param dataCA datras-data with age
#' @param percentOfAreaRepresentative the percentage of the statical recangle within sea depth intervall
#' @param bootstrapProcedure The bootstrap procedure ("simple", "stratisfied", ...)
#' @param B The number of simulations in the selected bootstrap procedure
#' @export
#' @return Returns the mCPUE per age class in the given RFA with uncertainty
#' @examples
getEstimatesCPUEage = function(RFA, species, year, quarter,dataHL,dataCA, percentOfAreaRepresentative = NULL, bootstrapProcedure="simple", B = 10)
{

  #Extract the data of interest
  dataToSimulateFromCA = dataCA[!is.na(dataCA$Year) & dataCA$Year == year&
                                 !is.na(dataCA$Quarter) & dataCA$Quarter == quarter&
                                 !is.na(dataCA$Roundfish) & dataCA$Roundfish == RFA ,]
  dataToSimulateFromHL = dataHL[!is.na(dataHL$Year) & dataHL$Year == year&
                                  !is.na(dataHL$Quarter) & dataHL$Quarter == quarter&
                                  !is.na(dataHL$Roundfish) & dataHL$Roundfish == RFA ,]

  #Estimate CPUEs with uncertainty
  ALK = calculateALK(RFA = RFA, species = species, year = year, quarter = quarter,data = dataToSimulateFromCA)
  cpueEst = calcmCPUErfaWithALK(RFA = RFA,species = species, year = year, quarter = quarter, data = dataToSimulateFromHL,ALK = ALK)

  simCPUEs = matrix(NA,length(cpueEst),B)
  for(i in 1:B)
  {
    if(bootstrapProcedure =="simple")
    {
      simDataCA = simTrawlHaulsCASimple(RFA,year,quarter, data = dataToSimulateFromCA)
      simDataHL = simTrawlHaulsHLSimple(RFA,year,quarter, data = dataToSimulateFromHL)
    }else if(bootstrapProcedure =="stratified"){
      simDataCA = simTrawlHaulsCAStratified(RFA,year,quarter, data = dataToSimulateFromCA)
      simDataHL = simTrawlHaulsHLStratified(RFA,year,quarter, data = dataToSimulateFromHL)
    }else{
      return("Select a valid bootstrap procedure.")
    }

    simALK = calculateALK(RFA = RFA, species = species, year = year, quarter = quarter,data = simDataCA)
    sim = calcmCPUErfaWithALK(RFA = RFA, species = species, year = year, quarter = quarter, data = simDataHL,ALK = simALK)

    simCPUEs[,i] = sim
    print(i)
  }

  #Construct a data.frame with estimates and C.I.
  cpue = data.frame(cpueEst)
  cpue$bootstrapMean = rep(0,length(cpueEst))
  cpue$lQ = rep(0,length(cpueEst))
  cpue$uQ = rep(0,length(cpueEst))
  for(i in 1:length(cpueEst))
  {
    quantile = quantile(simCPUEs[i,],c(0.025,0.975))
    cpue$lQ[i] = quantile[1]
    cpue$uQ[i] = quantile[2]
    cpue$bootstrapMean[i] = mean(simCPUEs[i,])

  }
  return(cpue)
}




