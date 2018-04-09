#This R-file consist of the functions which are doing the simulations in the bootstrap

#' simTrawlHaulsHL
#' @description Simulates trawl hauls with only length information used in the bootstrap procedure.
#' @param RFA Roundfish area number.
#' @param year The year of interest.
#' @param quarter The quarter of interest.
#' @export
#' @return Returns simulations of the dataras-data with length information on a similar format as the data used in the functions for calculating the CPUEs
#' @examples
simTrawlHaulsHL = function(RFA,year, quarter)
{
  #TODO
}



#' simTrawlHaulsCA
#' @description Simulates trawl hauls with both length and age information used in the bootstrap procedure.
#' @param RFA Roundfish area number.
#' @param year The year of interest.
#' @param quarter The quarter of interest.
#' @export
#' @return Returns simulations of the dataras-data with both length and age information on a similar format as the data used in the functions for calculating the CPUEs
#' @examples
simTrawlHaulsCA = function(RFA,year, quarter)
{
  #TODO
}



#' simTrawlHaulsHLSimple
#' @description Simulates trawl hauls with only length information used in the bootstrap procedure.
#' @param RFA Roundfish area number.
#' @param year The year of interest.
#' @param quarter The quarter of interest.
#' @export
#' @return Returns simulations of the dataras-data with length information on a similar format as the data used in the functions for calculating the CPUEs
#' @examples
simTrawlHaulsHLSimple = function(RFA,year, quarter,data)
{
  #Extract the data of interest-------------------------
  dataOfInterest = data[!is.na(data$Year) & data$Year == year&
                          !is.na(data$Quarter) & data$Quarter == quarter&
                          !is.na(data$Roundfish) & data$Roundfish == RFA ,]
  #-----------------------------------------------------

  #Simulate trawl hauls---------------------------------
  haulsID = unique(dataOfInterest$haul.id)
  nSim = length(haulsID)
  simHauls = sample(haulsID,nSim,replace = T)

  simData = list(NULL)

  for(i in 1:nSim)
  {
    simData[[i]]= dataOfInterest[dataOfInterest$haul.id==simHauls[i],]
    simData[[i]]$haul.id = paste(simData[[i]]$haul.id,i) #Needs unique haul.id, which is achived here.
  }
  simDataToBeReturned  =   do.call(rbind.data.frame,simData)
  #----------------------------------------------------

  return(simDataToBeReturned)
}


#' simTrawlHaulsHLdatras
#' @description Simulates trawl hauls with only length information used in the bootstrap procedure.
#' Keep the number of trawl ahuls within each statistical rectangle fixed, and sample them with replacement from the whole RFA.
#' @param RFA Roundfish area number.
#' @param year The year of interest.
#' @param quarter The quarter of interest.
#' @export
#' @return Returns simulations of the dataras-data with length information on a similar format as the data used in the functions for calculating the CPUEs
#' @examples
simTrawlHaulsHLdatras = function(RFA,year, quarter,data)
{
  #Extract the data of interest-------------------------
    dataOfInterest = data[!is.na(data$Year) & data$Year == year&
                          !is.na(data$Quarter) & data$Quarter == quarter&
                          !is.na(data$Roundfish) & data$Roundfish == RFA ,]
  #-----------------------------------------------------

  #Simulate trawl hauls---------------------------------
  haulsID = unique(dataOfInterest$haul.id)
  nSim = length(haulsID)
  simHauls = sample(haulsID,nSim,replace = T)


  #Define a help variable for keeping the number of trawl hauls within each statistical rectangle fixed----
  rectangleID = rep(NA,nSim)
  for(i in 1:nSim)
  {
    rectangleID[i] = unique(dataOfInterest$StatRec[dataOfInterest$haul.id== haulsID[i]])
  }
  #-----------------------------------------------------

  simData = list(NULL)
  for(i in 1:nSim)
  {
    simData[[i]]= dataOfInterest[dataOfInterest$haul.id==simHauls[i],]
    simData[[i]]$haul.id = paste(simData[[i]]$haul.id,i) #Needs unique haul.id, which is achived here.

    simData[[i]]$StatRec = rectangleID[i] #Overwrite the statstical rectangle to keep the number of trawl hauls within each statistical rectangle fixed.
  }
  simDataToBeReturned  =   do.call(rbind.data.frame,simData)
  #----------------------------------------------------

  return(simDataToBeReturned)
}


#' simTrawlHaulsHLStratified
#' @description Simulates trawl hauls with only length information used in the bootstrap procedure.
#' @param RFA Roundfish area number.
#' @param year The year of interest.
#' @param quarter The quarter of interest.
#' @export
#' @return Returns simulations of the dataras-data with length information on a similar format as the data used in the functions for calculating the CPUEs.
#' The simulated trawl hauls are simulated stratisfied on the statistical rectangles. TODO: choose a procedure if there is only one observation in the statistical recangle, I suggest to include the closest trawl haul no matter which statistical recangles it is assosiated with..
#' @examples
simTrawlHaulsHLStratified = function(RFA,year, quarter,data, loc = NULL)
{
  #Extract the data of interest-------------------------
  dataOfInterest = data[!is.na(data$Year) & data$Year == year&
                          !is.na(data$Quarter) & data$Quarter == quarter&
                          !is.na(data$Roundfish) & data$Roundfish == RFA ,]
  #-----------------------------------------------------

  #Simulate trawl hauls---------------------------------
  statRec = unique(dataOfInterest$StatRec)
  simData = list(NULL)
  for(i in 1:length(statRec))
  {
    rec = statRec[i]
    trawls = unique(dataOfInterest$haul.id[dataOfInterest$StatRec==rec])
    if(length(trawls)==1)
    {
      idClosest = loc$shortesDist[which(loc$uniqueId == trawls)]
      toSample = c(toString(idClosest),toString(trawls[1]))
      sampledTreawls = sample(toSample,1)
      dTmp = dataOfInterest[dataOfInterest$haul.id==sampledTreawls,]
      dTmp$haul.idReal = trawls[1] #The id to the real trawl taken at that location
      dTmp$haul.id = paste(dTmp$haul.id ,"i:",i,sep = "") #Needs unique haul.id, which is achived here. Used in the model based approach
      dTmp$StatRec = rec
    }else{
      sampledTreawls = sample(trawls,length(trawls),replace = TRUE)
      dTmp = dataOfInterest[dataOfInterest$haul.id==sampledTreawls[1],]
      dTmp$haul.idReal = trawls[1] #The id to the real trawl taken at that location
      dTmp$haul.id = paste(dTmp$haul.id ,"i:",i,sep = "") #Needs unique haul.id, which is achived here. Used in the model based approach

      for(j in 2:length(trawls))
      {
        add = dataOfInterest[dataOfInterest$haul.id==sampledTreawls[j],]
        add$haul.idReal = trawls[j] #The id to the real trawl taken at that location. Used in the model based approach
        add$haul.id = paste(add$haul.id ,"i:",i," j:",j,sep = "") #Needs unique haul.id, which is achived here.
        dTmp = rbind(dTmp,add)
      }
    }
    simData[[i]]= dTmp

  }
  simDataToBeReturned  =   do.call(rbind.data.frame,simData)
  #----------------------------------------------------
  return(simDataToBeReturned)

}



#' simTrawlHaulsCASimple
#' @description Simulates trawl hauls with both length and age information used in the bootstrap procedure.
#' @param RFA Roundfish area number.
#' @param year The year of interest.
#' @param quarter The quarter of interest.
#' @export
#' @return Returns simulations of the dataras-data with both length and age information on a similar format as the data used in the functions for calculating the CPUEs
#' @examples
simTrawlHaulsCASimple = function(RFA,year, quarter,data)
{
  #Extract the data of interest-------------------------
  dataOfInterest = data[!is.na(data$Year) & data$Year == year&
                          !is.na(data$Quarter) & data$Quarter == quarter&
                          !is.na(data$Roundfish) & data$Roundfish == RFA,]
  #-----------------------------------------------------

  #Simulate trawl hauls---------------------------------
  haulsID = unique(dataOfInterest$haul.id)
  nSim = length(haulsID)
  simHauls = sample(haulsID,nSim,replace = T)

  simData = list(NULL)

  for(i in 1:nSim)
  {
    simData[[i]]= dataOfInterest[dataOfInterest$haul.id==simHauls[i],]
    simData[[i]]$haul.id = paste(simData[[i]]$haul.id,i) #Needs unique haul.id, which is achived here.
  }
  simDataToBeReturned  =   do.call(rbind.data.frame,simData)
  #----------------------------------------------------

  return(simDataToBeReturned)
}

#' simTrawlHaulsCAStratified
#' @description Simulates trawl hauls with both length and age information used in the bootstrap procedure.
#' @param RFA Roundfish area number.
#' @param year The year of interest.
#' @param quarter The quarter of interest.
#' @export
#' @return Returns simulations of the dataras-data with both length and age information on a similar format as the data used in the functions for calculating the CPUEs
#' Sample stratisfied with respect to the length observed. This gives meaning since it is assumed that the ALK is similar in the whole roundfish area.
#' @examples
simTrawlHaulsCAStratified = function(RFA,year, quarter,data,species = "Gadus morhua")
{
  #Extract the data of interest-------------------------
  dataOfInterest = data[!is.na(data$Year) & data$Year == year&
                          !is.na(data$Quarter) & data$Quarter == quarter&
                          !is.na(data$Roundfish) & data$Roundfish == RFA &
                          !is.na(data$Species) & data$Species == species &
                          !is.na(data$Age),]
  #-----------------------------------------------------

  #Simulate trawl hauls---------------------------------
  lengths = unique(sort(floor(dataOfInterest$LngtCm)))
  simData = list(NULL)

  for(i in 1:length(lengths)){
    l = lengths[i]
    n = sum(floor(dataOfInterest$LngtCm)==l)
    if(n ==1)
    {
      if(l<max(lengths) & l>min(lengths))
      {
        distDown = l-lengths[i-1]
        distUp = lengths[i+1] -l
        if(distUp == distDown) distDown = distDown + 0.5 - runif(1)

        if(distDown<distUp)
        {
          extra = which(floor(dataOfInterest$LngtCm)==lengths[i-1])
        }else{
          extra = which(floor(dataOfInterest$LngtCm)==lengths[i+1])
        }
      }else if(l==min(lengths)){
        extra = which(floor(dataOfInterest$LngtCm)==lengths[i-1])
      }else if(l==max(lengths)){
        extra = which(floor(dataOfInterest$LngtCm)==lengths[i+1])
      }

      whichToAdd = sample(length(extra),1)
      dTmp = rbind(dataOfInterest[floor(dataOfInterest$LngtCm)==l,],
                   dataOfInterest[extra[whichToAdd],])
      dTmp$LngtCm = l
      dTmp = sample_n(dTmp, size = n,replace = TRUE)

    }else{
      dTmp = dataOfInterest[floor(dataOfInterest$LngtCm)==l,]
      dTmp = sample_n(dTmp, size = n,replace = TRUE)
    }
    simData[[l]]= dTmp

  }
  simDataToBeReturned  =   do.call(rbind.data.frame,simData)
  #------------------------------------------------------

  return(simDataToBeReturned)
}


#<<<<<<< HEAD







#' simCaHlSimultaniousyStratified
#' @description Simulates trawl hauls with only length information used in the bootstrap procedure.
#' @param RFA Roundfish area number.
#' @param year The year of interest.
#' @param quarter The quarter of interest.
#' @export
#' @return Returns simulations of the dataras-data with length information on a similar format as the data used in the functions for calculating the CPUEs.
#' The simulated trawl hauls are simulated stratisfied on the statistical rectangles. TODO: choose a procedure if there is only one observation in the statistical recangle, I suggest to include the closest trawl haul no matter which statistical recangles it is assosiated with..
#' @examples
simCaHlSimultaniousyStratified = function(RFA,year, quarter,dataHH, loc = NULL)
{
  #Extract the data of interest-------------------------
  dataOfInterest = dataHH[!is.na(dataHH$Year) & dataHH$Year == year&
                          !is.na(dataHH$Quarter) & dataHH$Quarter == quarter&
                          !is.na(dataHH$Roundfish) & dataHH$Roundfish == RFA ,]
  #-----------------------------------------------------



  #Simulate trawl hauls---------------------------------
  statRec = unique(dataOfInterest$StatRec)
  simData = list(NULL)
  for(i in 1:length(statRec))
  {
    rec = statRec[i]
    trawls = unique(dataOfInterest$haul.id[dataOfInterest$StatRec==rec])

    if(length(trawls)==1)
    {
      idClosest = loc$shortesDist[which(loc$uniqueId == trawls)]
      toSample = c(toString(idClosest),toString(trawls[1]))
      sampledTreawls = sample(toSample,1)
      dTmp = dataOfInterest[dataOfInterest$haul.id==sampledTreawls,]
      dTmp$StatRec = rec

    }else{
      sampledTreawls = sample(trawls,length(trawls),replace = TRUE)
      dTmp = dataOfInterest[dataOfInterest$haul.id==sampledTreawls[1],]

      for(j in 2:length(trawls))
      {
        add = dataOfInterest[dataOfInterest$haul.id==sampledTreawls[j],]
        dTmp = rbind(dTmp,add)
      }
    }
    simData[[i]]= dTmp

  }
  simDataToBeReturned  =   do.call(rbind.data.frame,simData)
  #----------------------------------------------------
  return(simDataToBeReturned)

}

#=======
#'
#' resampling for hiearchical bootstrap. Strucutre is a bit different from the above to ensure that haul selection is consistent between the different tables.
#' simTrawlHaulsHiearchical could probably be modified to make parameters quite similar to the ones above.
#'

#' selectHaulIdsHiearchical
#' @description Makes hiearchical selection of haul ids for bootstrap estimation.
#' @param hh Data set of hauls, formatted as the DATRAS table (could be HH, CA or HL), but containing at most one row pr. haul.
#' @param levels vector with names of columns in hh that represents levels in the hiearchical sampling. levels[1] identifies the primary sampling unit, levels[2] the secondary, and so on.
#' @param selection codes for how selection is done: "R" selection with replacement (sample size: the number of available samples), "S" selection without replacement with sample size a selected as a random integer between 1 and the number of available samples (inclusive), "N": No selection or selection without replacement with sample size: the number of available samples
#' @param idname the name of a column that identifies samples
#' @param seed seed for selection, if NULL seed will not be set.
#' @return a vector of haul ids, possibly repeating. Note: Does not return a data structure suitable for feeding into functions for CPUE-estimates
#' @keywords internal
selectHaulIdsHiearchical <-
  function(hh,
           levels,
           selection,
           seed = NULL) {
    if (length(levels) != length(selection)) {
      stop("selection codes must match the number of levels in hiearchy")
    }
    if (length(levels) < 1) {
      stop("Must provide at least one level")
    }

    idname = "haul.id"
    if (any(duplicated(hh[,c(idname, levels)]))){
      stop(paste("levels and ", idname, "does not uniquely identify rows."))
    }


    #
    # make selection at level
    #
    levelcolumn <- levels[1]
    selectionmethod <- selection[1]
    levelvalues <- unique(hh[, levelcolumn])
    if (!is.null(seed)) {
      set.seed(seed)
    }
    if (selectionmethod == "R") {
      levelvalues <- sample(levelvalues, size=length(levelvalues), replace=T)
    }
    else if (selectionmethod == "S") {
      n <- sample(1:length(levelvalues), size=1, replace=F)
      levelvalues <- sample(levelvalues, size=n, replace=F)
    }
    else if (selectionmethod == "N") {
      levelvalues <- sample(levelvalues, size=length(levelvalues), replace=F)
    }
    else{
      stop(paste("Invalid selection method", selectionmethod))
    }

    #
    # make selection on lower levels
    #
    ids <- c()

    #base case, no lower levels exists
    if (length(levels) == 1) {
      for (v in levelvalues) {
        ids <- c(ids, as.vector(hh[hh[, levelcolumn] == v, idname]))
      }
    }

    #recursive case, lower levels exists
    else if (length(levels) > 1) {
      sublevels <- levels[2:length(levels)]
      subselection <- selection[2:length(selection)]
      for (v in levelvalues) {
        ids <-
          c(ids,
            selectHaulIdsHiearchical(hh[hh[, levelcolumn] == v,], sublevels, subselection, NULL))
      }
    }
    else{
      stop("ERROR: Recursed past base case")
    }

    return(ids)
  }

#' simTrawlHaulsBySelection
#' @description selects a possibly repeating sample with generated unique ids for bootstrap
#' @param data data frame to resample, e.g. CA or HL table from DATRAS
#' @param idvalues values of the column haul.id that defines the selection
#' @param oldidname column name where original haul ids should be stored
#' @param removeMIssing indicates whether idvalues with no corresponding entries in data should be rmeoved. If FALSE, this case halts execution
#' @return data frame with the specified selection of samples, with column haul.id populated with unqiue identifiers in stead of the original identifiers, and the original identifiers stored in the column oldidname. idvalues not in data will be represented by NAs
#' @keywords internal
simTrawlHaulsBySelection <- function(data, idvalues, oldidname="original.id", removeMissing=F){
  idname="haul.id"

  hauls <- data.frame(oldid=idvalues, newid=as.factor(1:length(idvalues)))
  hauls[,oldidname] <- hauls$oldid

  if (!all(hauls$oldid %in% data[,idname]) & removeMissing){
    hauls <- hauls[hauls$oldid %in% data[,idname],]
  }
  else if (!all(hauls$oldid %in% data[,idname]) & !removeMissing){
    stop("Not all idvalues found in data")
  }

  simdata <- merge(data, hauls, by.x=idname, by.y="oldid")
  simdata[,idname] <- simdata$newid
  simdata$newid <- NULL

  return(simdata)
}

#' simTrawlHaulsHiearchical
#' @description Simulates selection of hauls by selection of StatRec without replacement, and selection of haul.id with replacement
#' @param RFA Roundfish area number.
#' @param year The year of interest.
#' @param quarter The quarter of interest.
#' @param hl data formated as DATRAS HL table
#' @param ca data formated as DATRAS CA table
#' @returns list of tables with formatted the same way, but with artificial haul.ids, original haul.id is preserved in column original.id
#' @export
simTrawlHaulsHiearchical <- function(RFA, year, quarter, hl, ca, hierarchy=c("StatRec", "haul.id"), selection=c("S","R")){

  dataOfInterest <- function(data){
    return(data[!is.na(data$Year) & data$Year == year&
           !is.na(data$Quarter) & data$Quarter == quarter&
           !is.na(data$Roundfish) & data$Roundfish == RFA ,])
  }

  hl <- dataOfInterest(hl)
  ca <- dataOfInterest(ca)

  if (any(!(ca$haul.id %in% hl$haul.id))){
    stop("Some age measurements does not have corresponding length measurements")
  }

  hlframe <- hl[!duplicated(hl[,c(hierarchy, "haul.id")]),]

  haul.ids <- selectHaulIdsHiearchical(hlframe, hierarchy, selection)

  ret <- list()
  ret$simHL <- simTrawlHaulsBySelection(hl, haul.ids, oldidname="original.id", removeMissing=F)
  ret$simCA <- simTrawlHaulsBySelection(ca, haul.ids, oldidname="original.id", removeMissing=T)

  return(ret)
}
#>>>>>>> 8b4fed59e85b5aeb182d1dbdd71dfd1211d25fcd
