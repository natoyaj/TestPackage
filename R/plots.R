#' plotRemoval
#' @description .
#' @param
#' @export
#' @return
#' @examples
plotRemoval = function(year,art,quarter,n,procedure = "haulBased",minAge,maxAge,minAgePlot,maxAgePlot,path){
  for(i in 1:5){
     eval(parse(text = paste("removeSimple",art,i,"Year",year,"Q",quarter,"= readRDS(paste(path, 'Removal",art,"Dl",i,"year",year,"Q",quarter,"n",n,procedure,"1',sep = '')",")",sep = "")))
  }
  ageGroups = as.character(minAge:maxAge)

  CVsimple = matrix(0,11,maxAge + 1)
  eval(parse(text = paste("original",art,year,"Q",quarter," =  readRDS(paste('",path,art,year,"Q",quarter,"n",n,procedure,"'))",sep = "")))

  eval(parse(text = paste("CVsimple[1,] = original",art,year,"Q", quarter,"$sd/original",art,year,"Q", quarter,"$bootstrapMean",sep = "")))

  for(i in 1:5){
    eval(parse(text = paste("CVsimple[i+1,] = removeSimple",art, i,"Year",year, "Q",quarter,"$mCPUE$sd/removeSimple",art, i,"Year",year, "Q",quarter,"$mCPUE$mean",sep = "")))
  }

  meanRemovedOtolitsSimple = (1:length(1:5))*0
  for(i in (1:5)){
    eval(parse(text = paste("removeOtoliths = removeSimple",art, i,"Year",year, "Q",quarter,sep = "")))
    nOtolithsRemoved = attributes(removeOtoliths)$nOtolithsRemoved
    nOtolithsTotal = attributes(removeOtoliths)$nOtolithsTotal
    meanRemovedOtolitsSimple[i+1] = mean(nOtolithsRemoved/nOtolithsTotal)
  }

  plot(rep(meanRemovedOtolitsSimple[1],length((minAgePlot:maxAgePlot+1))),CVsimple[1,(minAgePlot:maxAgePlot+1)],
       xlim = c(0,0.8),
       ylim = c(0,0.7),
       col = col_vector[(minAgePlot +1):999],
       xlab = "Average otolith sampling reduction in proportion",
       ylab = "RSE",
       main =paste(art, " ", year, " Q",quarter,sep = ""),
       cex = 2,cex.lab = 1.5,cex.main = 3,
       pch = 19)

    for(i in (2:dim(CVsimple)[2])){
      points(rep(meanRemovedOtolitsSimple[i],length((minAgePlot:maxAgePlot+1))),CVsimple[i,(minAgePlot:maxAgePlot+1)],
       col = col_vector[(minAgePlot +1):999],
       cex = 2,
       pch = 15)
    }

  #Plot removed vs CV for a special selection of length groups
  if(file.exists(paste(path, "RemovalCodi",4,"year",year,"Q",quarter,"n",n,procedure,sep = ''))){ #Stupid name, need to change if we decide on a length division
    eval(parse(text = paste("remove",art,"SpecialSelectionYear",year,"Q",quarter,"= readRDS(paste(path, 'RemovalCodi",4,"year",year,"Q",quarter,"n",n,procedure,"',sep = '')",")",sep = "")))
    CV = CVsimple*0
    eval(parse(text = paste("CV[1,] = remove",art, "SpecialSelectionYear",year, "Q",quarter,"$mCPUE$sd/remove",art, "SpecialSelectionYear",year, "Q",quarter,"$mCPUE$mean",sep = "")))
    meanRemovedOtolits = (1:dim(CVsimple)[1])*0
    eval(parse(text = paste("removeOtoliths = remove",art,"SpecialSelectionYear",year,"Q",quarter,sep = "")))
    nOtolithsRemoved = attributes(removeOtoliths)$nOtolithsRemoved
    nOtolithsTotal = attributes(removeOtoliths)$nOtolithsTotal
    meanRemovedOtolits[1] = mean(nOtolithsRemoved/nOtolithsTotal)
    points(rep(meanRemovedOtolits[1],length((minAgePlot:maxAgePlot+1))),CV[1,(minAgePlot:maxAgePlot+1)]
           ,col =  col_vector[(minAgePlot +1):999],
           cex = 2.5,
           pch = 8)
    }

  abline(h = CVsimple[1,(minAgePlot:maxAgePlot+1)],col = col_vector[(minAgePlot +1):999])
  if(minAge==0){
    legend(x=0,y=0.7,legend = c(ageGroups[(minAgePlot:maxAgePlot+1)]),
           col = col_vector[(minAgePlot +1):999],box.lty =0,pch = 15,cex = 1.2)
  }else if(minAge==1){
    legend(x=0,y=0.7,legend = c(ageGroups[minAgePlot:maxAgePlot]),
           col = col_vector[(minAgePlot+1):999],box.lty =0,pch = 15,cex = 1.2)
  }
}
