#' plotRemoval
#' @description .
#' @param
#' @export
#' @return
#' @examples
plotRemoval = function(year,art,quarter,n,procedure = "haulBased",minAge,maxAge,minAgePlot,maxAgePlot,path,dl = NULL){
  if(is.null(dl)){
    for(i in 1:5){#extract results when using length groups 1,...,5 cm
      eval(parse(text = paste("removeSimple",art,i,"Year",year,"Q",quarter,"= readRDS(paste(path, 'Removal",art,"Dl",i,"year",year,"Q",quarter,"n",n,procedure,"1',sep = '')",")",sep = "")))
    }
  }else{
    for(i in 1:5){#extract results when using length groups with width dl, and with use of 1,...,5 otoliths
      eval(parse(text = paste("removeSimple",art,i,"Year",year,"Q",quarter,"= readRDS(paste(path, 'Removal",art,"Dl",dl,"year",year,"Q",quarter,"n",n,procedure,i,"',sep = '')",")",sep = "")))
    }
  }

  ageGroups = as.character(minAge:maxAge)

  CVsimple = matrix(0,6,maxAge + 1)
  if(is.null(dl)){
    eval(parse(text = paste("original",art,year,"Q",quarter," =  readRDS(paste('",path,art,year,"Q",quarter,"n",n,procedure,"'))",sep = "")))
    eval(parse(text = paste("CVsimple[1,] = original",art,year,"Q", quarter,"$sd/original",art,year,"Q", quarter,"$bootstrapMean",sep = "")))
  }else{
    eval(parse(text = paste("original",art,year,"Q",quarter,"= readRDS(paste(path, 'Removal",art,"Dl",i,"year",year,"Q",quarter,"n",n,procedure,"999',sep = '')",")",sep = "")))
    eval(parse(text = paste("CVsimple[1,] = original",art,year,"Q", quarter,"$mCPUE$sd/original",art,year,"Q", quarter,"$mCPUE$mean",sep = "")))
  }

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

  if(is.null(dl)){
    plot(rep(6,length((minAgePlot:maxAgePlot+1))),CVsimple[1,(minAgePlot:maxAgePlot+1)]+1000,
         xlim = c(0,6),
         ylim = c(0,0.7),
         col = col_vector[(minAgePlot +1):999],
         xlab = "",
         ylab = "RSE",
         main ="",
         cex = 2,cex.lab = 1.5,cex.main = 3,
         pch = 19,
         xaxt = 'n')
    }else{
    plot(rep(6,length((minAgePlot:maxAgePlot+1))),CVsimple[1,(minAgePlot:maxAgePlot+1)],
         xlim = c(0,6),
         ylim = c(0,0.7),
         col = col_vector[(minAgePlot +1):999],
         xlab = "",
         ylab = "RSE",
         main ="",
         cex = 2,cex.lab = 1.5,cex.main = 3,
         pch = 19,
         xaxt = 'n')
  }



  if(is.null(dl)){
    for(i in (2:(dim(CVsimple)[1]))){
      points(rep(dim(CVsimple)[1]-i+1,length((minAgePlot:maxAgePlot+1))),CVsimple[i,(minAgePlot:maxAgePlot+1)],
             col = col_vector[(minAgePlot +1):999],
             cex = 2,
             pch = 15)
    }
  }else{
    for(i in (2:(dim(CVsimple)[1]))){
      points(rep(i-1,length((minAgePlot:maxAgePlot+1))),CVsimple[i,(minAgePlot:maxAgePlot+1)],
             col = col_vector[(minAgePlot +1):999],
             cex = 2,
             pch = 15)
    }
  }

  if(is.null(dl)){
    axis(1, at=1:6, labels=c("5cm", "4cm","3cm","2cm","1cm",""))
    title( xlab="Lenght group width",
           main = paste(art, " ", year, " Q",quarter, sep = ""))
    abline(h = CVsimple[2,(minAgePlot:maxAgePlot+1)],col = col_vector[(minAgePlot +1):999])

  }else{
    axis(1, at=1:6, labels=c("1", "2","3","4","5","current"))
    title(xlab="Number of ototlihs sampled",
           main = paste(art, " ", year, " Q",quarter, ", 5cm length groups", sep = ""))
    abline(h = CVsimple[1,(minAgePlot:maxAgePlot+1)],col = col_vector[(minAgePlot +1):999])
  }


  if(minAge==0){
    legend(x=0.1,y=0.7,legend = c(ageGroups[(minAgePlot:maxAgePlot+1)]),
           col = col_vector[(minAgePlot +1):999],box.lty =0,pch = 15,cex = 1.2)
  }else if(minAge==1){
    legend(x=0.1,y=0.7,legend = c(ageGroups[minAgePlot:maxAgePlot]),
           col = col_vector[(minAgePlot+1):999],box.lty =0,pch = 15,cex = 1.2)
  }
}


