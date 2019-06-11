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
  #  eval(parse(text = paste("original",art,year,"Q",quarter,"= readRDS(paste(path, 'Removal",art,"Dl",1,"year",year,"Q",quarter,"n",n,procedure,"999',sep = '')",")",sep = "")))
  #eval(parse(text = paste("CVsimple[1,] = original",art,year,"Q", quarter,"$mCPUE$sd/original",art,year,"Q", quarter,"$mCPUE$mean",sep = "")))
    eval(parse(text = paste("CVsimple[1,] = original",art,year,"Q", quarter,"$sd/original",art,year,"Q", quarter,"$bootstrapMean",sep = "")))
  }else{
  #  eval(parse(text = paste("original",art,year,"Q",quarter,"= readRDS(paste(path, 'Removal",art,"Dl",i,"year",year,"Q",quarter,"n",n,procedure,"999',sep = '')",")",sep = "")))
    eval(parse(text = paste("original",art,year,"Q",quarter,"= readRDS(paste(path, 'Removal",art,"Dl",dl,"year",year,"Q",quarter,"n",n,procedure,"999',sep = '')",")",sep = "")))
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
    axis(1, at=1:6, labels=c("5cm", "4cm","3cm","2cm","1cm",""), cex.axis=1.5 , cex.sub=1)
    title( xlab="Lenght group width",
           main = paste(art, " ", year, " Q",quarter, sep = ""), cex.main =1.8, cex.lab=1.5, cex.axis=1)
  #  abline(h = CVsimple[2,(minAgePlot:maxAgePlot+1)],col = col_vector[(minAgePlot +1):999])

  }else{
    axis(1, at=1:6, labels=c("1", "2","3","4","5","current"), cex.axis=1.5 , cex.sub=1)
    title(xlab="Number of age samples",
           main = paste(art, " ", year, " Q",quarter, ", 5cm length groups", sep = ""), cex.main =1.8, cex.lab=1.5, cex.axis=1)
  #  abline(h = CVsimple[1,(minAgePlot:maxAgePlot+1)],col = col_vector[(minAgePlot +1):999])
  }



  if(minAge==0){
    ll = c(ageGroups[(minAgePlot:maxAgePlot+1)])
    ll = as.character(ll)
    ll[length(ll)] = paste0(ll[length(ll)] , "+")
    legend(x=0.1,y=0.7,legend = ll, title ="Age",
           col = col_vector[(minAgePlot +1):999],box.lty =0,pch = 15,cex = 1.2)
  }else if(minAge==1){
    ll = c(ageGroups[(minAgePlot:maxAgePlot)])
    ll = as.character(ll)
    ll[length(ll)] = paste0(ll[length(ll)] , "+")
    legend(x=0.1,y=0.7,legend = ll, title ="Age",
           col = col_vector[(minAgePlot+1):999],box.lty =0,pch = 15,cex = 1.2)
  }
}


#' plotRemoval
#' @description .
#' @param
#' @export
#' @return
#' @examples
plotRemovalNandO = function(year,art,quarter,procedure = "datras",minAge,maxAge,path,dl = 5){
  dlDiv = c(1,5)
  Ndiv = c(25,50,75,100,200,300,400,500)
  if(procedure=="haulBased")   Ndiv = c(25,50,75,100,200,300,400,500)
  for(ss in dlDiv){
    for(N in Ndiv){
      nUse = 0
      for(n in seq(100,1000,by = 100)){
        if(file.exists(paste0(path, "resample",art,"Dl",dl,"N",N,"year",year,"Q",quarter,"n",n,procedure,ss))){
          eval(parse(text = paste("removeSimple",ss,art,"DL",dl,"Year",year,"Q",quarter,"N",N,"= readRDS(paste(path, 'resample",art,"Dl",dl,"N",N,"year",year,"Q",quarter,"n",n,procedure,ss,"',sep = '')",")",sep = "")))
          nUse = n
        }
      }
      if(nUse==0)stop(paste0("No bootstrap samples with ", N ," hauls and ", ss," otoliths per length group provided "))

      print(paste0("Use ", nUse, " bootstrap samples for ", ss, " otoliths per ", dl, "cm length group with ", N , " resampled hauls in year ", year,"."))

    }
  }
  ageGroups = as.character(minAge:maxAge)

  CVsimple1 = matrix(0,length(Ndiv),maxAge+1)
  CVsimple5 = matrix(0,length(Ndiv),maxAge+1)
  for(ss in dlDiv){
    counter = 1
    for(N in Ndiv){
      eval(parse(text = paste("CVsimple",ss,"[counter,] = removeSimple",ss,art, "DL",dl,"Year",year, "Q",quarter,"N",N,"$sd/removeSimple",ss,art, "DL",dl,"Year",year, "Q",quarter,"N",N,"$bootstrapMean",sep = "")))
      counter = counter + 1
    }
  }

  nameInPlot = "areaBased"
  if(procedure == "haulBased")  nameInPlot = "haulBased"
  if(quarter == 1){
    par(mfrow = c(2,3))
  }else if(quarter ==3){
    par(mfrow = c(2,4))
  }
  for(i in (as.integer(ageGroups)+1)){
    plot(Ndiv,CVsimple5[,i],
         xlim = c(0,500),
         ylim = c(0,2.8),
         ylab = "RSE",
         main =paste0("Cod", " year ",year,  " Q", quarter, " age ", i-1, " "),
        # main =paste0(art, " year ",year,  " Q", quarter, " age ", i-1, " ",nameInPlot),
        xlab = "Number of hauls",
        cex = 2,cex.lab = 1.8,cex.main = 2, cex.axis=1.8,
         pch = 19,
         lwd = 3,
         type = 'l')
    points(Ndiv,CVsimple1[,i],
           col = "red",
           type = 'l',
           lwd = 3,
           add = TRUE)
  }
}

