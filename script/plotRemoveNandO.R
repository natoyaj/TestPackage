#' plotRemoval
#' @description .
#' @param
#' @export
#' @return
#' @examples
plotRemovalNandO = function(year,art,quarter,n,procedure = "datras",minAge,maxAge,path,dl = 5){
 # path = "Papers/manuscript/results/olav/resamplingNandOtoliths/"
 # procedure = "datras"
 # procedure = "haulBased"
  dlDiv = c(1,5)
  Ndiv = c(50,75,100,150,200,250,300,350)
  dl = 5
  #year = 2018
  #n = 300
  for(ss in dlDiv){
    for(N in Ndiv){
      eval(parse(text = paste("removeSimple",ss,art,"DL",dl,"Year",year,"Q",quarter,"N",N,"= readRDS(paste(path, 'resample",art,"Dl",dl,"N",N,"year",year,"Q",quarter,"n",n,procedure,ss,"',sep = '')",")",sep = "")))
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
 # x11()
  if(quarter == 1){
    par(mfrow = c(2,3))
  }else if(quarter ==3){
    par(mfrow = c(2,4))
  }
  for(i in (as.integer(ageGroups)+1)){
    plot(Ndiv,CVsimple5[,i],
         xlim = c(0,350),
         ylim = c(0,1),
         ylab = "RSE",
         main =paste0(art, " year ",year,  " Q", quarter, " age ", i-1, " ",nameInPlot),
         xlab = "N",
         cex = 2,cex.lab = 1.5,cex.main = 2,
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

