pathFigures = "Papers/ourPaper/figures/"
#Read data and fit model----------
year = 2018; quarter = 1; species = "Gadus morhua";
#year = 2017; quarter = 3; species = "Pollachius virens";

dat = readIBTSData(survey = "NS-IBTS", year = year, quarter = quarter,species = species)
fit =  fitModel(species = species, quarter =quarter, year = year, ca_hh = dat$ca_hh,hh = dat$hh)
#------------------------------------

source("script/fullHaulBasedALKforPlot.R") #Script for defining function which can return the full haul based ALK

#Plot cod 2018 Q1 ALK with model and with Datras for a haul in RFA 1 with the most observations of ages of cod
id = "2018:1:SCO:SCO3:GOV:42:42"
for(RFA in 1:9){
  alk = calculateALKModel(RFA = RFA, species= species, year = year, quarter = quarter,hh = dat$hh,data = dat$ca_hh, fitModel = fit,report = NULL)
  alkD = calculateALKDatras(RFA = RFA, species= species, year = year, quarter = quarter,data = dat$ca_hh)
  alkH = calculateALKNewFull(RFA = RFA, species= species, year = year, quarter = quarter,data = dat$ca_hh,data_hl = dat$hl_hh)
  for(i in 1:length(alk)){
    if(alk[[i]][1,1]==id){
      counter = i
      break
    }
  }
  for(i in 1:length(alkH)){
    if(alkH[[i]][1,1]==id){
      counter2 = i
      break
    }
  }
  if(counter >0)break;
}

alk = alk[[counter]]
alkH = alkH[[counter2]]

jpeg(paste(pathFigures,"ALKmodelQ1year2018RFA1HaulWithMostObservations.jpeg",sep = ""))
plot(alk$Length,alk$`0` ,
     ylim = c(0,1),
     xlab = "Length", ylab = "Proportion", main = "Model based ALK",
     lwd=3, col="red",type = 'l',
     cex.lab=1.5, cex.main = 1.6,cex.axis = 1.2)
lines(alk$Length,alk$`1` ,
      ylim = c(0,1),
      lwd=3, col="grey",type = 'l')
lines(alk$Length,alk$`2` ,
      ylim = c(0,1),
      lwd=3, col="blue",type = 'l')
lines(alk$Length,alk$`3` ,
      ylim = c(0,1),
      lwd=3, col="black",type = 'l')
lines(alk$Length,alk$`4` ,
      ylim = c(0,1),
      lwd=3, col="darkgreen",type = 'l')
lines(alk$Length,alk$`5` ,
      ylim = c(0,1),
      lwd=3, col="purple",type = 'l')
lines(alk$Length,alk$`6` ,
      ylim = c(0,1),
      lwd=3, col="brown",type = 'l')
abline(h=0)
legend(x=61,y=1.02,legend = c("1 year", "2 year","3 year","4 year","5 year",">5 year")
       ,col=c("grey","blue","black","darkgreen","purple","brown"),lty = 1,lwd = 3,cex = 0.9)
caInt = dat$ca_hh[dat$ca_hh$haul.id==id,]
points(caInt$LngtCm[caInt$Age==1],1/10 + runif(sum(caInt$Age==1))*0.01,pch = 16, col = 'grey')
points(caInt$LngtCm[caInt$Age==2],2/10 + runif(sum(caInt$Age==2))*0.01,pch =16, col = 'blue')
points(caInt$LngtCm[caInt$Age==3],3/10 + runif(sum(caInt$Age==3))*0.01,pch=16,col = 'black')
points(caInt$LngtCm[caInt$Age==4],4/10 + runif(sum(caInt$Age==4))*0.01,pch=16,col = 'darkgreen')
points(caInt$LngtCm[caInt$Age==5],5/10 + runif(sum(caInt$Age==5))*0.01,pch=16,col = 'purple')
points(caInt$LngtCm[caInt$Age>5],6/10 + runif(sum(caInt$Age>5))*0.01,pch=16,col = 'brown')
dev.off()


for(i in 1:dim(alkD)[1]){
  alkD[i,3:8] = alkD[i,3:8]/sum(alkD[i,3:8])
}


jpeg(paste(pathFigures,"ALKdatrasQ1year2018RFA1.jpeg",sep = ""))
plot(alkD$length,alkD$`0` ,
     ylim = c(0,1),
     xlab = "Length", ylab = "Proportion", main = "DATRAS ALK",
     lwd=3, col="red",type = 'l',
     cex.lab=1.5, cex.main = 1.6,cex.axis = 1.2)
lines(alkD$length,alkD$`1` ,
      ylim = c(0,1),
      lwd=3, col="grey",type = 'l')
lines(alkD$length,alkD$`2` ,
      ylim = c(0,1),
      lwd=3, col="blue",type = 'l')
lines(alkD$length,alkD$`3` ,
      ylim = c(0,1),
      lwd=3, col="black",type = 'l')
lines(alkD$length,alkD$`4` ,
      ylim = c(0,1),
      lwd=3, col="darkgreen",type = 'l')
lines(alkD$length,alkD$`5` ,
      ylim = c(0,1),
      lwd=3, col="purple",type = 'l')
lines(alkD$length,alkD$`6` ,
      ylim = c(0,1),
      lwd=3, col="brown",type = 'l')
abline(h=0)
dev.off()


for(i in 1:dim(alkH)[1]){
  alkH[i,4:9] = alkH[i,4:9]/sum(alkH[i,4:9])
}
jpeg(paste(pathFigures,"ALKhaulQ1year2018RFA1.jpeg",sep = ""))
plot(alkH$Length,alkH$`0` ,
     ylim = c(0,1),
     xlab = "Length", ylab = "Proportion", main = "Haul based ALK",
     lwd=3, col="red",type = 'l',
     cex.lab=1.5, cex.main = 1.6,cex.axis = 1.2)
lines(alkH$Length,alkH$`1` ,
      ylim = c(0,1),
      lwd=3, col="grey",type = 'l')
lines(alkH$Length,alkH$`2` ,
      ylim = c(0,1),
      lwd=3, col="blue",type = 'l')
lines(alkH$Length,alkH$`3` ,
      ylim = c(0,1),
      lwd=3, col="black",type = 'l')
lines(alkH$Length,alkH$`4` ,
      ylim = c(0,1),
      lwd=3, col="darkgreen",type = 'l')
lines(alkH$Length,alkH$`5` ,
      ylim = c(0,1),
      lwd=3, col="purple",type = 'l')
lines(alkH$Length,alkH$`6` ,
      ylim = c(0,1),
      lwd=3, col="brown",type = 'l')
abline(h=0)
dev.off()

#---------------------------------------------------------






#Plot cod 2018 Q1 ALK spatial effect----------------------------------------
xlim = c(-6,14)
ylim = c(53,62)
age = 2
length = 20


polygons <- readOGR("inst/shapefiles/Roundfish_shapefiles")

data(countriesHigh)
map <- countriesHigh

report = fit$obj$report()
tau = exp(report$logTau)
d1 = report$x[,2]/tau[2]
d2 = report$x[,3]/tau[3]
d3 = report$x[,4]/tau[4]
d4 = report$x[,5]/tau[5]
d5 = report$x[,6]/tau[6]
projXY = fit$projXY
repLength = report$repLength

maxLength = confALK(species = species,quarter = quarter)$maxLength
nu1 = exp(repLength[length +maxLength*1] + d1)
nu2 = exp(repLength[length +maxLength*2] + d2)
nu3 = exp(repLength[length +maxLength*3] + d3)
nu4 = exp(repLength[length +maxLength*4] + d4)
nu5 = exp(repLength[length +maxLength*5] + d5)

truncationL = setBoundrySpline(species = species, quarter = quarter, ca_hh = dat$ca_hh)$l
truncationU = setBoundrySpline(species = species, quarter = quarter, ca_hh = dat$ca_hh)$u
boarder = fit$boarder

prob = list()
for(i in 1:7){
  prob[[i]] = nu1*0;
}


if(quarter ==1){

  if(truncationL[1]>length | truncationU[1]<length)nu2 = 0
  if(truncationL[2]>length | truncationU[2]<length)nu3 = 0
  if(truncationL[3]>length | truncationU[3]<length)nu4 = 0
  if(truncationL[4]>length | truncationU[4]<length)nu5 = 0

  prob[[3]] = nu2/(1+nu2)
  prob[[4]] = nu3/(1+nu3)*(1-prob[[3]])
  prob[[5]] = nu4/(1+nu4)*(1-prob[[3]]-prob[[4]])
  prob[[6]] = nu5/(1+nu5)*(1-prob[[3]]-prob[[4]]-prob[[5]])

  if(length<=boarder){
    prob[[2]]= round(1-prob[[3]] -prob[[4]] -prob[[5]]  -prob[[6]] , digits = 3)
  }else{
    prob[[7]] = round(1-prob[[3]] -prob[[4]] -prob[[5]]  -prob[[6]] , digits = 3)
  }
}
if(quarter ==3){

  if(truncationL[1]>length | truncationU[1]<length)nu1 = 0
  if(truncationL[2]>length | truncationU[2]<length)nu2 = 0
  if(truncationL[3]>length | truncationU[3]<length)nu3 = 0
  if(truncationL[4]>length | truncationU[4]<length)nu4 = 0
  if(truncationL[5]>length | truncationU[5]<length)nu5 = 0

  prob[[2]]  = nu1/(1+nu1)
  prob[[3]]  = nu2/(1+nu2)*(1-prob[[2]] )
  prob[[4]]  = nu3/(1+nu3)*(1-prob[[2]] -prob[[3]] )
  prob[[5]]  = nu4/(1+nu4)*(1-prob[[2]] -prob[[3]] -prob[[4]] )
  prob[[6]]  = nu5/(1+nu5)*(1-prob[[2]] -prob[[3]] -prob[[4]] -prob[[5]] )

  if(length<=boarder){
    prob[[1]]= round(1-prob[[2]]-prob[[3]]-prob[[4]]-prob[[5]] -prob[[6]], digits = 3)
  }else{
    prob[[7]] = round(1-prob[[2]]-prob[[3]]-prob[[4]]-prob[[5]] -prob[[6]], digits = 3)
  }
}


low = length-1
upp = length+1
indexAge = list()
indexAge[[1]] = which(dat$ca_hh$Age ==0 & dat$ca_hh$LngtCm>=low &dat$ca_hh$LngtCm<=upp)
indexAge[[2]] = which(dat$ca_hh$Age ==1 & dat$ca_hh$LngtCm>=low &dat$ca_hh$LngtCm<=upp)
indexAge[[3]] = which(dat$ca_hh$Age ==2 & dat$ca_hh$LngtCm>=low &dat$ca_hh$LngtCm<=upp)
indexAge[[4]] = which(dat$ca_hh$Age ==3 & dat$ca_hh$LngtCm>=low &dat$ca_hh$LngtCm<=upp)
indexAge[[5]] = which(dat$ca_hh$Age ==4 & dat$ca_hh$LngtCm>=low &dat$ca_hh$LngtCm<=upp)
indexAge[[6]] = which(dat$ca_hh$Age ==5 & dat$ca_hh$LngtCm>=low &dat$ca_hh$LngtCm<=upp)
indexAge[[7]] = which(dat$ca_hh$Age ==6 & dat$ca_hh$LngtCm>=low &dat$ca_hh$LngtCm<=upp)

breaks   = c(0.01,0.02,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.01)

image.plot(projXY$x,projXY$y, inla.mesh.project(projXY, prob[[age+1]]),col =  colorRampPalette(c("white","yellow", "red"))(11),
           main = paste("Proportion with age ", age, " among ", length, " cm long ", species,sep = ""), xlab = 'Degrees east', ylab = 'Degrees north',
           cex.lab=1.5, cex.main = 1,cex.axis = 1.2,
           xlim = xlim,
           ylim = ylim,
           breaks   =breaks)

contour(projXY$x, projXY$y,inla.mesh.project(projXY, prob[[age+1]]) ,add = T,labcex  = 1,cex = 1,
        breaks   = breaks,
        zlim = c(0.005,0.9))
plot(map, col="grey", add=T)
if (!is.null(polygons)){
  plot(polygons, add=T,lwd = 3)
}
#for(i in 1:7){
#  points(dat$ca_hh$lon[indexAge[[i]]] +runif(length(indexAge[[i]]))*0.1,dat$ca_hh$lat[indexAge[[i]]]+runif(length(indexAge[[i]]))*0.1,col = i ,lwd = 3)
#}
#legend(x=10,y=62,legend = c("0 year", "1 year","2 year","3 year","4 year","5 year", ">5 year")
#       ,col=c(1:7),cex = 1,pch = 1,lwd = 4)
pointLabel(coordinates(polygons),labels=polygons$AreaName,cex = 2)

points(dat$ca_hh$lon[indexAge[[1]]] +runif(length(indexAge[[1]]))*0.1,dat$ca_hh$lat[indexAge[[1]]]+runif(length(indexAge[[1]]))*0.1,col = "black" ,lwd = 3)
points(dat$ca_hh$lon[indexAge[[2]]] +runif(length(indexAge[[2]]))*0.1,dat$ca_hh$lat[indexAge[[2]]]+runif(length(indexAge[[2]]))*0.1,col = "green" ,lwd = 3)
points(dat$ca_hh$lon[indexAge[[3]]] +runif(length(indexAge[[3]]))*0.1,dat$ca_hh$lat[indexAge[[3]]]+runif(length(indexAge[[3]]))*0.1,col = "blue" ,lwd = 3)
points(dat$ca_hh$lon[indexAge[[4]]] +runif(length(indexAge[[4]]))*0.1,dat$ca_hh$lat[indexAge[[4]]]+runif(length(indexAge[[4]]))*0.1,col = "brown" ,lwd = 3)
legend(x=10,y=62,legend = c("0 year", "1 year","2 year","3 year")
         ,col=c("Black","Green","Blue","Brown"),cex = 1,pch = 1,lwd = 4)
#legend(x=10,y=62,legend = c("0 year", "1 year","2 year","3 year","4 year","5 year", ">5 year")
#       ,col=c(1:7),cex = 1,pch = 1,lwd = 4)
#-----------------------------------------------------------------




