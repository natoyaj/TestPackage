#Read data----------
year = 2018
quarter = 1
#quarter = 3
#species = "Pollachius virens"
species = "Gadus morhua";

dat = readIBTSData(survey = "NS-IBTS", year = year, quarter = quarter,species = species)
dat$hh$originalIdAtThisLocation = dat$hh$haul.id
#-------------------

fit =  fitModel(species = species, quarter =quarter, year = year, ca_hh = dat$ca_hh,hh = dat$hh)
lonLat = c(-1.5,59.27)
lonLat = c(4.9,58.03)
#lonLat = c(4.5,58.2)
#lonLat = c(5.3,58.9)

#id ="2018:1:SWE:DANS:GOV:77:19"
#id ="2018:1:SCO:SCO3:GOV:46:46"
#lonLat = c(dat$hh[dat$hh$haul.id==id,]$lon,dat$hh[dat$hh$haul.id==id,]$lat)

closest = 0
for(RFA in 1:9){
  alk = calculateALKModel(RFA = RFA, species= species, year = year, quarter = quarter,hh = dat$hh,data = dat$ca_hh, fitModel = fit,report = NULL)


  minimum = 99999
  idClosest = ""
  for(id in fit$haulId){
    tmp = dat$hh[dat$hh$haul.id==id,]
    tt = rbind(c(tmp$lon,tmp$lat),lonLat)
    if(minimum>spDists(tt)[2,1]){
      minimum = spDists(tt)[2,1]
      idClosest = id
    }
  }

  for(i in 1:length(alk)){
    if(alk[[i]][1,1]==idClosest){
      closest = i
      break
    }
  }
  if(closest >0)break;
}


alk = alk[[closest]]

x11()
plot(alk$Length,alk$`0` ,
     ylim = c(0,1),
     xlab = "Length", ylab = "Prob", main = "P(age| length,haul)",
     lwd=3, col="red",type = 'l',
     cex.lab=1.5, cex.main = 1.8,cex.axis = 1.2)
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
legend(x=75,y=0.99,legend = c("0 year","1 year", "2 year","3 year","4 year","5 year",">5 year")
       ,col=c("red","grey","blue","black","darkgreen","purple","brown"),lty = 1,lwd = 3)


#idClosest = "2018:1:DEN:DAN2:GOV:172:46" #Two hauls next to each other, but wery different age-length-composistion.
#idClosest2 = "2018:1:NOR:58G2:GOV:60046:42"
#head(dat$ca_hh[dat$ca_hh$haul.id==idClosest,],10)
#head(dat$ca_hh[dat$ca_hh$haul.id==idClosest2,],10)


#idClosest = "2018:1:DEN:DAN2:GOV:172:46"
#dat$hh[dat$hh$haul.id==idClosest,]

# par(mfrow = c(1,2))
# plot(dat$ca_hh[dat$ca_hh$haul.id==idClosest2,]$LngtCm,dat$ca_hh[dat$ca_hh$haul.id==idClosest2,]$Age)
# plot(dat$ca_hh[dat$ca_hh$haul.id==idClosest,]$LngtCm,dat$ca_hh[dat$ca_hh$haul.id==idClosest,]$Age)
# dat$hh[dat$hh$haul.id==idClosest,]
# dat$hh[dat$hh$haul.id==idClosest2,]
# head(dat$ca_hh[dat$ca_hh$haul.id==idClosest,],10)
# head(dat$ca_hh[dat$ca_hh$haul.id==idClosest2,],10)


caInt = dat$ca_hh[dat$ca_hh$haul.id==idClosest,]
points(caInt$LngtCm[caInt$Age==1],1/10 + runif(sum(caInt$Age==1))*0.01,pch = 16, col = 'grey')
points(caInt$LngtCm[caInt$Age==2],2/10 + runif(sum(caInt$Age==2))*0.01,pch =16, col = 'blue')
points(caInt$LngtCm[caInt$Age==3],3/10 + runif(sum(caInt$Age==3))*0.01,pch=16,col = 'black')
points(caInt$LngtCm[caInt$Age==4],4/10 + runif(sum(caInt$Age==4))*0.01,pch=16,col = 'darkgreen')
points(caInt$LngtCm[caInt$Age==5],5/10 + runif(sum(caInt$Age==5))*0.01,pch=16,col = 'purple')
points(caInt$LngtCm[caInt$Age>5],6/10 + runif(sum(caInt$Age>5))*0.01,pch=16,col = 'brown')



caInt[caInt$LngtCm & caInt$Age==4,]



#Plot spatial effect----------------------------------------
polygons <- readOGR("C:/Users/a5415/Documents/IBTSgithub/inst/shapefiles/Roundfish_shapefiles")
#rfa <- readOGR(paste(dataDir,"/shapefiles/",sep =""),"Roundfish_shapefiles")
data(countriesHigh)
map <- countriesHigh


report = fit$obj$report()
tau = exp(report$logTau)
d1 = report$x1/tau[2]
d2 = report$x2/tau[3]
d3 = report$x3/tau[4]
d4 = report$x4/tau[5]
d5 = report$x5/tau[6]
projXY = fit$projXY
beta0 = report$beta0
repLength = report$repLength

length = 39
maxLength = confALK(species = species,quarter = quarter)$maxLength
nu1 = exp(beta0[2]+ repLength[length +maxLength*1] + d1)
nu2 = exp(beta0[3]+ repLength[length +maxLength*2] + d2)
nu3 = exp(beta0[4]+ repLength[length +maxLength*3] + d3)
nu4 = exp(beta0[5]+ repLength[length +maxLength*4] + d4)
nu5 = exp(beta0[6]+ repLength[length +maxLength*5] + d5)

if(quarter ==1){
  sum = nu2 + nu3 + nu4 + nu5
  if(length<fit$boarder){
    p1 = 1/(1+sum)
    p6 = rep(0,length(nu2))
  }else{
    p1 = rep(0,length(nu2))
    p6 = 1/(1+sum)
  }
  p2 = nu2/(1+sum)
  p3 = nu3/(1+sum)
  p4 = nu4/(1+sum)
  p5 = nu5/(1+sum)
}

xlim = c(-5,16)
ylim = c(49,62.5)
breaks   = c(0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.01)

x11()

xVidde = 5.5;yVidde = 4.5;mainVidde = 3.5
par(mfrow = c(1,2),mar=c(xVidde,yVidde,mainVidde,3.2))

low = length-6
upp = length+6
hvilke3 = which(dat$ca_hh$Age ==3 & dat$ca_hh$LngtCm>low &dat$ca_hh$LngtCm<upp)
hvilke2 = which(dat$ca_hh$Age ==2 & dat$ca_hh$LngtCm>low &dat$ca_hh$LngtCm<upp)
hvilke1 = which(dat$ca_hh$Age ==1 & dat$ca_hh$LngtCm>low &dat$ca_hh$LngtCm<upp)
noize1 = runif(length(hvilke1))*0.2
noize12 = runif(length(hvilke1))*0.2
noize2 = runif(length(hvilke2))*0.2
noize22 = runif(length(hvilke2))*0.2
noize3 = runif(length(hvilke3))*0.2
noize32 = runif(length(hvilke3))*0.2


image.plot(projXY$x,projXY$y, inla.mesh.project(projXY, p2),col =  colorRampPalette(c("white","yellow", "red"))(10),
           main = paste("Prob of age 2 given ", length+1, " cm",sep = ""), xlab = 'Degrees east', ylab = 'Degrees north',
           cex.lab = 2.1,cex.axis = 2.1, cex.main=2.5, cex.sub= 2.1,
           xlim = xlim,
           ylim = ylim,
           breaks   =breaks)

contour(projXY$x, projXY$y,inla.mesh.project(projXY, p2) ,add = T,labcex  = 1,cex = 1,
        breaks   = breaks,
        zlim = c(0.005,0.9))
plot(map, col="grey", add=T)
if (!is.null(polygons)){
  plot(polygons, add=T,lwd = 3)
}
pointLabel(coordinates(polygons),labels=polygons$AreaName)


points(dat$ca_hh$lon[hvilke3] + noize3,dat$ca_hh$lat[hvilke3]+ noize32,col = 'green' )
points(dat$ca_hh$lon[hvilke2] +noize2,dat$ca_hh$lat[hvilke2] +noize22,col = 'blue' )
points(dat$ca_hh$lon[hvilke1] + noize1,dat$ca_hh$lat[hvilke1] + noize12,col = 'brown' )





image.plot(projXY$x,projXY$y, inla.mesh.project(projXY, p3),col =  colorRampPalette(c("white","yellow", "red"))(10),
           main = paste("Prob of age 3 given ", length+1, " cm",sep = ""), xlab = 'Degrees east', ylab = 'Degrees north',
           cex.lab = 2.1,cex.axis = 2.1, cex.main=2.5, cex.sub= 2.1,
           xlim = xlim,
           ylim = ylim,
           breaks   =breaks)

contour(projXY$x, projXY$y,inla.mesh.project(projXY, p3) ,add = T,labcex  = 1,cex = 1,
        breaks   = breaks,
        zlim = c(0.005,0.9))
plot(map, col="grey", add=T)
if (!is.null(polygons)){
  plot(polygons, add=T,lwd = 3)
}
pointLabel(coordinates(polygons),labels=polygons$AreaName)



points(dat$ca_hh$lon[hvilke3] + noize3,dat$ca_hh$lat[hvilke3]+ noize32,col = 'green' )
points(dat$ca_hh$lon[hvilke2] +noize2,dat$ca_hh$lat[hvilke2] +noize22,col = 'blue' )
points(dat$ca_hh$lon[hvilke1] + noize1,dat$ca_hh$lat[hvilke1] + noize12,col = 'brown' )

#-----------------------------------------------------------------



















#
#
#
# image.plot(projXY$x,projXY$y, inla.mesh.project(projXY, p4),col =  colorRampPalette(c("white","yellow", "red"))(10),
#            main = paste("Prob of age 1 given ", length+1, " cm",sep = ""), xlab = 'Degrees east', ylab = 'Degrees north',
#            cex.lab = 1.1,cex.axis = 1.1, cex.main=1, cex.sub= 1.1,
#            xlim = xlim,
#            ylim = ylim,
#            breaks   =breaks)
#
# contour(projXY$x, projXY$y,inla.mesh.project(projXY, p4) ,add = T,labcex  = 1,cex = 1,
#         breaks   = breaks,
#         zlim = c(0.005,0.9))
# plot(map, col="grey", add=T)
# if (!is.null(polygons)){
#   plot(polygons, add=T,lwd = 3)
# }
# pointLabel(coordinates(polygons),labels=polygons$AreaName)
#
# image.plot(projXY$x,projXY$y, inla.mesh.project(projXY, p5),col =  colorRampPalette(c("white","yellow", "red"))(10),
#            main = paste("Prob of age 1 given ", length+1, " cm",sep = ""), xlab = 'Degrees east', ylab = 'Degrees north',
#            cex.lab = 1.1,cex.axis = 1.1, cex.main=1, cex.sub= 1.1,
#            xlim = xlim,
#            ylim = ylim,
#            breaks   =breaks)
#
# contour(projXY$x, projXY$y,inla.mesh.project(projXY, p5) ,add = T,labcex  = 1,cex = 1,
#         breaks   = breaks,
#         zlim = c(0.005,0.9))
# plot(map, col="grey", add=T)
# if (!is.null(polygons)){
#   plot(polygons, add=T,lwd = 3)
# }
# pointLabel(coordinates(polygons),labels=polygons$AreaName)
#
#

