path = "Papers/manuscript/results/olav/"
remove1 = readRDS(paste(path, "RemovalCodDl1year2018Q1n100haulBased",sep = ""))
remove2 = readRDS(paste(path, "RemovalCodDl2year2018Q1n100haulBased",sep = ""))
remove3 = readRDS(paste(path, "RemovalCodDl3year2018Q1n100haulBased",sep = ""))
remove4 = readRDS(paste(path, "RemovalCodDl4year2018Q1n100haulBased",sep = ""))
remove5 = readRDS(paste(path, "RemovalCodDl5year2018Q1n100haulBased",sep = ""))
remove10 = readRDS(paste(path, "RemovalCodDl10year2018Q1n100haulBased",sep = ""))
remove20 = readRDS(paste(path, "RemovalCodDl20year2018Q1n100haulBased",sep = ""))
remove30 = readRDS(paste(path, "RemovalCodDl30year2018Q1n100haulBased",sep = ""))
remove40 = readRDS(paste(path, "RemovalCodDl40year2018Q1n100haulBased",sep = ""))
remove50 = readRDS(paste(path, "RemovalCodDl50year2018Q1n100haulBased",sep = ""))
remove60 = readRDS(paste(path, "RemovalCodDl60year2018Q1n100haulBased",sep = ""))
remove70 = readRDS(paste(path, "RemovalCodDl70year2018Q1n100haulBased",sep = ""))
original =readRDS(paste(path, "cod2018Q1n200haulBased",sep = ""))


path = "Papers/manuscript/results/olav/"
remove1 = readRDS(paste(path, "RemovalSaitheDl1year2017Q3n100haulBased",sep = ""))
remove2 = readRDS(paste(path, "RemovalSaitheDl2year2017Q3n100haulBased",sep = ""))
remove3 = readRDS(paste(path, "RemovalSaitheDl3year2017Q3n100haulBased",sep = ""))
remove4 = readRDS(paste(path, "RemovalSaitheDl4year2017Q3n100haulBased",sep = ""))
remove5 = readRDS(paste(path, "RemovalSaitheDl5year2017Q3n100haulBased",sep = ""))
remove10 = readRDS(paste(path, "RemovalSaitheDl10year2017Q3n100haulBased",sep = ""))
remove20 = readRDS(paste(path, "RemovalSaitheDl20year2017Q3n100haulBased",sep = ""))
remove30 = readRDS(paste(path, "RemovalSaitheDl30year2017Q3n100haulBased",sep = ""))
remove40 = readRDS(paste(path, "RemovalSaitheDl40year2017Q3n100haulBased",sep = ""))
remove50 = readRDS(paste(path, "RemovalSaitheDl50year2017Q3n100haulBased",sep = ""))
remove60 = readRDS(paste(path, "RemovalSaitheDl60year2017Q3n100haulBased",sep = ""))
remove70 = readRDS(paste(path, "RemovalSaitheDl70year2017Q3n100haulBased",sep = ""))
remove80 = readRDS(paste(path, "RemovalSaitheDl80year2017Q3n100haulBased",sep = ""))
remove90 = readRDS(paste(path, "RemovalSaitheDl90year2017Q3n100haulBased",sep = ""))
remove100 = readRDS(paste(path, "RemovalSaitheDl100year2017Q3n100haulBased",sep = ""))
original =readRDS(paste(path, "Saithe2017Q3n100haulBased",sep = ""))


#Plot changes in KI
nRemove = 6
dKIL = matrix(0,nRemove,7)
dKIU = matrix(0,nRemove,7)
dKIM = matrix(0,nRemove,7)
tmp = c(1,2,3,4,5,10,20,30,40,50,60,70)
tmp = 1:5
dKIL[1,] = original$Q025
dKIU[1,] = original$Q975
dKIM[1,] = original$bootstrapMean

for(i in 2:nRemove){
  j = tmp[i-1]
  eval(parse(text = paste("dKIL[i,] = remove", j,"$mCPUE$Q025",sep = "")))
  eval(parse(text = paste("dKIU[i,] = remove", j,"$mCPUE$Q975",sep = "")))
  eval(parse(text = paste("dKIM[i,] = remove", j,"$mCPUE$mean",sep = "")))
}

jpeg("removalCodModelKI.jpeg",width = 680, height = 680)
plot(1:nRemove,dKIM[,2],type = 'l',
     xlab = "Lenght group width",
     ylab = "mCPUE",
     main = "Removing otoliths using haulbased ALK",
     ylim = c(0,10),lwd = 5,
     col = "grey",
     xaxt = 'n')
axis(1, at = 1:nRemove,labels = c("current",tmp))
legend(x=10,y=10,legend = c("1 year", "2 year","3 year","4 year","5 year",">5 year")
       ,col=c("grey","blue","black","darkgreen","purple","red"),lty = 1,lwd = 3,box.lty =0)
points(1:nRemove,dKIL[,2],type = 'l',
       lwd = 1,lty = 2,
       col = "grey")
points(1:nRemove,dKIU[,2],type = 'l',
       lwd = 1,lty = 2,
       col = "grey")

for(j in 3:4){
  if(j ==3) col = "blue"
  if(j ==4)col = "black"
  if(j ==5)col = "darkgreen"
  if(j ==6)col = "purple"
  if(j ==7)col = "red"
  points(1:nRemove,dKIM[,j],type = 'l',
         lwd = 5,
         col = col)
  points(1:nRemove,dKIL[,j],type = 'l',
         lwd = 1,lty = 2,
         col = col)
  points(1:nRemove,dKIU[,j],type = 'l',
         lwd = 1,lty = 2,
         col = col)
}
abline(h = 0)

dev.off()

jpeg("removalCodModelKI2.jpeg",width = 680, height = 680)
plot(1:nRemove,dKIM[,5],type = 'l',
     xlab = "Lenght group width",
     ylab = "mCPUE",
     main = "Removing otoliths using haulbased ALK",
     ylim = c(0,3),lwd = 5,
     col = "darkgreen",
     xaxt = 'n')
axis(1, at = 1:nRemove,labels = c("current",tmp))
legend(x=1,y=3,legend = c("1 year", "2 year","3 year","4 year","5 year",">5 year")
       ,col=c("grey","blue","black","darkgreen","purple","red"),lty = 1,lwd = 3,box.lty =0)
points(1:nRemove,dKIL[,5],type = 'l',
       lwd = 1,lty = 2,
       col = "darkgreen")
points(1:nRemove,dKIU[,5],type = 'l',
       lwd = 1,lty = 2,
       col = "darkgreen")

for(j in 6:7){
  if(j ==3) col = "blue"
  if(j ==4)col = "black"
  if(j ==5)col = "darkgreen"
  if(j ==6)col = "purple"
  if(j ==7)col = "red"
  points(1:nRemove,dKIM[,j],type = 'l',
         lwd = 5,
         col = col)
  points(1:nRemove,dKIL[,j],type = 'l',
         lwd = 1,lty = 2,
         col = col)
  points(1:nRemove,dKIU[,j],type = 'l',
         lwd = 1,lty = 2,
         col = col)
}
abline(h = 0)
dev.off()




#Plot proprtions calculated with different ALK steps
nWithDatras = rep(-1,99)
nWithoutDatras= rep(-1,99)
nFoundWithin = rep(-1,99)
nNotFoundWithin= rep(-1,99)

nOtolithsRemoved= rep(-1,99)
nOtolithsTotal= rep(-1,99)

counter = 1
set.seed(131455)
dlSec = c(1:5)
for(dl in dlSec){

  eval(parse(text = paste("removeOtoliths = remove",dl,sep = "")))
  nWithDatras[counter] = mean(attributes(removeOtoliths)$nWithDatras)
  nWithoutDatras[counter] = mean(attributes(removeOtoliths)$nWithoutDatras)
  nFoundWithin[counter] = mean(attributes(removeOtoliths)$nFoundWithin)
  nNotFoundWithin[counter] = mean(attributes(removeOtoliths)$nNotFoundWithin)

  nOtolithsRemoved[counter] = mean(attributes(removeOtoliths)$nOtolithsRemoved)
  nOtolithsTotal[counter] = mean(attributes(removeOtoliths)$nOtolithsTotal)

  counter = counter +1
}

year = 2018;quarter = 1; species = "Gadus morhua" #NB Say which species and year
jpeg("PercentageOfOtolithsRemoved.jpeg",width = 580, height = 580)
nOtolithsRemoved = nOtolithsRemoved[nOtolithsRemoved>(-1)]
nOtolithsTotal = nOtolithsTotal[nOtolithsTotal>(-1)]
plot(dlSec,100-nOtolithsRemoved/(nOtolithsTotal)*100,
     xlab = "length group",
     ylab = "Average percentage of otoliths used",
     ylim = c(0,100),
     main = paste(species,", year ", year, "Q", quarter))
abline(h = 0)
abline(h = 100)
dev.off()



jpeg("ALKused3a.jpeg",width = 580, height = 580)
par(mfrow = c(1,2))
nWithDatras = nWithDatras[nWithDatras>(-1)]
nWithoutDatras = nWithoutDatras[nWithoutDatras>(-1)]
nFoundWithin = nFoundWithin[nFoundWithin>(-1)]
nNotFoundWithin = nNotFoundWithin[nNotFoundWithin>(-1)]
plot(dlSec,nFoundWithin/(nFoundWithin+nNotFoundWithin)*100,
     xlab = "length group",
     ylab = "Percentage of length observations mapped to age with 3a",
     ylim = c(0,100),
     main = paste(species,", year ", year, "Q", quarter))
abline(h = 0)
abline(h = 100)
plot(dlSec,nWithDatras/(nWithDatras+nWithoutDatras)*100,
     xlab = "length group",
     ylab = "Percentage of length observations mapped to age with 3d",
     ylim = c(0,100),
     main = paste(species,", year ", year, "Q", quarter))
abline(h = 0)
abline(h = 100)
dev.off()







































#Plot changes in KI
nRemove = 16
dKIL = matrix(0,nRemove,7)
dKIU = matrix(0,nRemove,7)
dKIM = matrix(0,nRemove,7)
tmp = c(1,2,3,4,5,10,20,30,40,50,60,70,80,90,100)

dKIL[1,] = original$Q025
dKIU[1,] = original$Q975
dKIM[1,] = original$bootstrapMean

for(i in 2:nRemove){
  j = tmp[i-1]
  eval(parse(text = paste("dKIL[i,] = remove", j,"$mCPUE$Q025",sep = "")))
  eval(parse(text = paste("dKIU[i,] = remove", j,"$mCPUE$Q975",sep = "")))
  eval(parse(text = paste("dKIM[i,] = remove", j,"$mCPUE$mean",sep = "")))
}

jpeg("removalSaitheModelKI.jpeg")
plot(1:nRemove,dKIM[,2],type = 'l',
     xlab = "Lenght group width",
     ylab = "mCPUE",
     main = "Removing otoliths using haulbased ALK",
     ylim = c(0,20),lwd = 5,
     col = "grey",
     xaxt = 'n')
axis(1, at = 1:nRemove,labels = c("current",tmp))
legend(x=10,y=10,legend = c("1 year", "2 year","3 year","4 year","5 year",">5 year")
       ,col=c("grey","blue","black","darkgreen","purple","red"),lty = 1,lwd = 3,box.lty =0)
points(1:nRemove,dKIL[,2],type = 'l',
       lwd = 1,lty = 2,
       col = "grey")
points(1:nRemove,dKIU[,2],type = 'l',
       lwd = 1,lty = 2,
       col = "grey")

for(j in 3:4){
  if(j ==3) col = "blue"
  if(j ==4)col = "black"
  if(j ==5)col = "darkgreen"
  if(j ==6)col = "purple"
  if(j ==7)col = "red"
  points(1:nRemove,dKIM[,j],type = 'l',
         lwd = 5,
         col = col)
  points(1:nRemove,dKIL[,j],type = 'l',
         lwd = 1,lty = 2,
         col = col)
  points(1:nRemove,dKIU[,j],type = 'l',
         lwd = 1,lty = 2,
         col = col)
}
dev.off()

jpeg("removalSaitheModelKI2.jpeg")
plot(1:nRemove,dKIM[,5],type = 'l',
     xlab = "Lenght group width",
     ylab = "mCPUE",
     main = "Removing otoliths using haulbased ALK",
     ylim = c(0,20),lwd = 5,
     col = "darkgreen",
     xaxt = 'n')
axis(1, at = 1:nRemove,labels = c("current",tmp))
legend(x=1,y=4,legend = c("1 year", "2 year","3 year","4 year","5 year",">5 year")
       ,col=c("grey","blue","black","darkgreen","purple","red"),lty = 1,lwd = 3,box.lty =0)
points(1:nRemove,dKIL[,5],type = 'l',
       lwd = 1,lty = 2,
       col = "darkgreen")
points(1:nRemove,dKIU[,5],type = 'l',
       lwd = 1,lty = 2,
       col = "darkgreen")

for(j in 6:7){
  if(j ==3) col = "blue"
  if(j ==4)col = "black"
  if(j ==5)col = "darkgreen"
  if(j ==6)col = "purple"
  if(j ==7)col = "red"
  points(1:nRemove,dKIM[,j],type = 'l',
         lwd = 5,
         col = col)
  points(1:nRemove,dKIL[,j],type = 'l',
         lwd = 1,lty = 2,
         col = col)
  points(1:nRemove,dKIU[,j],type = 'l',
         lwd = 1,lty = 2,
         col = col)
}
dev.off()


