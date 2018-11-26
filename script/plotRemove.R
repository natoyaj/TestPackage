path = "Papers/manuscript/results/olav/"
remove1 = readRDS(paste(path, "RemovalCodDl1year2018Q1n500haulBased",sep = ""))
remove2 = readRDS(paste(path, "RemovalCodDl2year2018Q1n500haulBased",sep = ""))
remove3 = readRDS(paste(path, "RemovalCodDl3year2018Q1n500haulBased",sep = ""))
remove4 = readRDS(paste(path, "RemovalCodDl4year2018Q1n500haulBased",sep = ""))
remove5 = readRDS(paste(path, "RemovalCodDl5year2018Q1n500haulBased",sep = ""))
remove10 = readRDS(paste(path, "RemovalCodDl10year2018Q1n500haulBased",sep = ""))
remove20 = readRDS(paste(path, "RemovalCodDl20year2018Q1n500haulBased",sep = ""))
remove30 = readRDS(paste(path, "RemovalCodDl30year2018Q1n500haulBased",sep = ""))
remove40 = readRDS(paste(path, "RemovalCodDl40year2018Q1n500haulBased",sep = ""))
remove50 = readRDS(paste(path, "RemovalCodDl50year2018Q1n500haulBased",sep = ""))
remove60 = readRDS(paste(path, "RemovalCodDl60year2018Q1n500haulBased",sep = ""))
remove70 = readRDS(paste(path, "RemovalCodDl70year2018Q1n500haulBased",sep = ""))
original =readRDS(paste(path, "cod2018Q1n500haulBased",sep = ""))


#Plot changes in KI
nRemove = 13
dKIL = matrix(0,nRemove,7)
dKIU = matrix(0,nRemove,7)
dKIM = matrix(0,nRemove,7)
tmp = c(1,2,3,4,5,10,20,30,40,50,60,70)

dKIL[1,] = original$Q025
dKIU[1,] = original$Q975
dKIM[1,] = original$bootstrapMean

for(i in 2:nRemove){
  j = tmp[i-1]
  eval(parse(text = paste("dKIL[i,] = remove", j,"$mCPUE$Q025",sep = "")))
  eval(parse(text = paste("dKIU[i,] = remove", j,"$mCPUE$Q975",sep = "")))
  eval(parse(text = paste("dKIM[i,] = remove", j,"$mCPUE$mean",sep = "")))
}

jpeg("removalCodModelKI.jpeg")
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
dev.off()

jpeg("removalCodModelKI2.jpeg")
plot(1:nRemove,dKIM[,5],type = 'l',
     xlab = "Lenght group width",
     ylab = "mCPUE",
     main = "Removing otoliths using haulbased ALK",
     ylim = c(0,4),lwd = 5,
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
