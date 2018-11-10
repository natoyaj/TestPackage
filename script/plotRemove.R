path = "Papers/manuscript/results/runOlav/"
remove1 = readRDS(paste(path, "RemovalCodDl1year2018Q1nSim1000haulBased",sep = ""))
remove2 = readRDS(paste(path, "RemovalCodDl2year2018Q1nSim1000haulBased",sep = ""))
remove3 = readRDS(paste(path, "RemovalCodDl3year2018Q1nSim1000haulBased",sep = ""))
remove4 = readRDS(paste(path, "RemovalCodDl4year2018Q1nSim1000haulBased",sep = ""))
remove5 = readRDS(paste(path, "RemovalCodDl5year2018Q1nSim1000haulBased",sep = ""))
original =readRDS(paste(path, "cod2018Q1n1000haulBased",sep = ""))

dS = matrix(0,5,7)
for(i in 1:5){
  eval(parse(text = paste("dS[i,] = remove", i,"$mCPUE$sd/ original$sd*100",sep = "")))
}

plot(0:5,c(0,dS[,2]),type = 'l',
     xlab = "Lenght group width",
     ylab = "Removal_SD in percentage of mCPUE_SD",
     main = "Removing otoliths cod",
     ylim = c(0,150),lwd = 3,
     col = "grey")
legend(x=0.1,y=150,legend = c("1 year", "2 year","3 year","4 year","5 year",">5 year")
       ,col=c("grey","blue","black","darkgreen","purple","red"),lty = 1,lwd = 3,box.lty =0)

points(0:5,c(0,dS[,3]),type = 'l',
     lwd = 3,
     col = "blue")

points(0:5,c(0,dS[,4]),type = 'l',
     lwd = 3,
     col = "black")

points(0:5,c(0,dS[,5]),type = 'l',
     lwd = 3,
     col = "darkgreen")

points(0:5,c(0,dS[,6]),type = 'l',
     lwd = 3,
     col = "purple")
points(0:5,c(0,dS[,7]),type = 'l',
     lwd = 3,
     col = "red")




remove1 = readRDS(paste(path, "RemovalSaitheDl1year2017Q3nSim1000haulBased",sep = ""))
remove2 = readRDS(paste(path, "RemovalSaitheDl2year2017Q3nSim1000haulBased",sep = ""))
remove3 = readRDS(paste(path, "RemovalSaitheDl3year2017Q3nSim1000haulBased",sep = ""))
remove4 = readRDS(paste(path, "RemovalSaitheDl4year2017Q3nSim1000haulBased",sep = ""))
remove5 = readRDS(paste(path, "RemovalSaitheDl5year2017Q3nSim1000haulBased",sep = ""))
original =readRDS(paste(path, "Saithe2017Q3n1000haulBased",sep = ""))

dS = matrix(0,5,7)
for(i in 1:5){
  eval(parse(text = paste("dS[i,] = remove", i,"$mCPUE$sd/ original$sd*100",sep = "")))
}

plot(0:5,c(0,dS[,1]),type = 'l',
     xlab = "Lenght group width",
     ylab = "Removal_SD in percentage of mCPUE_SD",
     main = "Removing otoliths saithe",
     ylim = c(0,150),lwd = 3,
     col = "grey")
legend(x=0.1,y=150,legend = c("1 year", "2 year","3 year","4 year","5 year",">5 year")
       ,col=c("grey","blue","black","darkgreen","purple","red"),lty = 1,lwd = 3,box.lty =0)

points(0:5,c(0,dS[,2]),type = 'l',
       lwd = 3,
       col = "blue")

points(0:5,c(0,dS[,3]),type = 'l',
       lwd = 3,
       col = "blue")

points(0:5,c(0,dS[,4]),type = 'l',
       lwd = 3,
       col = "black")

points(0:5,c(0,dS[,5]),type = 'l',
       lwd = 3,
       col = "darkgreen")

points(0:5,c(0,dS[,6]),type = 'l',
       lwd = 3,
       col = "purple")
points(0:5,c(0,dS[,7]),type = 'l',
       lwd = 3,
       col = "red")


