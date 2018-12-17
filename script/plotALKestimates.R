path = "Papers/manuscript/results/olav/"
AreaBasedResults = readRDS(paste(path, "cod2018Q1n1000datras",sep = ""))
HaulBasedResults = readRDS(paste(path, "cod2018Q1n1000haulBased",sep = ""))
ModelBasedResults = readRDS(paste(path, "cod2018Q1n1000modelBased",sep = ""))


AreaBasedResults = readRDS(paste(path, "saithe2018Q1n1000haulBased",sep = ""))
HaulBasedResults = readRDS(paste(path, "saithe2018Q1n1000haulBased",sep = ""))
ModelBasedResults = readRDS(paste(path, "saithe20181n1000haulBased",sep = ""))




#--------2018

library(dplyr)
library(Publish)
library(plotrix)
library(ggplot2)

a=c(1,2,3,4,5,"6+")
Species = rep("cod",18)
#Species = rep("saithe",18)
ALK = rep(c("Area based", "Haul based", "Model based"), each=6)
BootstrapProcedure = rep(c("Datras", "Stratified"), each=7)

#calculates relative standard error
rseDatras = (AreaBasedResults[,8]/AreaBasedResults[,1])*100
rseHaul   = (HaulBasedResults[,8]/HaulBasedResults[,1])*100
rseModel  = (ModelBasedResults[,8]/ModelBasedResults[,1])*100

#calculates width of confidence interval
CIDatras = AreaBasedResults[,7]-AreaBasedResults[,6]
CIHaul   = HaulBasedResults[,7]-HaulBasedResults[,6]
CIModel  = ModelBasedResults[,7]-ModelBasedResults[,6]


Areab= cbind(AreaBasedResults[-1,],rseDatras[-1], CIDatras[-1])
colnames(Areab) = c("mCPUE", "boostrapMean","median", "Q025", "Q975", "BiasCQ025", "BiasCQ975", "sd", "RSE%", "Width CI-95")

Haulb= cbind(HaulBasedResults[-1,],rseHaul[-1], CIHaul[-1])
colnames(Haulb) = c("mCPUE", "boostrapMean","median", "Q025", "Q975", "BiasCQ025", "BiasCQ975", "sd", "RSE%", "Width CI-95")

Modelb = cbind(ModelBasedResults[-1,],rseModel[-1],CIModel[-1])
colnames(Modelb) = c("mCPUE", "boostrapMean","median", "Q025", "Q975", "BiasCQ025", "BiasCQ975", "sd", "RSE%", "Width CI-95")


# combined ALKs.............................
AreaHaulModel = as.data.frame(cbind(ALK, Species, a, rbind(Areab, Haulb, Modelb)))
colnames(AreaHaulModel) = c("ALK", "Species", "Age", "lambda.hat_a", "Mean","Median",
                                  "PercentileQ025", "PercentileQ975","BiascorrectedQ025", "BiascorrectedQ975", "SE", "RSE%", "Width CI-95")


tab<-AreaHaulModel[,c("lambda.hat_a", "BiascorrectedQ025", "BiascorrectedQ975")]
names(tab)<-c("lambda.hat_a","lower","upper")

#dividing the dataframe
labels <- split(format(AreaHaulModel[,c("Age","Species", "SE", "RSE%", "Width CI-95")], digits=3),
                AreaHaulModel[,c("ALK")])


#pathFigures = "Papers/manuscript/figures/"
#jpeg(paste(pathFigures,"codALKs2018Q1n1000.jpeg", sep=""))
dev.new()
plotConfidence(x=tab,
               labels=labels,xratio=c(0.40,0.15),leftmargin=0.003,rightmargin=0.003,
               cex=3.0, pch=20, xlim = c(0,20), lwd =3, order=c(1,3,2),
               xlab = "Abundance at age",xaxis.lwd=2,xaxis.cex=2.2,points.col=rainbow(1), y.offset = c(0,0),
               y.title.offset = 2.0, section.sep = 0.8)
#dev.off()

