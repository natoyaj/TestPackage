#devtools::install_github("fishfollower/SAM/stockassessment")
library(stockassessment)
library(xlsx)

path = "inst/extdata/nsSaitheAssessment/"
cn<-read.ices(paste(path,"cn.dat",sep = ""))
cw<-read.ices(paste(path,"cw.dat",sep = ""))
dw<-read.ices(paste(path,"dw.dat",sep = ""))
lf<-read.ices(paste(path,"lf.dat",sep = ""))
lw<-read.ices(paste(path,"lw.dat",sep = ""))
mo<-read.ices(paste(path,"mo.dat",sep = ""))
nm<-read.ices(paste(path,"nm.dat",sep = ""))
pf<-read.ices(paste(path,"pf.dat",sep = ""))
pm<-read.ices(paste(path,"pm.dat",sep = ""))
sw<-read.ices(paste(path,"sw.dat",sep = ""))
surveys<-read.ices(paste(path,"survey.dat",sep = ""))

dat<-setup.sam.data(surveys=surveys,
                    residual.fleet=cn,
                    prop.mature=mo,
                    stock.mean.weight=sw,
                    catch.mean.weight=cw,
                    dis.mean.weight=dw,
                    land.mean.weight=lw,
                    prop.f=pf,
                    prop.m=pm,
                    natural.mortality=nm,
                    land.frac=lf)

conf<-loadConf(dat,paste(path,"model.cfg",sep = ""), patch=TRUE)
par<-defpar(dat,conf)
fit<-sam.fit(dat,conf,par)





#Fit with indices calculated with datras procedure
index = read.xlsx("meanToJennifer.xlsx",sheetIndex = 1)
kk = t(index)
kk = data.frame(kk,stringsAsFactors = FALSE)
kk = as.matrix(as.data.frame(lapply(kk, as.numeric)))
kk = round(kk,5)
kk = kk[-1,]
colnames(kk) = as.character(0:10)
kk = kk[,as.character(3:8)]
rownames(kk) = as.character(1992:2018)
attributes(kk) = attributes(surveys$`DATRAS Q3 3-8`)
surveys[[1]] =kk

dat<-setup.sam.data(surveys=surveys,
                    residual.fleet=cn,
                    prop.mature=mo,
                    stock.mean.weight=sw,
                    catch.mean.weight=cw,
                    dis.mean.weight=dw,
                    land.mean.weight=lw,
                    prop.f=pf,
                    prop.m=pm,
                    natural.mortality=nm,
                    land.frac=lf)

conf<-loadConf(dat,paste(path,"model.cfg",sep = ""), patch=TRUE)
#conf<-defcon(dat)
par<-defpar(dat,conf)
fitDatras<-sam.fit(dat,conf,par)




#Fit with indices calculated with our procedure
index = read.xlsx("indexSaitheQ3haulBased.xlsx",sheetIndex = 1)
kk = t(index)
kk = data.frame(kk,stringsAsFactors = FALSE)
kk = as.matrix(as.data.frame(lapply(kk, as.numeric)))
kk = round(kk,5)
kk = kk[-1,]
colnames(kk) = as.character(0:10)
kk = kk[,as.character(3:8)]
rownames(kk) = as.character(1992:2018)
attributes(kk) = attributes(surveys$`DATRAS Q3 3-8`)
surveys[[1]] =kk

dat<-setup.sam.data(surveys=surveys,
                    residual.fleet=cn,
                    prop.mature=mo,
                    stock.mean.weight=sw,
                    catch.mean.weight=cw,
                    dis.mean.weight=dw,
                    land.mean.weight=lw,
                    prop.f=pf,
                    prop.m=pm,
                    natural.mortality=nm,
                    land.frac=lf)
conf<-loadConf(dat,paste(path,"model.cfg",sep = ""), patch=TRUE)
#conf<-defcon(dat)
par<-defpar(dat,conf)

fitHaulBased<-sam.fit(dat,conf,par)

pathResults = "Papers/manuscript/results/olav/"
jpeg(paste(pathResults,"SAMsaitheSSB",".jpeg", sep = ""),width = 500,height = 500)
ssbplot(c(fitDatras,fitHaulBased), addCI=TRUE,main = "SSB saithe")
dev.off()

jpeg(paste(pathResults,"SAMsaitheF",".jpeg", sep = ""),width = 500,height = 500)
fbarplot(c(fitDatras,fitHaulBased), addCI=TRUE, main = "F saithe")
dev.off()


jpeg(paste(pathResults,"SAMsaitheSSBThree",".jpeg", sep = ""),width = 500,height = 500)
ssbplot(c(fitDatras,fitHaulBased,fit), addCI=TRUE,main = "SSB saithe")
dev.off()

jpeg(paste(pathResults,"SAMsaitheFThree",".jpeg", sep = ""),width = 500,height = 500)
fbarplot(c(fitDatras,fitHaulBased,fit), addCI=TRUE,main = "F saithe")
dev.off()








#COD----------------------------------
path = "inst/extdata/nsCodAssessment/"
catch.no<-read.ices(paste(path,'cn.dat',sep = ""))
catch.mean.weight<-read.ices(paste(path,'cw.dat',sep = ""))
dis.mean.weight<-read.ices(paste(path,'dw.dat',sep = ""))
land.mean.weight<-read.ices(paste(path,'lw.dat',sep = ""))
stock.mean.weight<-read.ices(paste(path,'sw.dat',sep = ""))
prop.mature<-read.ices(paste(path,'mo_raw.dat',sep = ""))
natural.mortality<-read.ices(paste(path,'nm.dat',sep = ""))
surveys<-read.ices(paste(path,'survey.dat',sep = ""))
land.no<-read.ices(paste(path,'lf.dat',sep = ""))
dis.no<-catch.no-land.no
prop.f<-read.ices(paste(path,'pf.dat',sep = ""))
prop.m<-read.ices(paste(path,'pm.dat',sep = ""))


#Stuff from stockassessment.org to set up data-------------
# redirect output to avoid mgcv message generating warning icons
zz=file("dummy",open="wt")
sink(zz)
sink(zz,type="message")
library(mgcv)
sink(zz,type="message")
sink()
file.remove("dummy")

# Smooth maturity

skipYears=c(1:10)
columnsToSmooth=1:5
mo=prop.mature[-c(skipYears),]

for(cc in columnsToSmooth){
  ww = mo[,cc];
  tt = 1:length(ww)
  tmp = gam(ww ~ s(tt))
  mo[,cc] = predict(tmp);
}
prop.mature[-c(skipYears),]=mo

#head=readLines(paste(path,"mo_raw.dat",sep = ""),n=5);
#file="mo.dat"
#cat(paste(head[1],"smoothed"),"\n",file=file)
#cat(head[2],"\n",file=file,append=TRUE)
#cat(head[3],"\n",file=file,append=TRUE)
#cat(head[4],"\n",file=file,append=TRUE)
#cat(head[5],"\n",file=file,append=TRUE)
#write.table(round(prop.mature,4),file=file,row.names=FALSE,col.names=FALSE,append=TRUE)


# Modify to 7+ data
cutage<-6
low<-1
GE<-which(as.numeric(colnames(catch.no))>=cutage)
E<-which(as.numeric(colnames(catch.no))==cutage)
w<-catch.no[,GE]/rowSums(catch.no[,GE])
wex<-rbind(w,w[nrow(w),])
wD<-dis.no[,GE]/ifelse(rowSums(dis.no[,GE])>0,rowSums(dis.no[,GE]),1)
wL<-land.no[,GE]/rowSums(land.no[,GE])
catch.no[,E]<-rowSums(catch.no[,GE])
catch.no<-catch.no[,low:E]
prop.mature[,E]<-rowSums(prop.mature[,GE]*wex)
prop.mature<-prop.mature[,low:E]
stock.mean.weight[,E]<-rowSums(stock.mean.weight[,GE]*wex)
stock.mean.weight<-stock.mean.weight[,low:E]
catch.mean.weight[,E]<-rowSums(catch.mean.weight[,GE]*w)
catch.mean.weight<-catch.mean.weight[,low:E]
dis.mean.weight[,E]<-rowSums(dis.mean.weight[,GE]*wD)
dis.mean.weight<-dis.mean.weight[,low:E]
land.mean.weight[,E]<-rowSums(land.mean.weight[,GE]*wL)
land.mean.weight<-land.mean.weight[,low:E]
natural.mortality[,E]<-rowSums(natural.mortality[,GE]*wex)
natural.mortality<-natural.mortality[,low:E]
land.no[,E]<-rowSums(land.no[,GE])
land.no<-land.no[,low:E]
land.frac<-ifelse(catch.no>0,land.no/catch.no,1)
prop.f<-prop.f[,low:E]
prop.m<-prop.m[,low:E]


#land.frac = lf
dat<-setup.sam.data(surveys=surveys,
                    residual.fleet=catch.no,
                    prop.mature=prop.mature,
                    stock.mean.weight=stock.mean.weight,
                    catch.mean.weight=catch.mean.weight,
                    dis.mean.weight=dis.mean.weight,
                    land.mean.weight=land.mean.weight,
                    prop.f=prop.f,
                    prop.m=prop.m,
                    natural.mortality=natural.mortality,
                    land.frac=land.frac)

conf<-defcon(dat)
conf$fbarRange <- c(2,4)
conf$corFlag<-2
#conf$keyLogFsta[1,] <- 0:5 #By some reason this fails with use of our indices.
conf$keyLogFpar[2,] <- c(0:4,-1)
conf$keyLogFpar[3,] <- c(5:8,-1,-1)
conf$keyVarF[1,] <- c(0,    1,    1,    1,    1,    1)
conf$keyVarLogN <- c(0, 1, 1, 1, 1, 1)
conf$keyVarObs[1,] <- c(0,1,2,2,2,2)
conf$keyVarObs[2,] <- c(3,4,4,4,4,-1)
conf$keyVarObs[3,] <- c(5,6,6,6,-1,-1)
conf$noScaledYears <- 13
conf$keyScaledYears <- c(1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005)
conf$keyParScaledYA<-rbind(
  c(0,    0,    0,    0,    0,    0),
  c(1,    1,    1,    1,    1,    1),
  c(2,    2,    2,    2,    2,    2),
  c(3,    3,    3,    3,    3,    3),
  c(4,    4,    4,    4,    4,    4),
  c(5,    5,    5,    5,    5,    5),
  c(6,    6,    6,    6,    6,    6),
  c(7,    7,    7,    7,    7,    7),
  c(8,    8,    8,    8,    8,    8),
  c(9,    9,    9,    9,    9,    9),
  c(10, 10 ,  10 ,  10  , 10 ,  10),
  c(11,  11,   11,   11 ,  11,   11),
  c(12,   12,   12,   12,   12,   12))
par<-defpar(dat,conf)

fit<-sam.fit(dat,conf,par)
#ssbplot(fit, addCI=TRUE)



#HaulBased----
index = read.xlsx("indexCodQ1haulBased.xlsx",sheetIndex = 1)
kk = t(index)
kk = data.frame(kk,stringsAsFactors = FALSE)
kk = as.matrix(as.data.frame(lapply(kk, as.numeric)))
kk = round(kk,5)
kk = kk[-1,]
colnames(kk) = as.character(0:6)
kk = kk[,as.character(1:6)]
rownames(kk) = as.character(1983:2018)

#Indices from ages 1-5 is used in stockassessment.org in Q1
kk = kk[,1:5]

attributes(kk)$time =  attributes(surveys$`IBTS_Q1_gam`)$time
surveys$`IBTS_Q1_gam\t\t\t\t\t\t` = kk

index = read.xlsx("indexCodQ3haulBased.xlsx",sheetIndex = 1)
kk = t(index)
kk = data.frame(kk,stringsAsFactors = FALSE)
kk = as.matrix(as.data.frame(lapply(kk, as.numeric)))
kk = round(kk,5)
kk = kk[-1,]
colnames(kk) = as.character(0:6)
kk = kk[,as.character(1:6)]
rownames(kk) = as.character(1992:2018)

kk = kk[,1:4]#Indices from ages 1-4 is used in stockassessment.org in Q3
attributes(kk)$time =  attributes(surveys$`IBTS_Q3_gam`)$time

surveys$`IBTS_Q3_gam\t\t\t\t\t\t` =kk

dat<-setup.sam.data(surveys=surveys,
                    residual.fleet=catch.no,
                    prop.mature=prop.mature,
                    stock.mean.weight=stock.mean.weight,
                    catch.mean.weight=catch.mean.weight,
                    dis.mean.weight=dis.mean.weight,
                    land.mean.weight=land.mean.weight,
                    prop.f=prop.f,
                    prop.m=prop.m,
                    natural.mortality=natural.mortality,
                    land.frac=land.frac)
conf<-defcon(dat)
if(TRUE){
  conf$fbarRange <- c(2,4)
  conf$corFlag<-2
  #conf$keyLogFsta[1,] <- 0:5 #By some reason this fails with use of our indices.
  conf$keyLogFpar[2,] <- c(0:4,-1)
  conf$keyLogFpar[3,] <- c(5:8,-1,-1)
  conf$keyVarF[1,] <- c(0,    1,    1,    1,    1,    1)
  conf$keyVarLogN <- c(0, 1, 1, 1, 1, 1)
  conf$keyVarObs[1,] <- c(0,1,2,2,2,2)
  conf$keyVarObs[2,] <- c(3,4,4,4,4,-1)#By some reason the indices used in assessment is of smaller dimension.
  conf$keyVarObs[3,] <- c(5,6,6,6,-1,-1)
  conf$noScaledYears <- 13
  conf$keyScaledYears <- c(1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005)
  conf$keyParScaledYA<-rbind(
    c(0,    0,    0,    0,    0,    0),
    c(1,    1,    1,    1,    1,    1),
    c(2,    2,    2,    2,    2,    2),
    c(3,    3,    3,    3,    3,    3),
    c(4,    4,    4,    4,    4,    4),
    c(5,    5,    5,    5,    5,    5),
    c(6,    6,    6,    6,    6,    6),
    c(7,    7,    7,    7,    7,    7),
    c(8,    8,    8,    8,    8,    8),
    c(9,    9,    9,    9,    9,    9),
    c(10, 10 ,  10 ,  10  , 10 ,  10),
    c(11,  11,   11,   11 ,  11,   11),
    c(12,   12,   12,   12,   12,   12))
}

par<-defpar(dat,conf)
fitHaulBased<-sam.fit(dat,conf,par)


#Datras----
index = read.xlsx("indexCodQ1Datras.xlsx",sheetIndex = 1)
kk = t(index)
kk = data.frame(kk,stringsAsFactors = FALSE)
kk = as.matrix(as.data.frame(lapply(kk, as.numeric)))
kk = round(kk,5)
kk = kk[-1,]
colnames(kk) = as.character(0:6)
kk = kk[,as.character(1:6)]
rownames(kk) = as.character(1983:2018)

kk = kk[,1:5]#Indices from ages 1-5 is used in stockassessment.org in Q1
attributes(kk)$time =  attributes(surveys$`IBTS_Q1_gam`)$time
surveys$`IBTS_Q1_gam\t\t\t\t\t\t` = kk


index = read.xlsx("indexCodQ3Datras.xlsx",sheetIndex = 1)
kk = t(index)
kk = data.frame(kk,stringsAsFactors = FALSE)
kk = as.matrix(as.data.frame(lapply(kk, as.numeric)))
kk = round(kk,5)
kk = kk[-1,]
colnames(kk) = as.character(0:6)
kk = kk[,as.character(1:6)]
rownames(kk) = as.character(1992:2018)

kk = kk[,1:4]#Indices from ages 1-4 is used in stockassessment.org in Q3
attributes(kk)$time =  attributes(surveys$`IBTS_Q3_gam`)$time
surveys$`IBTS_Q3_gam\t\t\t\t\t\t` =kk


dat<-setup.sam.data(surveys=surveys,
                    residual.fleet=catch.no,
                    prop.mature=prop.mature,
                    stock.mean.weight=stock.mean.weight,
                    catch.mean.weight=catch.mean.weight,
                    dis.mean.weight=dis.mean.weight,
                    land.mean.weight=land.mean.weight,
                    prop.f=prop.f,
                    prop.m=prop.m,
                    natural.mortality=natural.mortality,
                    land.frac=land.frac)
conf<-defcon(dat)

if(TRUE){
  conf$fbarRange <- c(2,4)
  conf$corFlag<-2
  #conf$keyLogFsta[1,] <- 0:5 #By some reason this fails with use of our indices.
  conf$keyLogFpar[2,] <- c(0:4,-1)
  conf$keyLogFpar[3,] <- c(5:8,-1,-1)
  conf$keyVarF[1,] <- c(0,    1,    1,    1,    1,    1)
  conf$keyVarLogN <- c(0, 1, 1, 1, 1, 1)
  conf$keyVarObs[1,] <- c(0,1,2,2,2,2)
  conf$keyVarObs[2,] <- c(3,4,4,4,4,-1) #By some reason the indices used in assessment is of smaller dimension.
  conf$keyVarObs[3,] <- c(5,6,6,6,-1,-1)
  conf$noScaledYears <- 13
  conf$keyScaledYears <- c(1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005)
  conf$keyParScaledYA<-rbind(
    c(0,    0,    0,    0,    0,    0),
    c(1,    1,    1,    1,    1,    1),
    c(2,    2,    2,    2,    2,    2),
    c(3,    3,    3,    3,    3,    3),
    c(4,    4,    4,    4,    4,    4),
    c(5,    5,    5,    5,    5,    5),
    c(6,    6,    6,    6,    6,    6),
    c(7,    7,    7,    7,    7,    7),
    c(8,    8,    8,    8,    8,    8),
    c(9,    9,    9,    9,    9,    9),
    c(10, 10 ,  10 ,  10  , 10 ,  10),
    c(11,  11,   11,   11 ,  11,   11),
    c(12,   12,   12,   12,   12,   12))
}

par<-defpar(dat,conf)
fitDatras<-sam.fit(dat,conf,par)


pathResults = "Papers/manuscript/results/olav/"
jpeg(paste(pathResults,"SAMCodSSB",".jpeg", sep = ""),width = 500,height = 500)
ssbplot(c(fitDatras,fitHaulBased), addCI=TRUE,main = "SSB cod")
dev.off()
jpeg(paste(pathResults,"SAMCodF",".jpeg", sep = ""),width = 500,height = 500)
fbarplot(c(fitDatras,fitHaulBased), addCI=TRUE, main = "F cod")
dev.off()

jpeg(paste(pathResults,"SAMCodSSBThree",".jpeg", sep = ""),width = 500,height = 500)
ssbplot(c(fitDatras,fitHaulBased,fit), addCI=TRUE,main = "SSB cod")
dev.off()

jpeg(paste(pathResults,"SAMCodFThree",".jpeg", sep = ""),width = 500,height = 500)
fbarplot(c(fitDatras,fitHaulBased,fit), addCI=TRUE,main = "F cod")
dev.off()





