require(MASS)
library(MASS)
require(data.table)
library(data.table)
require(stats4)
require(stats)
library(stats)
library(stats4)
library(base)
require(base)
library(dplyr)
require(dplyr)
require(plyr)
library(plyr)
library(ade4)
require(ade4)
library(maptools)
require(maptools)
require(sp)
library(plotrix)
require(plotrix)
require(maps)
require(mapdata)
#install.packages('DATRAS',repos='http://www.rforge.net/',type='source')
library(DATRAS)
require(DATRAS)
library(RColorBrewer)
library(mgcv)
require(mgcv)

#++++++++++++++++++++++++++++++++++++++++++++++++++
# Get data and merging by CA, HH, HL
# downloadExchange("NS-IBTS",2004:2017)
# print(table(ca$Year))
# d <- readExchange("Exchange")
#-9 = Missing Value
#.  = 1 mm length class, reporting units: mm
#0  = 0.5 cm length class, reporting units: mm
#1  = 1 cm length class, reporting units: cm
#2  = 2 cm length class, reporting units: cm
#5  = 5 cm length class, reporting units: cm
#9  = + group
#++++++++++++++++++++++++++++++++++++++++++++++++++

#To Download exchange data:
#downloadExchange("NS-IBTS",2004:2017)

dataDir <<- system.file("data", package = "TestPackage")

dat<- readExchangeDir(dataDir)


#Get specific sub-dataframe from dataframe--------------

ca <- dat[["CA"]]
hl <- dat[["HL"]]
hh <- dat[["HH"]]

#gives the names of variables in sub-dataframe---------

names(dat[["CA"]])
names(dat[["HL"]])
names(dat[["HH"]])



# MERGING hh and hl data-------------------------------

hh_keys <- c("haul.id")
hl_keys <- c(hh_keys, c("LngtClas", "Species")) #

ca_hh    <- merge(ca,hh, by=hh_keys, suffixes=c(".CA", ""))
ca_hl    <- merge(ca,hl, by=hl_keys, suffixes=c(".CA", ""))
hl_hh    <- merge(hl,hh, by=hh_keys, suffixes=c(".HL", ""))

ca_hl_hh <- merge(ca, hl_hh, by=hl_keys, suffixes=c(".CA", ""))



##########################
#MY PLOT
##########################

country.names <- unique(dat$Country)
year.no       <- unique(dat$Year)
roundfish     <- unique(dat$Roundfish[!is.na(dat$Roundfish)])
haulsAge      <- length(roundfish)
haulsLength   <- length(roundfish)

yearvec <- c("2004", "2005","2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015","2016", "2017")
RFvec <- c("1", "2","3", "4", "5", "6", "7", "8", "9", "10")
c = 1
y = 13
#R = 2

windows(height = 17, width = 25.5)
par(mfrow=c(3,4), tcl=-0.9, family="serif", omi=c(0.3,0.2,0.2,0.3)) #, oma=c(4.5, 4, 4, 2.5), mar=rep(.1, 4), cex=1, las=1)

species = "Gadus morhua"

for(R in 1:length(roundfish))
{
  print(R)

  #Extract the haul.id s you want. Here selection by species--------------------------
  haulselection <- unlist(unique(dat[["HL"]][!is.na(dat[["HL"]]$Species) & dat[["HL"]]$Species==species,"haul.id"]))


  #.. species and age samples-------------------------------
  haulselection_age <- unlist(unique(dat[["CA"]][!is.na(dat[["CA"]]$Species) & dat[["CA"]]$Species==species & !is.na(dat[["CA"]]$Age),"haul.id"]))


  #extract haul id s with age and length for species-----------------------------------
  haulselection_age_length <- unlist(unique(dat[["CA"]][!is.na(dat[["CA"]]$Species) & dat[["CA"]]$Species==species & !is.na(dat[["CA"]]$Age) & !is.na(dat[["CA"]]$LngtCm),"haul.id"]))



  #if all age dont have lengths return warning-------------------------------

  if (!all(haulselection_age %in% haulselection_age_length)){
    warning("Not all age readings have length")
  }


  #.. species and length samples------------------------------------------
  haulselection_length <- unlist(unique(dat[["HL"]][!is.na(dat[["HL"]]$Species) & dat[["HL"]]$Species==species & !is.na(dat[["HL"]]$LngtCm),"haul.id"]))


  #filter dat on haul.id (and country and year) and plot
  country.haul_species <- subset(dat,
                                 #                              Country ==country.names[c],
                                 Roundfish ==roundfish[R],
                                 Year == year.no[y],
                                 haul.id %in% unlist(haulselection))
  plot(country.haul_species)



  country.haul_species_length <- subset(dat,
                                        #                                     Country ==country.names[c],
                                        Roundfish ==roundfish[R],
                                        Year == year.no[y],
                                        haul.id %in% unlist(haulselection_length))
  plot(country.haul_species_length, add=T, col="blue")

  #Number of hauls in the above plot:--------------------------------
  haulsLength[R] <- nrow(country.haul_species_length[["HH"]])



  country.haul_species_age <- subset(dat,
                                     #                                  Country ==country.names[c],
                                     Roundfish ==roundfish[R],
                                     Year == year.no[y],
                                     haul.id %in% unlist(haulselection_age))
  plot(country.haul_species_age, add=T, col="red")

  title(main=paste("Gadus morhua in RF Area", RFvec[R]), line = 3)

  #title(main = "Gadus morhua in RF area",R, line = 3)

  #Number of hauls in the above plot:------------------------------------
  haulsAge[R] <-  nrow(country.haul_species_age[["HH"]])


  labelsZ=parse(text=paste(seq(30,62,1), sep=""))
  axis(side = 4, at = seq(30, 62, by =1), labels =labelsZ)

  axis(side = 3, at = seq(-4, 13, by =1)-0.35, labels=c("E5", "E6", "E7", "E8", "E9", "F0",
                                                        "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "G0","G1", "G2"), tick =FALSE)
  #axis(side = 3, at = seq(-4, 13, by =1), labels=FALSE, tick =TRUE)


  # axis(side = 3, at = seq(-5, 13, by =1))

  ticks=parse(text=paste(abs(seq(-4, 13, 1)), "^o ", sep=""))
  # axis(side = 1, at = seq(-4, 12, 2), labels = ticks)

  labelsY=parse(text=paste(seq(30,62,5), "^o ", sep=""))
  # axis(side = 2, at = seq(50, 62, by =2),  labels =  labelsY)

}

plot_colors <- c("red","blue")
text <- c("Hauls with age and length data","hauls with length data")
plot.new()
par(xpd=TRUE)
legend("center", legend = text,col=plot_colors, pch=16, cex=1,horiz = FALSE)
par(xpd=FALSE)

dev.print(pdf, file="ENG.pdf");
#dev.off()

haulsLength
haulsAge


################################
#YOUR PLOT
################################
#Select the year of interest------------
yearOfInterest = 2016
d <- subset(dat, Year == yearOfInterest)
ca = d[["CA"]]
hh = d[["HH"]]
hl = d[["HL"]]
#--------------------------------------

#Select the species of interest--------
speciesofInterest = "Gadus morhua"
ca = ca[ca$Species == speciesofInterest,]
hl = hl[hl$Species == speciesofInterest,]
#--------------------------------------

#Find rawl hauls which are both in CA and HL---
haulsWithAge = unique(ca$haul.id[which(!is.na(ca$Age))])
haulsWithAge = haulsWithAge[!is.na(haulsWithAge)]
l1 = rep(NA,length(haulsWithAge))
for(i in 1:length(haulsWithAge)){
  l1[i] = which(hh$haul.id==haulsWithAge[i])
}

haulsWithLength = unique(hl$haul.id[which(!is.na(hl$LngtCm))])
haulsWithLength = haulsWithLength[!is.na(haulsWithLength)]
l2 = rep(NA,length(haulsWithLength))
for(i in 1:length(haulsWithLength)){
  l2[i] = which(hh$haul.id==haulsWithLength[i])
}
#--------------------------------------

x11()
plot(dat,type = "n")
title(main = "Title")
points(hh$lon[l2],hh$lat[l2],col = 'blue', lwd = 2)
points(hh$lon[l1],hh$lat[l1],col = 'red', lwd = 2) #These are the trawl hauls with age information

legend("bottomright", pch = 1, legend = c("With age information","Only length information"), col= c("red","blue"))

