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

dat<- readExchangeDir()


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

y = 13
windows(height = 15, width = 25.5)

#Extract the haul.id s you want. Here selection by species--------------------------
haulselection <- unlist(unique(dat[["HL"]][!is.na(dat[["HL"]]$Species) & dat[["HL"]]$Species=="Gadus morhua","haul.id"]))


#.. species and age samples-------------------------------
haulselection_age <- unlist(unique(dat[["CA"]][!is.na(dat[["CA"]]$Species) & dat[["CA"]]$Species=="Gadus morhua" & !is.na(dat[["CA"]]$Age),"haul.id"]))


#extract haul id s with age and length for species-----------------------------------
haulselection_age_length <- unlist(unique(dat[["CA"]][!is.na(dat[["CA"]]$Species) & dat[["CA"]]$Species=="Gadus morhua" & !is.na(dat[["CA"]]$Age) & !is.na(dat[["CA"]]$LngtCm),"haul.id"]))



#if all age dont have lengths return warning-------------------------------

if (!all(haulselection_age %in% haulselection_age_length)){
  warning("Not all age readings have length")
}


#.. species and length samples------------------------------------------
haulselection_length <- unlist(unique(dat[["HL"]][!is.na(dat[["HL"]]$Species) & dat[["HL"]]$Species=="Gadus morhua" & !is.na(dat[["HL"]]$LngtCm),"haul.id"]))


country.haul_species_length <- subset(dat,
#                                     Country ==country.names[c],
                                      Year == year.no[y],
                                      haul.id %in% unlist(haulselection_length))
plot(country.haul_species_length)

#filter dat on haul.id (and country and year) and plot
country.haul_species <- subset(dat,
                               #                               Country ==country.names[c],
                               Year == year.no[y],
                               haul.id %in% unlist(haulselection_age_length))
plot(country.haul_species, add=T, col="blue")

#Number of hauls in the above plot:--------------------------------
nrow(country.haul_species_length[["HH"]])



country.haul_species_age <- subset(dat,
#                                  Country ==country.names[c],
                                   Year == year.no[y],
                                   haul.id %in% unlist(haulselection_age))
plot(country.haul_species_age, add=T, col="red")

#Number of hauls in the above plot:------------------------------------
nrow(country.haul_species_age[["HH"]])



################################
#YOUR PLOT
################################

#Select the year of interest------------
yearOfInterest = 2016
d <- subset(dat1, Year == yearOfInterest)
ca = d[["CA"]]
hh = d[["HH"]]
hl = d[["HL"]]


#Select the species of interest--------
speciesofInterest = "Gadus morhua"
ca = ca[ca$Species == speciesofInterest,]
hl = hl[hl$Species == speciesofInterest,]
#--------------------------------------


#Find rawl hauls which are both in CA and HL---
haulsWithAge = unique(ca$haul.id)
haulsWithAge = haulsWithAge[!is.na(haulsWithAge)]
l = rep(NA,length(haulsWithAge))
for(i in 1:length(haulsWithAge)){
  l[i] = which(hh$haul.id==haulsWithAge[i])
}
#--------------------------------------

#--------------------------------------
windows(height = 15, width = 25.5)

plot(dat1,type = "n")
title(main = "Title")
points(hh$lon,hh$lat,col = 'blue', lwd = 2)
points(hh$lon[l],hh$lat[l],col = 'red', lwd = 2) #These are the trawl hauls with age information

legend("bottomright", pch = 1, legend = c("With age information","Only length information"), col= c("red","blue"))


