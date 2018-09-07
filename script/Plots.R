









library(DATRAS)

#Read IBTS-data-----------------------------------------
dataDir <<- system.file("Data", package = "TestPackage")
dat<- readExchangeDir(dataDir)
#-------------------------------------------------------

#Extract the data frames from IBTS-data and merge them---
ca <- dat[["CA"]]
hl <- dat[["HL"]]
hh <- dat[["HH"]]

hh_keys <- c("haul.id")
hl_keys <- c(hh_keys, c("LngtClas", "Species")) #
ca_hh    <- merge(ca,hh, by=hh_keys, suffixes=c(".CA", ""))
ca_hl    <- merge(ca,hl, by=hl_keys, suffixes=c(".CA", ""))
hl_hh    <- merge(hl,hh, by=hh_keys, suffixes=c(".HL", ""))
ca_hl_hh <- merge(ca, hl_hh, by=hl_keys, suffixes=c(".CA", ""))
#---------------------------------------------------------



country.names <- unique(dat$Country)
year.no       <- unique(dat$Year)
roundfish     <- unique(dat$Roundfish[!is.na(dat$Roundfish)])[order(unique(dat$Roundfish[!is.na(dat$Roundfish)]))]
haulsAge      <- length(roundfish)
haulsLength   <- length(roundfish)
quarter       <- unique(dat$Quarter)[order(unique(dat$Quarter))]

yearvec <- c("2004", "2005","2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015","2016", "2017")
RFvec <- c("1", "2","3", "4", "5", "6", "7", "8", "9", "10")
#c = 1
y = 14
q = 1
#R = 5

windows(height = 19, width = 25.5)
par(mfrow=c(3,4), tcl=-0.9, family="serif", omi=c(0.3,0.2,0.2,0.3)) #, mar = c(5,7,4,2) + 0.1) #, oma=c(4.5, 4, 4, 2.5), mar=rep(.1, 4), cex=1, las=1)



for(R in 1:length(roundfish))
{

  species = "Galeus melastomus"

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
                                 Quarter == quarter[q],
                                 haul.id %in% unlist(haulselection))

  if(length(country.haul_species)!=0){
    plot(country.haul_species)
  } else {
    plot(subset(dat,
                #                              Country ==country.names[c],
                Roundfish ==roundfish[R],
                Year == year.no[y],
                Quarter == quarter[q]))}



  country.haul_species_length <- subset(dat,
                                        #                                     Country ==country.names[c],
                                        Roundfish ==roundfish[R],
                                        Year == year.no[y],
                                        Quarter == quarter[q],
                                        haul.id %in% unlist(haulselection_length))

  if(length(country.haul_species_length)!=0){
    plot(country.haul_species_length, add=T, col="blue")
  }

  #Number of hauls in the above plot:--------------------------------
  haulsLength[R] <- nrow(country.haul_species_length[["HH"]])



  country.haul_species_age <- subset(dat,
                                     #                                  Country ==country.names[c],
                                     Roundfish ==roundfish[R],
                                     Year == year.no[y],
                                     Quarter == quarter[q],
                                     haul.id %in% unlist(haulselection_age))

  if(length(country.haul_species_age)!=0){
    plot(country.haul_species_age, add=T, col="red" )
  }

  #Number of hauls in the above plot:------------------------------------
  haulsAge[R] <-  nrow(country.haul_species_age[["HH"]])



  title(main=paste("2017 Q1:", species, "in RFA", RFvec[R]), line = 2.5)# , sub = paste(haulsLength[R],  haulsAge[R]), cex.sub=1)
  mtext(paste("HWL =", haulsLength[R], " ", " ",  "HWAL =", haulsAge[R]), side=1, line=4, cex =0.6, font = 1)



  ###labels on axes
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

plot_colors <- c("red","blue","black", "white", "white")
text <- c("Hauls with age and length data","Hauls with length data","All Hauls in RFA", "HWL: No. Hauls with length data", "HWAL: No. Hauls with age and length data")
plot.new()
par(xpd=TRUE)
legend("center", legend = text,col=plot_colors, pch=16, cex=1,horiz = FALSE)
par(xpd=FALSE)

dev.print(pdf, file="Galeus melastomus.pdf");
#dev.off()

haulsLength
haulsAge

cbind(roundfish, haulsLength, haulsAge)


