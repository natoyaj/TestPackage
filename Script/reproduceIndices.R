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

