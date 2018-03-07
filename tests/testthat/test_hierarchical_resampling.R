require(testthat)
require(DATRAS)
context("hierachical resampling")

dataDir <<- system.file("Data", package = "TestPackage")
dat <- readExchangeDir(dataDir)

ca <- dat[["CA"]]
hl <- dat[["HL"]]
hh <- dat[["HH"]]

#trim down set for faster execution of tests

hh <- hh[50:100,]
ca <- ca[ca$haul.id %in% hh$haul.id,]
hl <- hl[hl$haul.id %in% hh$haul.id,]

remove = c("RecordType", "GearExp", "DoorType", "SpecCode","AreaType","Valid_Aphia","Survey",
           "Stratum","HaulVal","HydroStNo","StdSpecRecCode","BycSpecRecCode","Rigging",
           "Tickler","Warplngt", "Warpdia","WarpDen","DoorSurface","DoorSpread","WingSpread",
           "Buoyancy","KiteDim","WgtGroundRope","TowDir","SurCurDir","SpeedWater","SurCurSpeed","BotCurDir","BotCurSpeed",
           "WindDir","WindSpeed","SwellDir","SwellHeight","SurTemp","BotTemp","SurSal","BotSal",
           "ThermoCline","ThClineDepth","DoorWgt","GroundSpeed","Distance","Netopening","Depth","abstime",
           "timeOfYear","DateofCalculation","SweepLngt","Maturity","Ship","Gear","StNo","HaulNo",
           "SpecCodeType","PlusGr","CatCatchWgt","Sex","DayNight","HaulLong","TimeShotHour")
ca = ca[,which(!(names(ca) %in% remove))]
hl = hl[,which(!(names(hl) %in% remove))]
hh = hh[,which(!(names(hh) %in% remove))]

hh_keys <- c("haul.id")
ca_hh <- merge(ca,hh, by=hh_keys, suffixes=c(".CA", ""))
hl_hh <- merge(hl,hh, by=hh_keys, suffixes=c(".HL", ""))

expect_error(simTrawlHaulsHiearchical(ca_hh, hl_hh)) #CA and HL reversed
expect_error(simTrawlHaulsHiearchical(hl_hh, ca_hh, c(), c())) #No levels given
expect_error(simTrawlHaulsHiearchical(hl_hh, ca_hh, c("StatRec", "haul.id"), c("N"))) #Selection misspecified

#default parameters
sim <- simTrawlHaulsHiearchical(hl_hh, ca_hh, hierarchy=c("StatRec", "haul.id"), selection=c("S","R"))

#check set consistency
expect_true(all(sim$simCA$haul.id %in% sim$simHL$haul.id))
expect_gt(nrow(ca), nrow(sim$simCA))
expect_gt(nrow(hl), nrow(sim$simHL))

# check id links (original vs new)
for (nid in unique(sim$simCA$haul.id)){
  d <- sim$simCA[sim$simCA$haul.id==nid,]
  dd <- sim$simHL[as.character(sim$simHL$haul.id)==nid,]
  expect_true(all(as.character(dd$original.id)==as.character(d$original.id[[1]])))
}
for (nid in unique(sim$simHL$haul.id)){
  d <- sim$simHL[sim$simHL$haul.id==nid,]
  dd <- sim$simCA[as.character(sim$simCA$haul.id)==nid,]
  expect_true(all(as.character(dd$original.id)==as.character(d$original.id[[1]])))
}

# check that all age samples in each haul match original
for (nid in unique(sim$simCA$haul.id)){
  d <- sim$simCA[sim$simCA$haul.id==nid,]
  expect_equal(nrow(d), nrow(ca[as.character(ca$haul.id)==as.character(d$original.id[[1]]),]))
}

# check that all length samples in each haul match original
for (nid in unique(sim$simHL$haul.id)){
  d <- sim$simHL[sim$simHL$haul.id==nid,]
  expect_equal(nrow(d), nrow(hl[as.character(hl$haul.id)==as.character(d$original.id[[1]]),]))
}

#check that it works with function that expects DATRAS (figure out the data prep pipeline first)
#alk <- calculateALKNew(3, "Gadus morhua", 2015, 1, merge(sim$simHH[,c("haul.id", "lon", "lat", "Roundfish")], sim$simCA), merge(sim$simHH[,c("haul.id", "lon", "lat", "Roundfish")], sim$simHL))

#check some other parameters
sim <- simTrawlHaulsHiearchical(hl_hh, ca_hh, hierarchy=c("StatRec", "haul.id"), selection=c("N","N"))
expect_equal(nrow(hl), nrow(sim$simHL))
expect_equal(nrow(ca), nrow(sim$simCA))

#some tests on hh, which has one row pr haul, so nrow should be equal for sellection w replacement
sim <- simTrawlHaulsHiearchical(hh, ca_hh, hierarchy=c("StatRec", "haul.id"), selection=c("N","R"))
expect_equal(nrow(hh), nrow(sim$simHL))

sim <- simTrawlHaulsHiearchical(hh, ca_hh, hierarchy=c("haul.id"), selection=c("R"))
expect_equal(nrow(hh), nrow(sim$simHL))
