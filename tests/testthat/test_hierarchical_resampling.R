require(testthat)
require(DATRAS)
context("hierachical resampling")

dataDir <<- system.file("Data", package = "TestPackage")
dat <- readExchangeDir(dataDir)

CA <- dat[["CA"]]
HL <- dat[["HL"]]
HH <- dat[["HH"]]

expect_error(simTrawlHaulsHiearchical(HH, CA, HL)) #CA and HL reversed
expect_error(simTrawlHaulsHiearchical(HH, CA, HL, c(), c())) #No levels given
expect_error(simTrawlHaulsHiearchical(HH, CA, HL, c("StatRec", "haul.id"), c("N"))) #Selection misspecified

#default parameters
sim <- simTrawlHaulsHiearchical(HH, HL, CA, hierarchy=c("StatRec", "haul.id"), selection=c("S","R"))

#check set consistency
expect_true(!any(duplicated(sim$simHH$haul.id)))
expect_true(any(duplicated(sim$simHH$original.id)))
expect_true(all(sim$simCA$haul.id %in% sim$simHH$haul.id))
expect_true(all(sim$simHL$haul.id %in% sim$simHH$haul.id))
expect_gt(nrow(CA), nrow(sim$simCA))
expect_gt(nrow(sim$simCA), nrow(HH))
expect_gt(nrow(HL), nrow(sim$simHL))
expect_gt(nrow(sim$simHL), nrow(HH))

# check id links (original vs new)
for (nid in unique(sim$simCA$haul.id)){
  d <- sim$simCA[sim$simCA$haul.id==nid,]
  dd <- sim$simHL[as.character(sim$sim$HL$haul.id)==nid,]
  expect_true(all(as.character(dd$original.id)==as.character(d$original.id[[1]])))
}
for (nid in unique(sim$simHL$haul.id)){
  d <- sim$simHL[sim$simHL$haul.id==nid,]
  dd <- sim$simCA[as.character(sim$sim$CA$haul.id)==nid,]
  expect_true(all(as.character(dd$original.id)==as.character(d$original.id[[1]])))
}

# check that all age samples in each haul match original
for (nid in unique(sim$simCA$haul.id)){
  d <- sim$simCA[sim$simCA$haul.id==nid,]
  expect_equal(nrow(d), nrow(CA[as.character(CA$haul.id)==as.character(d$original.id[[1]]),]))
}

# check that all length samples in each haul match original
for (nid in unique(sim$simHL$haul.id)){
  d <- sim$simHL[sim$simHL$haul.id==nid,]
  expect_equal(nrow(d), nrow(HL[as.character(HL$haul.id)==as.character(d$original.id[[1]]),]))
}

#check some other parameters
sim <- simTrawlHaulsHiearchical(HH, HL, CA, hierarchy=c("StatRec", "haul.id"), selection=c("N","N"))
expect_equal(nrow(HH), nrow(sim$simHH))
expect_equal(nrow(HL), nrow(sim$simHL))
expect_equal(nrow(CA), nrow(sim$simCA))

sim <- simTrawlHaulsHiearchical(HH, HL, CA, hierarchy=c("StatRec", "haul.id"), selection=c("N","R"))
expect_equal(nrow(HH), nrow(sim$simHH))

sim <- simTrawlHaulsHiearchical(HH, HL, CA, hierarchy=c("haul.id"), selection=c("R"))
expect_equal(nrow(HH), nrow(sim$simHH))
