runCompare <<- function(name){
	setwd(paste0("../", name, "/"))
	stopifnot(require("testthat"),
		require("IBTSindices"))
	source("../compareRuns.R")
	source("script.R")
	load("runExpected.Rdata")
	compareRuns(run, runExp)
}
