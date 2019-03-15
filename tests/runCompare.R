runCompare <<- function(name){
	#context(paste0(name, " example"))
	setwd(paste0("../", name, "/"))
	stopifnot(require("testthat"),
		require("IBTSindices"))
	source("../compareRuns.R")
	source("script.R")
	load("runExpected.Rdata")
	compareRuns(run, runExp)
}
