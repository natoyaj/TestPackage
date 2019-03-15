compareRuns <<- function(run, run.exp)
{
	test_that("same area based index", {expect_equal(run, run.exp, tol=1e-4)})
}
