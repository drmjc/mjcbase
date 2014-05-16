context("padNA suite")

test_that("padNA works", {
	l <- list(1:3, 1:5 ,1:10)
	
	expected.res <- list(c(1L, 2L, 3L, NA, NA, NA, NA, NA, NA, NA), c(1L, 2L, 3L, 
	4L, 5L, NA, NA, NA, NA, NA), 1:10)
	res <- padNA(l)
	expect_equal(res, expected.res)
})
