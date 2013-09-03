test_that("collate,character works", {
	expected.res <- c("a", "A", "b", "B", "c", "C", "d", "D", "e", "E", "f", "F", 
	"g", "G", "h", "H", "i", "I", "j", "J", "k", "K", "l", "L", "m", 
	"M", "n", "N", "o", "O", "p", "P", "q", "Q", "r", "R", "s", "S", 
	"t", "T", "u", "U", "v", "V", "w", "W", "x", "X", "y", "Y", "z", 
	"Z")
	res <- collate(letters, LETTERS)
	expect_identical(res, expected.res)
})

test_that("collate,numeric works", {
	expected.res <- c(1L, 100L, 2L, 101L, 3L, 102L, 4L, 103L, 5L, 104L, 6L, 105L, 
	7L, 106L, 8L, 107L, 9L, 108L, 10L, 109L)
	res <- collate(1:10, 100:109)
	expect_identical(res, expected.res)
})

test_that("collate,data.frame works", {	
	library(datasets)
	data(iris)
	a <- iris[1:5,]; colnames(a) <- letters[1:5]
	b <- iris[1:5,]; colnames(b) <- LETTERS[1:5]
	res <- collate(a, b)
	expected.res <- structure(list(a = c(5.1, 4.9, 4.7, 4.6, 5), A = c(5.1, 4.9, 
	4.7, 4.6, 5), b = c(3.5, 3, 3.2, 3.1, 3.6), B = c(3.5, 3, 3.2, 
	3.1, 3.6), c = c(1.4, 1.4, 1.3, 1.5, 1.4), C = c(1.4, 1.4, 1.3, 
	1.5, 1.4), d = c(0.2, 0.2, 0.2, 0.2, 0.2), D = c(0.2, 0.2, 0.2, 
	0.2, 0.2), e = structure(c(1L, 1L, 1L, 1L, 1L), .Label = c("setosa", 
	"versicolor", "virginica"), class = "factor"), E = structure(c(1L, 
	1L, 1L, 1L, 1L), .Label = c("setosa", "versicolor", "virginica"
	), class = "factor")), .Names = c("a", "A", "b", "B", "c", "C", 
	"d", "D", "e", "E"), class = "data.frame", row.names = c(NA, 
	5L))
	
	expect_identical(res, expected.res)
})

