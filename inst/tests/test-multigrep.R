context("multi-grep testingL mgrep, mgrepl, grepf, grepfl")
library(datasets)

test_that("datasets present", {
	expect_equivalent(
		state.name,
		c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", 
		"Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", 
		"Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", 
		"Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", 
		"Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
		"New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", 
		"Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", 
		"South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", 
		"Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", 
		"Wyoming")
	)
})

test_that("mgrep works", {
	expect_equivalent( 
		mgrep(c("New","^V", "Iran"), state.name),
		structure(list(New = 29:32, `^V` = 45:46, Iran = NA), .Names = c("New", "^V", "Iran"))
	)
	expect_equivalent( 
		mgrep(c("New","^V", "Iran"), state.name, value=TRUE),
		structure(list(New = c("New Hampshire", "New Jersey", "New Mexico", 
		"New York"), `^V` = c("Vermont", "Virginia"), Iran = NA), .Names = c("New", 
		"^V", "Iran"))
	)
	expect_equivalent( 
		mgrep(c("New","^V", "Iran"), state.name, nomatch=NULL),
		structure(list(New = 29:32, `^V` = 45:46, Iran = integer(0)), .Names = c("New", 
		"^V", "Iran"))
	)
	expect_equivalent( 
		mgrep(c("New","^V", "Iran"), state.name, value=TRUE, nomatch=NULL),
		structure(list(New = c("New Hampshire", "New Jersey", "New Mexico", 
		"New York"), `^V` = c("Vermont", "Virginia"), Iran = character(0)), .Names = c("New", 
		"^V", "Iran"))
	)
})

test_that("mgrepl works", {
	expect_equivalent(
		mgrepl(c("New","^V", "Iran"), state.name),
		structure(c(TRUE, TRUE, FALSE), .Names = c("New", "^V", "Iran"))
	)
})

test_that("grepf works", {
	expect_equivalent(
		grepf(c("New","^V", "Iran"), state.name),
		c(29L, 30L, 31L, 32L, 45L, 46L)
	)
	expect_equivalent(
		grepf(c("New","^V", "Iran"), state.name, value=TRUE),
		c("New Hampshire", "New Jersey", "New Mexico", "New York", "Vermont", 
		"Virginia")
	)
	expect_equivalent(
		grepf(c("New","^V", "Iran"), state.name, invert=TRUE),
		c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 
		15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L, 
		28L, 33L, 34L, 35L, 36L, 37L, 38L, 39L, 40L, 41L, 42L, 43L, 44L, 
		47L, 48L, 49L, 50L)
	)
})


test_that("grepfl works", {
	expect_equivalent(
		grepfl(c("New","^V", "Iran"), state.name),
		c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
		FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
		FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
		FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, 
		FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, 
		FALSE, FALSE, FALSE, FALSE)
	)
	
	expect_equivalent(
		grepfl(c("New","^V", "Iran"), state.name, invert=TRUE),
		c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, 
		TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, 
		TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, 
		FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, 
		TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)
	)
})
