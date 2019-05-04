
context("Tests for main binomial functions")


#tests for bin_choose
test_that("bin_choose with invalid lengths",{
  expect_error(bin_choose(10, 6, 3))
  expect_error(bin_choose(10, 6, 3, 4))
  expect_error(bin_choose(10))
})

test_that("bin_choose with k greater than n", {
  expect_error(bin_choose(10, 11))
})

test_that("bin_choose with invalid inputs",{
  expect_error(bin_choose(-1, 2))
  expect_error(bin_choose(1, -2))
})

test_that("bin_choose with valid inputs", {
  expect_equal(bin_choose(n = 5, k = 2), 10)
  expect_equal(bin_choose(5, 0), 1)
  expect_equal(bin_choose(5, 1:3), c(5, 10, 10))
})





#tests for bin_probability
test_that("bin_probability with invalid lengths",{
  expect_error(bin_probability(10, 6, 3, 4))
  expect_error(bin_probability(10))
})

test_that("bin_probability with invalid inputs",{
  expect_error(bin_probability(-1, 2, 0.6))
  expect_error(bin_probability(1, -2, 0.5))
  expect_error(bin_probability(3, 6, 2))
})

test_that("bin_probability with valid inputs",{
  expect_equal(bin_probability(success = 2, trials = 5, prob = 0.5), 0.3125)
  expect_equal(bin_probability(success = 0:2, trials = 5, prob = 0.5), c(0.03125, 0.15625, 0.31250))
})





#tests for bin_distribution
test_that("bin_distribution with invalid lengths",{
  expect_error(bin_distribution(10, 6, 3))
  expect_error(bin_distribution(10, 6, 3, 4))
  expect_error(bin_distribution(10))
})

test_that("bin_distribution with invalid inputs",{
  expect_error(bin_distribution(-1, 2, 0.6))
  expect_error(bin_distribution(1, -2, 0.5))
  expect_error(bin_distribution(3, 6, 2))
})

test_that("bin_distribution class is bindis and data.frame", {
  expect_equal(class(bin_distribution(trials = 5, prob = 0.5)), c("bindis", "data.frame"))
})



#tests for bin_cumulative
test_that("bin_cumulative with invalid lengths",{
  expect_error(bin_cumulative(10, 6, 3))
  expect_error(bin_cumulative(10, 6, 3, 4))
  expect_error(bin_cumulative(10))
})

test_that("bin_cumulative with invalid inputs",{
  expect_error(bin_cumulative(-1, 2, 0.6))
  expect_error(bin_cumulative(1, -2, 0.5))
  expect_error(bin_cumulative(3, 6, 2))
})

test_that("bin_cumulative class is bindis and data.frame", {
  expect_equal(class(bin_cumulative(trials = 5, prob = 0.5)), c("bincum", "data.frame"))
})










