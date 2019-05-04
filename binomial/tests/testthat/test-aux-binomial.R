
context("Tests for Auxilary functions for summary stats")



#tests for aux_mean function
test_that("aux_mean with invalid length",{
  expect_error(aux_mean(10, 0.1, 3))
  expect_error(aux_mean(trials = 10, prob = 0.2, 10))
  expect_error(aux_mean(10))
})

test_that("aux_mean output is less than trials",{
  expect_lte(aux_mean(10, 0.1), 10)
  expect_lte(aux_mean(trials = 10, prob = 0.2), 10)
})


#tests for aux_variance function
test_that("aux_variance with invalid length",{
  expect_error(aux_variance(10, 0.1, 3))
  expect_error(aux_variance(trials = 10, prob = 0.2, 10))
  expect_error(aux_variance(10))
})


#tests for aux_mode function
test_that("aux_mode with invalid length",{
  expect_error(aux_mode(10, 0.1, 3))
  expect_error(aux_mode(trials = 10, prob = 0.2, 10))
  expect_error(aux_mod(10))
})



#tests for aux_skewness function
test_that("aux_skewness with invalid length",{
  expect_error(aux_skewness(10, 0.1, 3))
  expect_error(aux_skewness(trials = 10, prob = 0.2, 10))
  expect_error(aux_skewness(10))
})



#tests for aux_kurtosis function
test_that("aux_kurtosis with invalid length",{
  expect_error(aux_kurtosis(10, 0.1, 3))
  expect_error(aux_kurtosis(trials = 10, prob = 0.2, 10))
  expect_error(aux_kurtosis(10))
})



