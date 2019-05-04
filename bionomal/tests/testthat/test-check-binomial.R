context("Test for check functions")

#tests for private function check_prob
test_that("check_prob with number between 0 and 1", {
  expect_true(check_prob(0.5))
  expect_true(check_prob(prob = 0.7))
})

test_that("check_prob with invalid length", {
  expect_error(check_prob(prob = c(0.5, 0.7)))
})

test_that("check_prob with invalid input", {
  expect_error(check_prob('one'))
  expect_error(check_prob(-0.5))
  expect_error(check_prob(5))
})





#tests for private fucntion check_trials
test_that("check_trials with non-negative integer", {
    expect_true(check_trials(25))
    expect_true(check_trials(trials = 10))
  })

test_that("check_trials with invalid inputs", {
  expect_error(check_trials(-6))
  expect_error(check_trials(0.2))
  expect_error(check_trials("three"))
})

test_that("check_trials with invalid lengths", {
  expect_error(check_trials(C(10, 26)))
})




#tests for private fucntion check_success
test_that("check_success with non-negative integers", {
  expect_true(check_success(24, 25))
  expect_true(check_success(trials = 10, success = 9))
})

test_that("check_success with success greater than trials", {
  expect_error(check_success(25, 24))
  expect_error(check_success(success = 30, trials = 29))
})

test_that("check_success with invalid inputs", {
  expect_error(check_success(0))
  expect_error(check_success(25))
  expect_error(check_success(25,26,27))
  expect_error(check_success("three","four"))
})


