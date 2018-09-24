library(testthat)
library(covr)
source("Input.R")

c <- function_coverage(input_function, {
  test_result <- c()
  test_result <-
    c(test_result, test_that("Right input: exp(sqrt(x))", {
      expect_equal(input_function("exp(sqrt(x))"), 0)
    }))
  test_result <- c(test_result,
                   test_that("Not right input: afsadfasdg", {
                     expect_equal(input_function("afsadfasdg"), 2)
                   }))
  test_result <- c(test_result,
                   test_that("Not right input: (2 + 2) * 2d", {
                     expect_equal(input_function("(2 + 2) * 2d"), 2)
                   }))
})
print(c)