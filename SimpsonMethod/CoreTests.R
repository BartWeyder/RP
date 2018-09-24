library(testthat)
library(covr)
source("SimpsonMethod.R")

c <- function_coverage(simpson_method, {
    test_result <- c()
    test_result <- c(
        test_result, test_that("Test exp(sqrt(x)) from 0 to 2", {
        f <- function(x) { return(exp(sqrt(x))) }
        expect_that(abs(simpson_method(f, 0, 2) - integrate(f, 0, 2)$value) < 0.0001,
                equals(TRUE))
        })
    )
    test_result <- c(test_result,
        test_that("Test exp(sqrt(x)) from 0 to 2", {
            f <- function(x) { return(x^4 * (1 + x^3) ^ -1) }
            expect_that(abs(simpson_method(f, 1, 2) - integrate(f, 1, 2)$value) < 0.0001,
                equals(TRUE))
        })
    )
    }
)

print(c)