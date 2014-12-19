library(contingitools)
context("One factor should be a matrix, more should be an array")

# No pattern
cttable1 <- cttable(c(5, 5, 5, 5))
cttable2 <- cttable(c(5, 5, 5, 5, 10, 10, 10, 10))

test_that("Dimensions", {
              expect_equal(c(2, 2), dim(cttable1))
              expect_equal(c(2, 2, 2), dim(cttable2))
})

