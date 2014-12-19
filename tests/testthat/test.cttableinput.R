library(contingitools)
context("Handle incorrect inputs")


test_that("Dimensions", {
              expect_error(ct.rr(c(1, 2, 3)))
              expect_error(ct.er(c(1, 2, 3)))
              expect_error(ct.ar(c(1, 2, 3)))
              expect_error(ct.or(c(1, 2, 3)))
              expect_error(ct.rr(c(1, 2, 3, 4, 5, 6)))
              expect_error(ct.er(c(1, 2, 3, 4, 5, 6)))
              expect_error(ct.ar(c(1, 2, 3, 4, 5, 6)))
              expect_error(ct.or(c(1, 2, 3, 4, 5, 6)))
})

