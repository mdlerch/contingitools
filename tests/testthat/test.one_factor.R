library(contingitools)
context("One factor point estimates")

# No pattern
cttable1 <- cttable(c(5, 5, 5, 5))
# Table 3.1 pg 21
cttable2 <- cttable(c(16712, 1197142, 18784, 2878421))

test_that("Odds Ratio", {
              expect_equal(1, ct.or(cttable1)$OR)
              expect_true(abs(2.14 - ct.or(cttable2)$OR) < 0.01)})

test_that("Relative Risk", {
              expect_equal(1, ct.rr(cttable1)$RR)
              expect_true(abs(2.12 - ct.rr(cttable2)$RR) < 0.1)})


# test_that("Excess Risk", {
#               expect_equal(ct.or(c(5, 5, 5, 5)), 0)})

# test_that("Excess Risk", {
#               expect_equal(ct.or(c(5, 5, 5, 5)), 0)})
