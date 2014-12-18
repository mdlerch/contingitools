library(contingitools)
context("One factor point estimates")

# No pattern
cttable1 <- cttable(c(5, 5, 5, 5))
# Table 3.1 pg 21
cttable2 <- cttable(c(16712, 1197142, 18784, 2878421))
# Table 7.4
cttable3 <- cttable(c(178, 1411, 79, 1486))

# Point estimates
test_that("Odds Ratio", {
              expect_equal(1, ct.or(cttable1)$OR)
              expect_true(abs(2.14 - ct.or(cttable2)$OR) < 0.01)})

test_that("Relative Risk", {
              expect_equal(1, ct.rr(cttable1)$RR)
              expect_true(abs(2.12 - ct.rr(cttable2)$RR) < 0.1)})

test_that("Excess Risk", {
              expect_equal(0, ct.er(cttable1)$ER)
              expect_true(abs(0.062 - ct.er(cttable3)$ER) < 0.01)})

# intervals

test_that("Excess Risk Interval", {
              expect_equal(c(0.043, 0.080), round(ct.er(cttable3)$interval, 3))})

