library(contingitools)
context("One factor point estimates")

# No pattern
cttable1 <- cttable(c(5, 5, 5, 5))
# Table 3.1 pg 21
cttable3.1 <- cttable(c(16712, 1197142, 18784, 2878421))
# Table 7.4
cttable7.4 <- cttable(c(178, 1411, 79, 1486))
# Table 7.3
cttable7.3 <- cttable(c(347, 555, 20, 88))

# Point estimates
test_that("Odds Ratio", {
              expect_equal(1, ct.or(cttable1)$OR)
              expect_equal(2.14, round(ct.or(cttable3.1)$OR), 2)
              expect_equal(2.75, round(ct.or(cttable7.3)$OR), 2)
              expect_equal(2.62, round(ct.or(cttable7.3, correction = TRUE)$OR), 2)
})

test_that("Relative Risk", {
              expect_equal(1, ct.rr(cttable1)$RR)
              expect_equal(2.12,  round(ct.rr(cttable3.1)$RR, 2))
              expect_equal(2.22,  round(ct.rr(cttable7.4)$RR, 2))
})

test_that("Excess Risk", {
              expect_equal(0, ct.er(cttable1)$ER)
              expect_equal(0.062, round(ct.er(cttable7.4)$ER, 3))
})

test_that("Attributable Risk", {
              expect_equal(0, ct.ar(cttable1)$AR)
              expect_equal(0.38, round(ct.ar(cttable7.4)$AR, 2))
})

# intervals

test_that("Odds Ratio Interval", {
              expect_equal(c(1.66, 4.55), round(ct.or(cttable7.3)$CI, 2))
              expect_equal(c(1.64, 4.45), round(ct.or(cttable7.3, correction = TRUE)$CI, 2))
})

test_that("Relative Risk Interval", {
              expect_equal(c(1.72, 2.87), round(ct.rr(cttable7.4)$CI, 2))
})

test_that("Excess Risk Interval", {
              expect_equal(c(0.043, 0.080), round(ct.er(cttable7.4)$CI, 3))
})

test_that("Attributable Risk Interval", {
              expect_equal(c(0.26, 0.48), round(ct.ar(cttable7.4)$CI, 2))
})
