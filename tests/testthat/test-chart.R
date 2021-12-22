context("chart Tests")
library(rchart); library(testthat); library(dplyr)

test_that("chart plots correctly", {
  tVal1 <- 1
  testthat::expect_gt(tVal1,0)
})
