context("chart Tests")
library(rchart); library(testthat); library(dplyr)

test_that("calculate_diff with no diff produces empty tibble", {
  a1 <- calculate_diff(data=rchart::exampleMapDataParam)
  tVal1 <- nrow(a1)
  testthat::expect_equal(tVal1,0)
})

test_that("calculate_diff with scenRef works", {
  a1 <- calculate_diff(data=rchart::exampleMapDataParam, scenRef = "GCAM_SSP3")
  tVal1 <- nrow(a1)
  tVal2 <- nrow(rchart::exampleMapDataParam)
  testthat::expect_gt(tVal1,tVal2)
})

test_that("calculate_diff with xRef works", {
  a1 <- calculate_diff(data=rchart::exampleMapDataParam, xRef = "2010")
  tVal1 <- nrow(a1)
  tVal2 <- nrow(rchart::exampleMapDataParam)
  testthat::expect_gt(tVal1,tVal2)
})

test_that("calculate_diff with xRef and scenRef works", {
  a1 <- calculate_diff(data=rchart::exampleMapDataParam, xRef = "2010", scenRef = "GCAM_SSP3")
  a2 <- calculate_diff(data=rchart::exampleMapDataParam, xRef = "2010")
  tVal1 <- nrow(a1)
  tVal2 <- nrow(a2)
  testthat::expect_gt(tVal1,tVal2)
})
