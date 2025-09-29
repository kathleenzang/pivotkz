library(testthat)
library(dplyr)
library(tidyr)
library(pivotkz)

test_that("pivotkz works as expected", {
  # Example data
  df <- tibble(
    id = 1:2,
    X2000 = c(10, 20),
    X2001 = c(30, 40)
  )

  # Apply function
  result <- pivotkz(df, cols = starts_with("X"), value_name = "value")

  # Expected result
  expected <- tibble(
    id = c(1, 1, 2, 2),
    year = c(2000, 2001, 2000, 2001),
    value = c(10, 30, 20, 40)
  )

  # Check structure
  expect_s3_class(result, "data.frame")

  # Check correct values
  expect_equal(result$value, expected$value)

  # Check correct years
  expect_equal(result$year, expected$year)

  # Check dimensions
  expect_equal(dim(result), dim(expected))
})
