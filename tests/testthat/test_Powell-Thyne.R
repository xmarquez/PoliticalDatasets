library(dplyr)

context("Powell-Thyne")

test_that("No country code changes", {
  expect_equal(nrow(PowellThyne %>% filter(powell_ccode != GWn)), 0)
})




