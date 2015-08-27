library(dplyr)

context("NELDA")

test_that("Only Serbia changes", {
  expect_equal(nrow(nelda %>% filter(nelda_ccode != GWn)), 4)
})

