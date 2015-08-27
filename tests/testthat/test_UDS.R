library(dplyr)

context("UDS")

test_that("Only code for Yugoslavia (Serbia) changes, for period 2007-2012", {
  expect_equal(nrow(uds %>% filter(uds_ccode != GWn)), 6)
  expect_equal(nrow(uds %>% filter(!in_system)), 10)
})

