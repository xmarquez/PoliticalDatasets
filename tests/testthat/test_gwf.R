library(dplyr)

context("Geddes Wright and Frantz")

test_that("autocratic_gwf", {
  expect_equal(nrow(autocratic_gwf %>% filter(gwf_cowcode != GWn)), 0)
  expect_equal(nrow(autocratic_gwf %>% filter(!in_system)), 0)
  
})

test_that("all_gwf", {
  expect_equal(nrow(all_gwf %>% filter(gwf_cowcode != GWn)), 65)
  expect_equal(nrow(all_gwf %>% filter(!in_system)), 1) # Czechoslovakia 1993
})

