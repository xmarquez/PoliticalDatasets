library(dplyr)

context("Polity")

test_that("48 cases change code, all correctly; 145 cases not in system", {
  expect_equal(nrow(polity_cases %>% filter(GWn != polity_ccode)), 48)
  expect_equal(nrow(polity_cases %>% filter(!in_system)), 145)
})



