library(dplyr)

context("Svolik")

test_that("Country-year version of the data is correct", {
  expect_equal(nrow(SvolikRegimeAll_cy %>% filter(GWn != svolik_ccode)), 81)
  expect_equal(nrow(SvolikRegimeAll_cy %>% filter(!in_system)), 22)
  expect_equal(nrow(SvolikRegimeAll_cy %>% filter(grepl("Serbi",cname))), 17)
})

test_that("Authoritarian spells are correct", {
  expect_equal(nrow(SvolikAuthoritarianSpells %>% filter(GWn != svolik_ccode)), 3)
  expect_equal(nrow(SvolikAuthoritarianSpells %>% filter(!in_system)), 0)
})

test_that("Leader data is correct", {
  expect_equal(nrow(SvolikLeader %>% filter(GWn != svolik_ccode)), 9)
  expect_equal(nrow(SvolikLeader %>% filter(leadid %in% leadid[duplicated(leadid)])), 0)
})

test_that("No authority data is correct", {
  expect_equal(nrow(SvolikNoAuthority %>% filter(GWn != svolik_ccode)), 0)
})





