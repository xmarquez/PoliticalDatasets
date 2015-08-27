library(dplyr)

context("bmr")

test_that("Number of countries whose country code changes", {
  expect_equal(nrow(bmr %>% filter(bmr_ccode != GWn)), 508)
  expect_equal(nrow(bmr %>% filter(grepl("SARD",bmr_country),bmr_ccode != GWn)), 46)
  expect_equal(nrow(bmr %>% filter(grepl("YEM",bmr_country),bmr_ccode != GWn)), 21)
  expect_equal(nrow(bmr %>% filter(grepl("GERM",bmr_country),bmr_ccode != GWn)), 21)
  expect_equal(nrow(bmr %>% filter(is.na(country_name))), 0)
})
