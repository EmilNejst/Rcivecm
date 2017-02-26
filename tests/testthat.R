library(testthat)
library(Rcivecm)

test_check("Rcivecm")

data("interest_rates")

test_that('Adding a constant to the data through add_trend works', {
  expect_that(sum(add_trend(data = interest_rates,
                            trend_order = 0,
                            name = 'constant')[,'x_constant']) == nrow(interest_rates))
})
