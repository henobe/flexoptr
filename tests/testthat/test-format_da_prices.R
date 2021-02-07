sample_prices <- rep(c(1, 2, 3), each = 24)

test_that("Standard values are handled correctly", {
  formatted_prices <- format_da_prices(sample_prices)
  expect_equal(length(formatted_prices), 3 * 24)
  expect_true(all(unique(unlist(formatted_prices)) %in% c(NA, 1, 2, 3)))
  expect_equal(unlist(formatted_prices[2:24]), rep(NA, 23))
  expect_equal(formatted_prices[[1]], rep(1, 24))
})

test_that("Length not a multiple of 24 throws error", {
  indexed_prices <- sample_prices[-c(60:(3 * 24))]
  expect_error(format_da_prices(indexed_prices))
})
