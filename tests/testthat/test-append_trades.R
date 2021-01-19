new_trades <- describe_trades(c(3,4,5,6), c(3,3,6,6), c(20, 30, 40, 50))

test_that("Old trades can be a Null-object", {
  new_trades$trading_time <- 0
  expect_equal(append_trades(new_trades, NULL, 0), new_trades)
})

test_that("Offset is added correctly", {
  base_times <- c(2, 3)
  expect_equal(append_trades(new_trades, NULL, 2)$time, base_times + 2)
  expect_equal(append_trades(new_trades, NULL, -1)$time, base_times - 1)
  expect_equal(append_trades(new_trades, NULL, 10)$time, base_times + 10)
})

test_that("Trading time equals offset", {
  expect_equal(unique(append_trades(new_trades, NULL)$trading_time), 0)
  expect_equal(unique(append_trades(new_trades, NULL, 2)$trading_time), 2)
})

test_that("Standard values are handled correlty", {
  old_trades <- describe_trades(c(2,5,5,6), c(3,3,6,6), c(10, 20, -5, 30))
  old_trades$trading_time <- 0
  complete_trades <- append_trades(new_trades, old_trades, 3)

  expect_equal(nrow(complete_trades), 5)
  expect_equal(complete_trades$trading_time, c(0, 0, 0, 3, 3))
  expect_equal(complete_trades$time, c(1, 2, 3, 5, 6))
  expect_equal(complete_trades$volume, c(1, -2, 1, -1, 1))
  expect_equal(complete_trades$prices, c(10, 20, -5, 30, 40))
})

test_that("Empty data frame returns unchanged old_trades", {
  new_trades$trading_time <- 0
  expect_equal(append_trades(new_trades[0,], new_trades), new_trades)
})
