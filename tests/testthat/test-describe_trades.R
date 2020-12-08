test_that("Times without trade are filtered", {
  expect_equal(describe_trades(c(1, 2, 3), c(2, 2, 4), c(10, 20, 30))$time,
               c(1, 3))
  expect_equal(describe_trades(c(2, 2, 3), c(2, 2, 4), c(10, 20, 30))$time,
               c(3))
  expect_equal(describe_trades(c(2, 3, 3), c(2, 2, 4), c(10, 20, 30))$time,
               c(2, 3))
  expect_equal(describe_trades(c(2, 3, 4), c(2, 2, 4), c(10, 20, 30))$time,
               c(2))
  expect_equal(describe_trades(c(1, 3, 2), c(2, 2, 4), c(10, 20, 30))$time,
               c(1, 2, 3))
})

test_that("Error when vectors have different lengths", {
  expect_error(describe_trades(c(1, 3, 2), c(1, 4, 3, 2), c(10, 20, 30)))
  expect_error(describe_trades(c(1, 3, 2, 4), c(1, 4, 3), c(10, 20, 30)))
  expect_error(describe_trades(c(1, 3, 2), c(1, 4, 3), c(10, 20, 30, 40)))
})

test_that("No trades result in data frame with zero rows", {
  expect_equal(describe_trades(c(1, 2, 3), c(1, 2, 3), c(10, 20, 30)),
               data.frame("time" = numeric(0),
                          "volume" = numeric(0),
                          "prices" = numeric(0)))
})

test_that("Standard values are handled correctly", {
  expect_equal(describe_trades(c(2, 3, 1), c(1, 0, 5), c(10, 20, 30)),
               data.frame("time" = c(1:3),
                          "volume" = c(-1, -3, 4),
                          "prices" = c(10, 20, 30)))
})
