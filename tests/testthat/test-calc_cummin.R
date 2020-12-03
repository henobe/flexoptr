context("Generate minimal charge constraints")

test_that("Result has same length as cycle", {
  expect_vector(calc_cummin(1, 5, 1), numeric(0), 1)
  expect_vector(calc_cummin(10, 5, 1), numeric(0), 10)
  expect_vector(calc_cummin(100, 5, 1), numeric(0), 100)
})

test_that("Standard values are handled correctly", {
  expect_equal(calc_cummin(4, 5, 1), rep(0, 4))
  expect_equal(calc_cummin(4, 15, 3), rep(0, 4))
  expect_equal(calc_cummin(20, 21, 1), rep(0, 20))
})

test_that("No minimum reached is handled correctly", {
  expect_equal(calc_cummin(4, 5, 1), rep(0, 4))
})

test_that("Immediate minimum reached is handled correctly", {
  expect_equal(calc_cummin(3, 2, 3), c(1, 4, 7))
})
