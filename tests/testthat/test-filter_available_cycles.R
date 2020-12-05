context("Filter available cycles in constraints")

test_that("Function caps at first cummin > 0", {
  expect_equal(filter_available_cycles(build_constraints(10, 11, 20, 1, 6))$cummin,
               rep(0, 10))
  expect_equal(filter_available_cycles(build_constraints(10, 11, 20, 2, 6))$cummin,
               c(0, 0, 0, 0, 0, 1))
  expect_equal(filter_available_cycles(build_constraints(10, 11, 20, 3, 6))$cummin,
               c(0, 0, 0, 1))
  expect_equal(filter_available_cycles(build_constraints(10, 11, 20, 4, 6))$cummin,
               c(0, 0, 1))
  expect_equal(filter_available_cycles(build_constraints(10, 11, 20, 7, 8))$cummin,
               c(0, 3))
  expect_equal(filter_available_cycles(build_constraints(10, 5, 20, 7, 8))$cummin,
               c(2))
})

test_that("Only available cummax are returned", {

  df <- data.frame(cummin = c(0, 0),
                   cummax = c(5, 5),
                   dirmax = c(4, 4))
  row.names(df) <- as.integer(c(3, 4))

  expect_equal(filter_available_cycles(data.frame(cummin = c(0, 0, 0, 0),
                                                  cummax = c(0, 0, 5, 5),
                                                  dirmax = c(3, 3, 4, 4))),
               df)
})

test_that("Only available dirmax are returned", {

  df <- data.frame(cummin = c(0, 0),
                   cummax = c(3, 5),
                   dirmax = c(3, 4))
  row.names(df) <- as.integer(c(1, 3))

  expect_equal(filter_available_cycles(data.frame(cummin = c(0, 0, 0, 0),
                                                  cummax = c(3, 3, 5, 5),
                                                  dirmax = c(3, 0, 4, 0))),
               df)
})
