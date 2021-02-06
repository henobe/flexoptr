# calc_cummax -------------------------
test_that("Cummax has same length as cycle", {
  expect_vector(calc_cummax(1, 5, 20, 1, 2), numeric(0), 1)
  expect_vector(calc_cummax(10, 5, 20, 1, 2), numeric(0), 10)
  expect_vector(calc_cummax(100, 5, 20, 1, 2), numeric(0), 100)
})

test_that("Standard cummax values are handled correctly", {
  expect_equal(calc_cummax(5, 9, 10, 2, 4), c(3, 5, 7, 9, 11))
  expect_equal(calc_cummax(20, 5, 20, 1, 4),
               c(20, 20, 20, 20, 20, 21, 22, 23, 24, 25,
                 26, 27, 28, 29, 30, 31, 32, 33, 34, 35))
  expect_equal(calc_cummax(10, 5, 10, 3, 4),
               c(20, 20, 20, 20, 20, 23, 26, 29, 32, 35))
})

test_that("Cummax below capacity is handled correctly", {
  expect_equal(calc_cummax(4, 5, 10, 3, 4), c(20, 20, 20, 20))
})

# calc_cummin -------------------------
test_that("Cummin has same length as cycle", {
  expect_vector(calc_cummin(1, 5, 1), numeric(0), 1)
  expect_vector(calc_cummin(10, 5, 1), numeric(0), 10)
  expect_vector(calc_cummin(100, 5, 1), numeric(0), 100)
})

test_that("Standard cummin values are handled correctly", {
  expect_equal(calc_cummin(3, 3, 2), c(0, 1, 3))
  expect_equal(calc_cummin(3, 5, 2), c(0, 0, 1))
})

test_that("Cummin above zero is handled correctly", {
  expect_equal(calc_cummin(4, 5, 1), rep(0, 4))
  expect_equal(calc_cummin(4, 15, 3), rep(0, 4))
  expect_equal(calc_cummin(20, 21, 1), rep(0, 20))
})

test_that("Immediate below zero for cummin is handled correctly", {
  expect_equal(calc_cummin(3, 2, 3), c(1, 4, 7))
  expect_equal(calc_cummin(3, 0, 3), c(3, 6, 9))
})


# build_constraints -------------------
test_that("Error when not all base parameters are present", {
  expect_error(build_constraints(5, 10, capacity = 20, loss_rate = 2))
  expect_error(build_constraints(5, 10, loss_rate = 2, charge_rate = 4))
  expect_error(build_constraints(5, 10, capacity = 20, charge_rate = 4))

  my_parameters <- c("capacity" = 20, "loss_rate" = 2, charge_rate = 4)

  expect_error(build_constraints(5, 10, parameters = my_parameters[c(1, 2)]))
  expect_error(build_constraints(5, 10, parameters = my_parameters[c(2, 3)]))
  expect_error(build_constraints(5, 10, parameters = my_parameters[c(1, 3)]))
})


test_that("Error when any parameter was negative", {
  expect_error(build_constraints(5, 10, capacity = 20, loss_rate = 2, charge_rate = -4))
  expect_error(build_constraints(5, 10, capacity = 20, loss_rate = -2, charge_rate = 4))
  expect_error(build_constraints(5, 10, capacity = -20, loss_rate = 2, charge_rate = 4))
  expect_error(build_constraints(5, 10, capacity = -20, loss_rate = -2, charge_rate = -4))
})
