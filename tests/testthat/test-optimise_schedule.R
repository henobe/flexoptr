test_that("Too large a volume is blocked", {
  my_schedule <- c(0, 1, 0, 0)
  my_constraints <- build_constraints(4, 8, 20, 3, 4)
  some_prices <- c(4, 6, 10, 3)

  expect_warning(expect_equal(
    optimise_schedule(my_schedule, 20, my_constraints, some_prices)$schedule,
    my_schedule))

  expect_warning(expect_equal(
    optimise_schedule(my_schedule, -2, my_constraints, some_prices)$schedule,
    my_schedule))

  my_constraints <- build_constraints(4, 18, 20, 3, 4)
  expect_warning(expect_equal(
    optimise_schedule(my_schedule, 15, my_constraints, some_prices)$schedule,
    my_schedule))

})

test_that("Output has correct volume", {
  my_schedule <- c(0, 3, 0, 0)
  my_constraints <- build_constraints(4, 8, 20, 3, 4)
  some_prices <- c(4, 6, 10, 3)

  res <- optimise_schedule(my_schedule, 10, my_constraints, some_prices)
  expect_equal(sum(res$schedule), 13)

  res <- optimise_schedule(my_schedule, -2, my_constraints, some_prices)
  expect_equal(sum(res$schedule), 1)
})
