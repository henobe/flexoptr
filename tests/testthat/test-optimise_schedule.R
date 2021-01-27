test_that("All parameter values must be present", {
  params <- c("starting_state" = 5, "capacity" = 10, "loss_rate" = 3, "charge_rate" = 5)
  expect_error(optimise_schedule(NULL, NULL, NULL, params[-1]))
  expect_error(optimise_schedule(NULL, NULL, NULL, params[-2]))
  expect_error(optimise_schedule(NULL, NULL, NULL, params[-3]))
  expect_error(optimise_schedule(NULL, NULL, NULL, params[-4]))
})
