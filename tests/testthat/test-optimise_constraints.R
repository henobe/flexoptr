test_that("Standard values are handled correctly", {
  expect_equal(optimise_constraints(build_constraints(9, 4, 8, 1, 3),
                                    c(2, 4, 1, 4, 3, 5, 6, 8, 7),
                                    6),
               c(3, 0, 3, 0, 0, 0, 0, 0, 0))

  expect_equal(optimise_constraints(build_constraints(9, 4, 8, 1, 3),
                                    c(3, 4, 1, 2, 9, 5, 4, 8, 7),
                                    9),
               c(2, 0, 3, 3, 0, 0, 1, 0, 0))
})

test_that("No volume returns empty schedule", {
  expect_equal(optimise_constraints(build_constraints(10, 5, 20, 2, 4),
                                    sample.int(50, 10, replace = TRUE),
                                    0),
               rep(0, 10))
})
