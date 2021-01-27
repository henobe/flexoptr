base_constraints <- build_constraints(5, 2, 6, 1, 3)

test_that("No blockers or NAs do not change constraints", {
  expect_equal(base_constraints,
               match_constraints(base_constraints,
                                 rep(0, nrow(base_constraints)),
                                 numeric(0)))
})

test_that("Reservations are correctly transferred into constraints", {
  expect_equal(match_constraints(base_constraints,
                                 c(0, 1, 1, 0, 0),
                                 numeric(0)),
               data.frame(cummin = c(0, 0, 0, 0, 1),
                          cummax = c(5, 5, 5, 6, 7),
                          dirmax = c(3, 2, 2, 3, 3)))
})

test_that("Untradeable indexes are correctly transferred into constraints", {
  expect_equal(match_constraints(base_constraints,
                                 rep(0, nrow(base_constraints)),
                                 c(2,3)),
               data.frame(cummin = c(0, 0, 1, 2, 3),
                          cummax = c(6, 6, 7, 8, 9),
                          dirmax = c(3, 0, 0, 3, 3)))
})

test_that("Reservations and untradeable values are correctly transferred into constraints", {
  expect_equal(match_constraints(base_constraints,
                                 c(0, 1, 2, 1, 0),
                                 c(2, 3)),
               data.frame(cummin = c(0, 0, 0, 0, 0),
                          cummax = c(4, 4, 4, 4, 5),
                          dirmax = c(3, 0, 0, 2, 3)))
})
