test_that("No reservations and all available prices do not change flexible schedule", {
  res <- split_schedule(c(0, 0, 3, 2, 3, 4),
                        rep(FALSE, 6),
                        c(30, 40, 20, 25, 60, 5))
  
  expect_equal(res$fixed, rep(0, 6))
  expect_equal(res$flexible, c(0, 0, 3, 2, 3, 4))
  expect_equal(length(res$untradeable), 0)
})

test_that("Reservations are present", {
  res <- split_schedule(c(0, 0, 3, 2, 3, 4),
                        c(F, F, T, T, F, F),
                        c(30, 40, 20, 25, 60, 5))

  expect_equal(res$fixed, c(0, 0, 3, 2, 0, 0))
  expect_equal(res$flexible, c(0, 0, 0, 0, 3, 4))
  expect_equal(length(res$untradeable), 2)
})

test_that("NA in prices", {
  res <- split_schedule(c(0, 0, 3, 3, 2, 4),
                        rep(F, 6),
                        c(30, 40, 20, NA, NA, 5))

  expect_equal(res$fixed, c(0, 0, 0, 3, 2, 0))
  expect_equal(res$flexible, c(0, 0, 3, 0, 0, 4))
  expect_equal(res$untradeable, c(4, 5))
})

test_that("NAs in prices and blockers", {
  res <- split_schedule(c(0, 0, 3, 3, 2, 4),
                 c(F, F, T, T, F, F),
                 c(30, 40, 20, NA, NA, 5))

  expect_equal(res$fixed, c(0, 0, 3, 3, 2, 0))
  expect_equal(res$flexible, c(0, 0, 0, 0, 0, 4))
  expect_equal(res$untradeable, c(3, 4, 5))
})

test_that("Inputs must have same length", {
  expect_error(split_schedule(c(0, 0, 3, 3, 2),
                              c(F, F, T, T, F, F),
                              c(30, 40, 20, NA, NA, 5)))
  expect_error(split_schedule(c(0, 0, 3, 3, 2, 4),
                              c(F, F, T, T, F),
                              c(30, 40, 20, NA, NA, 5)))
  expect_error(split_schedule(c(0, 0, 3, 3, 2, 4),
                              c(F, F, T, T, F, F),
                              c(30, 40, 20, NA, NA)))
})
