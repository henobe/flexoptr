test_that("No reservations and all available prices do not change flexible schedule", {
  res <- split_schedule(c(0, 0, 3, 2, 3, 4),
                        rep(0, 6),
                        c(30, 40, 20, 25, 60, 5))

  expect_equal(res$fixed, rep(0, 6))
  expect_equal(res$flexible, c(0, 0, 3, 2, 3, 4))
  expect_equal(length(res$untradeable), 0)
})

test_that("Reservations are present", {
  res <- split_schedule(c(0, 0, 3, 2, 3, 4),
                        c(0, 0, 2, 2, 0, 0),
                        c(30, 40, 20, 25, 60, 5))

  expect_equal(res$fixed, c(0, 0, 2, 2, 0, 0))
  expect_equal(res$flexible, c(0, 0, 1, 0, 3, 4))
  expect_equal(length(res$untradeable), 0)
})

test_that("NA in prices", {
  res <- split_schedule(c(0, 0, 3, 3, 2, 4),
                        rep(0, 6),
                        c(30, 40, 20, NA, NA, 5))

  expect_equal(res$fixed, c(0, 0, 0, 3, 2, 0))
  expect_equal(res$flexible, c(0, 0, 3, 0, 0, 4))
  expect_equal(res$untradeable, c(4, 5))
})

test_that("NAs in prices and blockers", {
  res <- split_schedule(c(0, 0, 3, 3, 2, 4),
                 c(0, 0, 2, 2, 0, 0),
                 c(30, 40, 20, NA, NA, 5))

  expect_equal(res$fixed, c(0, 0, 2, 3, 2, 0))
  expect_equal(res$flexible, c(0, 0, 1, 0, 0, 4))
  expect_equal(res$untradeable, c(4, 5))
})

test_that("Different length of inputs", {
  expect_error(split_schedule(sample.int(5),
                              sample.int(5),
                              sample.int(4)))

  expect_error(split_schedule(sample.int(5),
                              sample.int(4),
                              sample.int(5)))

  expect_error(split_schedule(sample.int(4),
                              sample.int(5),
                              sample.int(5)))
})

test_that("Reservations exceed schedule", {
  expect_error(split_schedule(c(0, 2, 3, 4, 1),
                              c(0, 2, 4, 2, 0),
                              sample.int(5)))
})
