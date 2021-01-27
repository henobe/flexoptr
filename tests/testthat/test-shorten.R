test_that("Returns correct length", {
  expect_equal(shorten(seq(20), 5), seq(15))
  expect_equal(shorten(seq(20), 10), seq(10))
  expect_equal(shorten(seq(20), 1), seq(19))
  expect_equal(shorten(seq(20), 19), 1)
})

test_that("Shorten by zero returns same vector", {
  expect_equal(shorten(seq(20), 0), seq(20))
})

test_that("Shorten by whole length returns error", {
  expect_error(shorten(seq(20), 20))
})
