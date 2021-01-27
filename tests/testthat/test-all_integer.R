test_that("Non-Integer parameters are blocked", {
  expect_false(all_integer(c(3,4,5,6.4)))
  expect_false(all_integer(c(3.3,4.3,5.3,6.4)))
})

test_that("Named vector computes normally", {
  expect_false(all_integer(c("a" = 3, "b" = 4, "c" = 5, "d" = 6.4)))
})

test_that("Only integers gives true", {
  expect_true(all_integer(c(3, 4, 5, 6)))
})

test_that("Named vector computes normally", {
  expect_false(all_integer(c("a" = 3, "b" = 4, "c" = 5, "d" = 6.4)))
  expect_true(all_integer(c("a" = 3, "b" = 4, "c" = 5, "d" = 6)))
})
