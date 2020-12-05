context("Change constraints on charging decision")

std_df <- data.frame(cummin = c(0, 0, 1, 2, 3),
                     cummax = c(10, 10, 10, 10, 10),
                     dirmax = c(2, 2, 2, 2, 2))

test_that("Cummax values are handled correctly", {
  changed_df <- adapt_constraints(std_df, 3)
  expect_equal(changed_df$cummax, c(9, 9, 9, 9, 9))

  df <- data.frame(cummin = c(0, 0, 0, 0, 0),
                   cummax = c(8, 10, 10, 11, 12),
                   dirmax = c(2, 2, 2, 2, 2))
  changed_df <- adapt_constraints(df, 3)
  expect_equal(changed_df$cummax, c(8, 9, 9, 10, 11))
})

test_that("Dirmax changes at index", {
  expect_equal(adapt_constraints(std_df, 1)$dirmax, c(1, 2, 2, 2, 2))
  expect_equal(adapt_constraints(std_df, 2)$dirmax, c(2, 1, 2, 2, 2))
  expect_equal(adapt_constraints(std_df, 3)$dirmax, c(2, 2, 1, 2, 2))
  expect_equal(adapt_constraints(std_df, 4)$dirmax, c(2, 2, 2, 1, 2))
  expect_equal(adapt_constraints(std_df, 5)$dirmax, c(2, 2, 2, 2, 1))
})

test_that("Cummin values are reduced by one until zero", {
  changed_df <- adapt_constraints(std_df, 1)
  expect_equal(changed_df$cummin, c(0, 0, 0, 1, 2))
  changed_df <- adapt_constraints(changed_df, 1)
  expect_equal(changed_df$cummin, c(0, 0, 0, 0, 1))
  changed_df <- adapt_constraints(changed_df, 2)
  expect_equal(changed_df$cummin, c(0, 0, 0, 0, 0))
  changed_df <- adapt_constraints(changed_df, 2)
  expect_equal(changed_df$cummin, c(0, 0, 0, 0, 0))
})

test_that("Index out of range throws error", {
  expect_error(adapt_constraints(std_df, 9))
})
