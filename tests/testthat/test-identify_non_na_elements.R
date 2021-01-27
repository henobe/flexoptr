test_that("Only NA elements returns empty vector", {
  expect_equal(identify_non_na_elements(list(rep(NA, 5))), integer(0))
})

test_that("Elements in list can contain some NAs", {
  expect_equal(identify_non_na_elements(list(c(1,2,3, NA, 5),
                                             NA,
                                             NA,
                                             c(3,4,5, 6, 7),
                                             NA)),
               c(1,4))
})

test_that("Elements can have parallel NAs", {
  expect_equal(identify_non_na_elements(list(c(NA, 1, 2, 3),
                                             c(NA, 3, 4, 5),
                                             c(NA, 4, 5, 6))),
               c(1, 2, 3))
})

test_that("Elements in list can have different lengths", {
  expect_equal(identify_non_na_elements(list(c(1,2,3, NA, 5),
                                             c(NA, NA),
                                             NA,
                                             c(3,4,5, 7),
                                             c(NA, NA, NA))),
               c(1,4))
})
