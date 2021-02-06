test_that("Type of vector is has no impact", {
  expect_true(check_same_length(sample.int(5),
                                factor(sample.int(5))))
})

test_that("Different length of inputs", {
  expect_false(check_same_length(sample.int(5),
                                 sample.int(5),
                                 sample.int(4)))
  
  expect_false(check_same_length(sample.int(5),
                                 sample.int(4),
                                 sample.int(5)))
  
  expect_false(check_same_length(sample.int(4),
                                 sample.int(5),
                                 sample.int(5)))
})

test_that("Null vector still computes", {
  expect_false(check_same_length(sample.int(5),
                                 sample.int(5),
                                 integer(0)))
})
