some_prices <- data.frame(
  time = c(1, 2, 3, 4, 5),
  id_1 = c(20, 30, 40, 50, 45),
  id_2 = c(22, 28, 39, 48, 46),
  id_3 = c(25, 27, 41, 49, 50)
)

format_id_prices(some_prices, c("id_1", "id_2", "id_3"))

test_that("Order of arguments controls order of elements", {
  expect_equal(
    format_id_prices(some_prices, c("id_1", "id_2", "id_3"))$lookout[[1]],
    c(NA, 30, 39, 49)
  )
  expect_equal(
    format_id_prices(some_prices, c("id_1", "id_3", "id_2"))$lookout[[1]],
    c(NA, 30, 41, 48)
  )
  expect_equal(
    format_id_prices(some_prices, c("id_3", "id_2", "id_1"))$lookout[[1]],
    c(NA, 27, 39, 50)
  )
})

test_that("Values in original data frame are unchanged", {
  returned_data <- format_id_prices(some_prices, c("id_1", "id_2", "id_3"))
  original_vars <- !(names(returned_data) == "lookout")
  expect_equal(some_prices, returned_data[original_vars])
})

test_that("NAs values in prices are handled correctly", {
  some_prices$id_2[3] <- NA
  expect_equal(
    format_id_prices(some_prices, c("id_1", "id_2", "id_3"))$lookout[[1]],
    c(NA, 30, NA, 49)
  )
})
