#' Make a day ahead price list
#'
#' Reformats a vector of price data into a day ahead format that is compatible
#' with \code{optimise_schedule}.
#'
#' Data is grouped into groups of 24 and put into a list, the first element
#' containing the first 24 prices, the next 23 elements being empty, the 25th
#' element containing the next prices and so on.
#'
#' @param prices a numerical vector of prices.
#'
#' @return a list
#' @export
#'
#' @seealso \code{\link{optimise_schedule}}
#' @examples
#' some_prices <- rnorm(48, 20, 3)
#' format_da_prices(some_prices)
format_da_prices <- function(prices){
  number_of_prices <- length(prices)
  if(number_of_prices %% 24 != 0) stop("Number of prices not a multiple of 24")

  days <- seq(number_of_prices %/% 24)
  wide_prices <- lapply(days, function(x) `[`(prices, ((x-1) * 24 + 1):(x * 24)))

  reformatted_prices <- as.list(rep(NA, number_of_prices))
    for(i in days){
    reformatted_prices[[((i-1) * 24 + 1)]] <- wide_prices[[i]]
  }
  reformatted_prices
}
