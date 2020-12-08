#' Optimise a constrained schedule over price data
#'
#' The order of the data frame and the price data is assumed to represent
#' the time hierarchy.
#'
#' @param constraints a data frame with constraints, i.e. columns, for a
#' cumulative minimum, \code{cummin}, a cumulative maximum \code{cummax}
#' and a direct maximum charge \code{dirmax}.
#' @param prices a vector of numeric prices, must have same length as rows
#'  of constraints.
#' @param volume a positive integer, sets the number of units of energy that
#' should be distributed over the possible times.
#'
#' @return A vector of integers
#' @export
#'
#' @seealso \code{\link{optimise_schedule}}
#'
#' @examples
#' sample_constraints <- build_constraints(10, 5, 20, 2, 4)
#' sample_prices <- sample.int(50, 10, replace = TRUE)
#' optimise_constraints(sample_constraints, sample_prices, 15)
optimise_constraints <- function(constraints, prices, volume) {
  if(nrow(constraints) != length(prices)){
    stop("Constrains and prices have different lengths")
  }

  constraints$prices <- prices
  constraints$index <- 1:length(prices)
  schedule <- rep(0, length(prices))

  while(sum(schedule) < volume){
    available_times <- filter_available_cycles(constraints)
    minimum_price_index <- which.min(available_times$prices)
    constraint_index <- available_times$index[minimum_price_index]
    schedule[constraint_index] <- schedule[constraint_index] + 1
    constraints <- adapt_constraints(constraints, constraint_index)
  }
  schedule
}