#' Select only viable charging times
#'
#' @param constraints a data frame with constraints for cumulative minimum,
#' \code{cummin}, cumulative maximum \code{cummax},
#' a direct maximum charge \code{dirmax}.
#'
#' The order of the data frame is assumed to represent the time hierarchy.
#'
#' @return a data frame with original row numbers and prices
#' @seealso build_cum_constraints
filter_available_cycles <- function(constraints){

  if (any(constraints$cummin > 0)){
    # filter until first value where charge is necessary
    max_index <- min(which(constraints$cummin > 0))
    constraints <- constraints[1:max_index,]
  }

  # filter values where charge is possible
  constraints[(constraints$cummax > 0) & (constraints$dirmax > 0),]
}


#' Adapt constraints to a charge input
#'
#' @param constraints a data frame
#' @param index a positive integer, the index where a unit of charge is added
#'
#' @return a data frame, updated constraints input
adapt_constraints <- function(constraints, index) {
  # at index and all following, the cumulative charge is reduced by one
  constraints$cummax[index:nrow(constraints)] <- constraints$cummax[index:nrow(constraints)] - 1

  # for preceding values, the cumulative charge cannot exceed the
  # updated charge at index. cumsum is reduced if higher.
  previous_values <- constraints$cummax[1:(index-1)]  # also works for index 1
  max_pre_cummax <- constraints$cummax[index]
  constraints$cummax[1:(index-1)] <- ifelse(previous_values > max_pre_cummax,
                                            max_pre_cummax, previous_values)

  # at index, direct charge must be reduced by one
  constraints$dirmax[index] <- constraints$dirmax[index] - 1

  # cumulative minimum charge is reduced by one,
  # but only if value was greater zero, so other values do not become negative
  constraints$cummin[constraints$cummin != 0] <- constraints$cummin[constraints$cummin != 0] - 1

  constraints
}


#' Optimise a constrained schedule over price data
#'
#' The order of the data frame and the price data is assumed to represent
#' the time hierarchy.
#'
#' @param constraints a data frame with constraints, i.e. columns, for a
#' cumulative minimum, \code{cummin}, a cumulative maximum \code{cummax}
#' and a direct maximum charge \code{dirmax}. These
#' @param prices a vector of numeric prices, must have same length as rows
#'  of constraints.
#' @param volume a positive integer, sets the number of units of energy that
#' should be distributed over the possible times.
#'
#' @return A vector of integers
#' @export
#'
#' @examples
#' sample_constraints <- build_constraints(10, 5, 20, 2, 4)
#' sample_prices <- sample.int(50, 10, replace = TRUE)
#' optimise_schedule(sample_constraints, sample_prices, 15)
optimise_schedule <- function(constraints, prices, volume) {
  if(nrow(constraints) != length(prices)){
    stop("Constrains and prices have different lengths")
  }

  constraints$prices <- prices
  constraints$index <- 1:length(prices)
  schedule <- rep(0, length(prices))

  while(sum(schedule) < volume){
    # component row.names beutzen

    available_times <- filter_available_cycles(constraints)
    minimum_price_index <- which.min(available_times$prices)
    constraint_index <- available_times$index[minimum_price_index]
    schedule[constraint_index] <- schedule[constraint_index] + 1
    constraints <- adapt_constraints(constraints, constraint_index)
  }
  schedule
}
