#' Optimise a schedule
#'
#' Checks input parameters and then returns an optimised schedule
#' and the necessary trades.
#'
#' @param change_in_schedule an integer, indicates the difference to the
#' the sum of \code{current_schedule}
#' @param current_schedule a vector of integers, must have same length as rows
#' of \code{constraints}
#' @inheritParams optimise_constraints
#'
#' @return a list with a numerical vector for the schedule and
#' a data frame describing the trades. If no trades happened,
#' then the data frame has zero rows
#' @export
#'
#' @seealso \code{\link{build_constraints}}
#' @examples
#' my_schedule <- rep(0, 4)
#' my_constraints <- build_constraints(4, 8, 20, 3, 4)
#' some_prices <- c(4, 6, 10, 3)
#' optimisation_result <- optimise_schedule(
#'   current_schedule = my_schedule,
#'   change_in_schedule = 12,
#'   constraints = my_constraints,
#'   prices = some_prices)
#'
#' # can also re-optimise with new prices
#' new_prices <- c(5, 2, 1, 8)
#' new_result <- optimise_schedule(
#'   current_schedule = optimisation_result$schedule,
#'   change_in_schedule = 0,
#'   constraints = my_constraints,
#'   prices = new_prices)
optimise_schedule <- function(current_schedule, change_in_schedule,
                              constraints, prices){
  volume <- sum(current_schedule) + change_in_schedule

  if(volume > utils::tail(constraints$cummax, n=1) |
     volume > sum(constraints$dirmax) |
     volume < 0) {
    warning("change in schedule requested which exceeds physical limits,
            returning original schedule")
    return(list(schedule = current_schedule,
                # generate a mock trade data frame for consistent data format
                trades = describe_trades(0, 0, 0)))
  }

  new_schedule <- optimise_constraints(constraints, prices, volume)
  trades <- describe_trades(current_schedule, new_schedule, prices)

  list(schedule = new_schedule,
       trades = trades)
}
