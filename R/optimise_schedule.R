#' Optimise a schedule with an iterating approach
#'
#' The optimal possible schedule considering a set of prices a pre-existing 
#' schedule and physical constraints of a storage is calculated.
#'
#' A current schedule is taken, the corresponding constraints are generated and
#' then the optimal schedule in these constraints is calculated. The necessary
#' trades are then recorded. This procedure is repeated as coded in the prices
#' parameter.
#'
#' The approach allows for overlapping optimisations, where the result of the
#' previous run influences the outcome of the next - as is the case during a
#' typical intra day optimisation.
#'
#' Non-overlapping time frames are also handled, as is the case during a day
#' ahead process where the shift represents the energy loss over 24 hours.
#'
#' @param schedule a numeric vector of the current schedule.
#' @param prices a list of available prices. When an element consists only of
#' NA-values (or a single), then that hour will not be iterated. When some price
#' values inside a list element are NA, this will be interpreted to mean that
#' only those hours are not tradeable.
#' @param parameters a named vector of integers, including values for
#' charge_rate, loss_rate, starting_state, and capacity.
#' @param shift an integer, indicates the difference to the the sum of the
#' \code{current_schedule} that is added or subtracted each iteration.
#' @param blocked A logical vector indicating whether the schedule for that
#' hour should be kept as is and not changed by the optimisation, same length
#' as schedule.
#'
#' @return A list with three elements, an optimised schedule, the states of the
#' storage according to that new schedule and a data frame of corresponding trades.
#' @export
#'
#' @seealso \code{\link{build_constraints}}
optimise_schedule <- function(schedule, prices, parameters, shift, blocked = NULL) {
  if (any(sapply(
    c("starting_state", "charge_rate", "loss_rate", "capacity"),
    function(x) is.na(parameters[x])
  ))) {
    stop("All parameter elements must be present.")
  }
  if (!all_integer(parameters)) {
    stop("Can only compute integers in parameters, was")
  }
  if (!all_integer(schedule)) {
    stop("Can only compute integers in schedule")
  }
  if ((!all_integer(shift)) | (length(shift) != 1)) {
    stop("Shift must be a single integer value")
  }
  if (is.null(blocked)) {
    blocked <- rep(FALSE, length(schedule))
  } else {
    if (!check_same_length(schedule, blocked)) {
      stop("Schedule and Blocked parameter do not have the same length")
    }
  }
  iteration_index <- identify_non_na_elements(prices)
  last_iteration <- max(iteration_index)
  if (last_iteration + length(prices[[last_iteration]]) - 1 > length(schedule)) {
    stop("Last price index exceeds length of schedule")
  }

  state <- c(unname(parameters["starting_state"]), rep(0, length(schedule)))
  trades <- NULL

  for (i in iteration_index) {
    next_prices <- prices[[i]]
    range <- length(next_prices)
    i_end <- i + range - 1

    facets <- split_schedule(schedule[i:i_end], blocked[i:i_end], next_prices)
    constraints <- build_constraints(range, state[i], parameters = parameters) %>%
      match_constraints(facets$fixed, facets$untradeable)

    tweaked_schedule <- optimise_constraints(
      constraints,
      next_prices,
      sum(facets$flexible) + shift
    )

    new_schedule <- facets$fixed + tweaked_schedule
    schedule[i:i_end] <- new_schedule
    state[(i + 1):(i_end + 1)] <- state[i] + cumsum(new_schedule) -
      seq_len(range) * parameters["loss_rate"]
    trades <- describe_trades(facets$flexible, tweaked_schedule, next_prices) %>%
      append_trades(trades, (i - 1))
  }
  list(schedule = schedule, state = state[-1], trades = trades)
}
