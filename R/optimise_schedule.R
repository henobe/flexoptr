#' Optimise a schedule with an iterating approach
#'
#' @param schedule a numeric vector of the current schedule.
#' @param prices a list of available prices. When an element consists only of
#' NA-values (or a single), then that hour will not be iterated. When some price
#' values inside a list element are NA, this will be interpreted that only
#' those hours are not tradeable.
#' @param parameters a named vector of integers, including values for
#' charge_rate, loss_rate, starting_state, and capacity.
#' @param shift an integer, indicates the difference to the the sum of the
#' \code{current_schedule} that is added or subtracted each iteration
#' @param blocked a numeric vector, sane length as \code{schedule},
#' that blocks amounts that are not optimised but considered when calculating
#' the needs of the storage.
#'
#' @return A list with three elements, an optimised schedule, the states of the
#' storage according to that new schedule and a data frame of corresponding trades.
#' @export
#'
#' @seealso \code{\link{build_constraints}}
optimise_schedule <- function(schedule, prices, parameters,
                              shift, blocked=NULL) {
  if(any(sapply(c("starting_state", "charge_rate", "loss_rate", "capacity"),
                function(x) is.null(parameters[x])))){
    stop("All parameter elements must be present.")
  }
  if(!all_integer(parameters)){
    stop("Can only compute integers in parameters")
  }
  if(!all_integer(schedule)){
    stop("Can only compute integers in schedule")
  }
  if((length(shift) != 1)| (!all_integer(shift)) | shift < 0){
    stop("Shift must be a single postive integer value")
  }
  
  state <- c(unname(parameters["starting_state"]), rep(0, length(schedule)))
  if(is.null(blocked)) blocked <- rep(0, length(schedule))
  trades <- NULL

  for (i in identify_non_na_elements(prices)){
    next_prices <- prices[[i]]
    range <- length(next_prices)
    i_end <- i + range - 1

    facets <- split_schedule(schedule[i:i_end], blocked[i:i_end], next_prices)
    constraints <- build_constraints(range, state[i], parameters = parameters) %>%
      match_constraints(facets$fixed, facets$untradeable)
    
    tweaked_schedule <- optimise_constraints(constraints,
                                             next_prices,
                                             sum(facets$flexible) + shift)
    
    new_schedule <- facets$fixed + tweaked_schedule
    schedule[i:i_end] <- new_schedule
    state[(i + 1):(i_end + 1)] <- state[i] + cumsum(new_schedule) -
      seq_len(range) * parameters["loss_rate"]
    trades <- describe_trades(facets$flexible, tweaked_schedule, next_prices) %>%
      append_trades(trades, (i - 1))
  }
  list(schedule = schedule, state = state[-1], trades = trades)
}
