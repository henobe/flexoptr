#' Calculates the cumulative minimum charge
#'
#' @param cycles a positive integer
#' @param state a positive integer
#' @param loss_rate a positive integer
#'
#' @return a vector of integers, with length \code{cycles}.
calc_cummin <- function(cycles, state, loss_rate){
  cycles_until_empty <- state / loss_rate

  if(floor(cycles_until_empty) < cycles) {
    overshoot <- ceiling(cycles_until_empty) * loss_rate - state

    c(rep(0, ceiling(cycles_until_empty) - 1),
      overshoot,
      overshoot + cumsum(rep(loss_rate,
                             cycles - (ceiling(cycles_until_empty)))))
  } else {
    rep(0, cycles)
  }
}


#' Calculates the cumulative maximum charge
#'
#' @param cycles a positive integer
#' @param state a positive integer
#' @param capacity a positive integer
#' @param loss_rate a positive integer
#' @param charge_rate a positive integer
#'
#' @return a vector of integers, with length \code{cycles}.
calc_cummax <- function(cycles, state, capacity, loss_rate, charge_rate){
  charge_cycles <- (capacity - state) / (charge_rate - loss_rate)
  full_charge_cycles <- floor(charge_cycles)
  remaining_charge <- capacity + ceiling(charge_cycles) * loss_rate -
    state - (full_charge_cycles * charge_rate)

  charge_until_full <- full_charge_cycles * charge_rate + remaining_charge

  if(charge_cycles > cycles) {
    return(rep(charge_until_full, cycles))
  } else {
    following_charge <- cumsum(rep(loss_rate, cycles - ceiling(charge_cycles)))
    return(c(rep(charge_until_full, ceiling(charge_cycles)),
             charge_until_full + following_charge))
  }
}


#' Calculates the cumulative minimum and maximum charge for a storage
#'
#' Given the physical parameters, this function calculates the necessary,
#' i.e lower limits, and possible, i.e. upper limits, for charging a storage.
#'
#' Considering a steady loss rate in some kind of energy storage, this function
#' calculates the cumulative minimal charge required to not go below zero
#' charge.
#'
#' In the same sense, a maximum cumulative charge is calculated which indicates
#' the physical and realistic maximum of energy that could be put into the
#' storage until it is full.
#'
#' The function thinks in time cycles, where one would charge x amount of
#' energy from the beginning of the cycle until the end of the same cycle.
#'
#' @param cycles a positive integer, the number of cycles this function should consider
#' @param state a positive integer, the starting state of energy in the storage
#' @param capacity a positive integer, the maximum amount of energy that can be stored
#' @param loss_rate a positive integer, the energy / cycle depleted from storage
#' @param charge_rate a positive integer, the maximum energy / cycle which with the storage can be charged
#'
#' @return a data frame with cycle number, minimum, and maximum cumulative charge
#' @export
#'
#' @examples
#' build_constraints(10, 5, 20, 2, 4)
build_constraints <- function(cycles, state, capacity,
                                  loss_rate, charge_rate){
  data.frame(
    "cummin" = calc_cummin(cycles, state, loss_rate),
    "cummax" = calc_cummax(cycles, state, capacity, loss_rate, charge_rate),
    "dirmax" = charge_rate)
}