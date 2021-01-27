#' Limit constraints to respect reservations
#'
#' This function allows to adapt constraints not only for a single index but
#' for a range of indexes and a range of values.
#'
#' @param constraints A data.frame with constraints
#' @param fixed_schedule A numeric vector describing which amounts should be
#' applied to schedule. It is implicitly assumed that these values do not
#' exceed the limits of the constraints.
#' @param untradeable A numeric vector of indexes in which no trades can occur,
#' so constraints are adapted accordingly.
#'
#' @return A data.frame of adapted, more limited constraints
#' @export
#'
#' @examples
#' some_constraints <- build_constraints(5, 2, 6, 1, 3)
#' already_scheduled <- c(0, 2, 2, 1, 0)
#' untradeable <- c(3, 4)
#' match_constraints(some_constraints, already_scheduled, untradeable)
match_constraints <- function(constraints, fixed_schedule, untradeable = NULL){
  sequential_blockers <- rep(seq_along(fixed_schedule), times = fixed_schedule)
  for(i in sequential_blockers){
    constraints <- adapt_constraints(constraints, i)
  }
  constraints$dirmax[untradeable] <- 0
  constraints
}
