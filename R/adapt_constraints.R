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
