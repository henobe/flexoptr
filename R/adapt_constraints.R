#' Adapt constraints to a charge input
#'
#' Constraints built with \code{build_constraints} to incorporate the changes
#' when a charge is added at an index. The values in the columns \code{cummax},
#' \code{cummin} and \code{dirmax} are updated accordingly.
#'
#' @param constraints a data frame of constraints, expected to resemble output
#' of \code{build_constraints}.
#' @param index a positive integer, the index where a unit of charge is added.
#'
#' @return the constraints data frame with updated values
#' @export
adapt_constraints <- function(constraints, index) {
  n_rows <- nrow(constraints)
  # at index and all following, the cumulative charge is reduced by one
  constraints$cummax[index:n_rows] <- constraints$cummax[index:n_rows] - 1

  # for preceding values, the cumulative charge cannot exceed the
  # updated charge at index. cumsum is reduced if higher.
  pre_values <- constraints$cummax[1:(index - 1)] # also works for index 1
  max_pre_cummax <- constraints$cummax[index]
  constraints$cummax[1:(index - 1)] <- ifelse(
    pre_values > max_pre_cummax,
    max_pre_cummax,
    pre_values
  )

  # at index, direct charge must be reduced by one
  constraints$dirmax[index] <- constraints$dirmax[index] - 1

  # cumulative minimum charge is reduced by one,
  # but only at index and following values,
  # and only if value was greater zero, so other values do not become negative
  post_values <- constraints$cummin[index:n_rows]
  constraints$cummin[index:n_rows] <- post_values - ifelse(post_values > 0, 1, 0)

  constraints
}
