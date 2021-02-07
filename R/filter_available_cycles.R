#' Select only viable charging times
#'
#' A constraints data frame describes the physical limits of a charging process.
#' 
#' The function selects the rows until \code{cummin} column first contains a
#' value greater than zero (this row is also selected). Out of this selection
#' only the rows with a \code{cummax} and a \code{dirmax} value greater than
#' zero are selected.
#'
#' @param constraints a data frame with columns for cumulative minimum,
#' \code{cummin}, cumulative maximum \code{cummax}, a direct maximum charge
#' \code{dirmax}. The order of the rows is assumed to be strictly chronological
#' without time lapses.
#'
#' The order of the data frame is assumed to represent the time hierarchy.
#'
#' @return the original data with filtered rows.
#' @seealso  \code{\link{build_constraints}}
filter_available_cycles <- function(constraints) {
  if (any(constraints$cummin > 0)) {
    # filter until first value where charge is necessary
    max_index <- min(which(constraints$cummin > 0))
    constraints <- constraints[1:max_index, ]
  }

  # filter values where charge is possible
  constraints[(constraints$cummax > 0) & (constraints$dirmax > 0), ]
}
