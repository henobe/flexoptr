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
