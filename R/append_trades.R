#' Extend a trade data frame with new trades
#'
#' @param new_trades a data frame, formatted like the output of
#' \code{describe_trades}.
#' @param old_trades a data frame, formatted like the output of this function,
#' can be NULL.
#' @param time_offset the index of trades are assumed to be starting at 1, the
#' offset is added to this index.
#'
#' @return a data frame, with all values from \code{new_trades} and
#' \code{old_trades}
#'
#' @export
append_trades <- function(new_trades, old_trades, time_offset = 0) {
  if (nrow(new_trades) == 0) {
    return(old_trades)
  }

  new_trades$trading_time <- time_offset
  new_trades$time <- time_offset + new_trades$time
  rbind(old_trades, new_trades, make.row.names = FALSE)
}
