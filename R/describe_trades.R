#' Describe trades
#'
#' Generate a trade list base on a previous and a new schedule.
#'
#'
#' @param old_schedule a numerical vector
#' @param new_schedule a numerical vector, must be same length as
#' \code{current_schedule}
#' @param prices a numerical vector
#'
#' @returns a data frame with columns for original index, volume and buy/sell price
#' @export
#'
#' @examples
#' schedule1 <- c(4, 0, 0,  3)
#' schedule2 <- c(0, 3, 0, 4)
#' new_prices <- c(70, 65, 80, 60)
#' describe_trades(schedule1, schedule2, new_prices)
describe_trades <- function(old_schedule, new_schedule, prices){
  df <- data.frame(old = old_schedule, new = new_schedule, prices = prices)
  df$time <- seq_along(old_schedule)  # necessary for later traceability
  df$volume <- df$new - df$old
  df <- df[df$volume != 0, c(4, 5, 3)]  # only return times where a trade happened
  rownames(df) <- NULL
  df
}
