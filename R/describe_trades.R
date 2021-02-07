#' Describe trades
#'
#' Generate a trade list based on a previous and a new schedule. The function
#' will analyse the differences between the schedules and infer the time and
#' volume of the trades that occurred.
#'
#' @param old_schedule a numerical vector
#' @param new_schedule a numerical vector, must be same length as \code{current_schedule}.
#' @param prices a numerical vector, must be same length as \code{current_schedule}.
#'
#' @returns a data frame with columns describing the time at which a trade
#' occurred (as index of old_schedule), volume and buy/sell price
#' @export
#'
#' @examples
#' schedule1 <- c(4, 0, 0, 3)
#' schedule2 <- c(0, 3, 0, 4)
#' new_prices <- c(70, 65, 80, 60)
#' describe_trades(schedule1, schedule2, new_prices)
describe_trades <- function(old_schedule, new_schedule, prices) {
  if (!check_same_length(old_schedule, new_schedule, prices)) {
    stop("Input parameters do not have the same length")
  }
  df <- data.frame(old = old_schedule, new = new_schedule, prices = prices)
  df$time <- seq_along(old_schedule) # necessary for later traceability
  df$volume <- df$new - df$old
  df <- df[df$volume != 0, c(4, 5, 3)] # only return times where trades happened
  rownames(df) <- NULL # indexing in previous operation messes with row names
  df
}
