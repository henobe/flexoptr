#' Split a schedule in a fixed and a flexible part
#'
#' The parameters schedule, reservations and available_prices should all be
#' of same length.
#'
#' @param schedule A numeric vector of the current schedule.
#' @param reservations A numeric vector describing reserved amounts of power.
#' @param available_prices A numeric vector. When an element is NA, this
#' element is considered to be untradeable, therefore the schedule is taken as
#' fixed for that index.
#'
#' @return A list of three vectors, the first element describes fixed parts of
#' the schedule, the second the flexible parts, and a third contains the
#' indexes where no trade is possible
#'
#' @export
#'
#' @examples
#' my_schedule <- c(0, 0, 3, 2, 3, 4)
#' reserved_capacity <- c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE)
#' available_prices <- c(30, 40, 20, NA, NA, 5)
#' split_schedule(my_schedule, reserved_capacity, available_prices)
split_schedule <- function(schedule, reservations, available_prices) {
  if (!check_same_length(schedule, reservations, available_prices)) {
    stop("Input parameters do not have the same length.")
  }

  untradeable <- available_prices %>%
    is.na() %>%
    which() %>%
    c(which(reservations)) %>%
    unique() %>%
    sort()

  fixed <- rep(0, length(schedule))
  fixed[untradeable] <- schedule[untradeable]

  list(fixed = fixed, flexible = schedule - fixed, untradeable = untradeable)
}
