#' Mock the output \code{optimise_schedule} for settings without storage
#'
#' @param parameters a named vector, which must contain \code{loss_rate} and
#' \code{starting_state}.
#' @param prices a numeric vector.
#'
#' @return a list of schedule, state, ans trades. Compatible with
#' \code{optimise_schedule}
#' @export
simulate_no_storage <- function(parameters, prices) {
  repeater <- function(x) rep.int(x, length(prices))

  schedule <- repeater(parameters["loss_rate"])
  state <- repeater(parameters["starting_state"])
  trades <- describe_trades(repeater(0), schedule, prices)

  list(schedule = schedule, state = state, trades = trades)
}
