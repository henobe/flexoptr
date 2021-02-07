#' Simulate different marketing scenarios for a set of parameters and prices
#'
#' A single set of parameters, day ahead prices and intra day prices are used
#' to construct the performance of marketing the parameters without storage on
#' the day ahead market, with storage on the day ahead market, and with an intra
#' day marketing on top of day ahead.
#'
#' @param parameters A named vector of parameters.
#' @param da_prices A vector of day ahead prices, must be a multiple of 24.
#' @param id_prices A data frame of intra day prices, different columns describe
#' different indexes.
#' @param id_index_names A vector of strings describing the order of columns
#' for consecutive hours as encoded in the parameter \code{id_prices}.
#' @param blocked_per_day An optional vector of 24 integers describing the
#' power that is predetermined for certain hours and cannot be optimised.
#' @param simplify Whether the output should be a simple data frame
#' describing the revenues or a more complex list with the whole trade logs,
#' schedule and state.
#'
#' @return A list of results for each of the three scenarios or a data frame
#' with the respective revenues.
#' @export
#' @seealso \code{\link{format_id_prices}}, \code{\link{format_da_prices}},
#' \code{\link{optimise_schedule}}
simulate_marketing <- function(parameters, da_prices, id_prices, id_index_names,
                               blocked_per_day = NULL, simplify = FALSE) {
  zero_schedule <- rep(0, length(da_prices))
  fixed_schedule <- zero_schedule
  if (!is.null(blocked_per_day)) {
    if (!is.logical(blocked_per_day)) {
      stop("Parameter blocked_per_day must only contain logical values")
    }
    if (length(blocked_per_day) != 24) {
      stop("blocked_per_day must be of length 24, one value for each hour.")
    }
    blocked_schedule <- rep(blocked_per_day, length(da_prices) / 24)
    reserved_power <- round(unname(parameters["charge_rate"]) / 2)
    fixed_schedule[which(blocked_schedule)] <- reserved_power
    blocked_trades <- describe_trades(zero_schedule, fixed_schedule, da_prices) %>%
      append_trades(NULL, 0)

    da_shift <- sum(blocked_per_day) * reserved_power
  } else {
    blocked_schedule <- NULL
    blocked_trades <- NULL
    da_shift <- 0
  }

  no_optim <- simulate_no_storage(parameters, da_prices)

  da_optim <- optimise_schedule(
    schedule = fixed_schedule,
    prices = format_da_prices(da_prices),
    parameters = parameters,
    shift = parameters["loss_rate"] * 24 - da_shift,
    blocked = blocked_schedule
  )
  da_optim$trades <- rbind(da_optim$trades, blocked_trades, make.row.names = FALSE)

  id_optim <- optimise_schedule(
    schedule = da_optim$schedule,
    prices = format_id_prices(id_prices, id_index_names) %>%
      `$`("lookout") %>%
      shorten(length(id_index_names)),
    parameters = parameters,
    shift = 0,
    blocked = blocked_schedule
  )

  revenues <- sapply(
    list(no_optim$trades, da_optim$trades, id_optim$trades),
    calc_revenue
  )

  # intra day positive revenue is added to costs of day ahead
  revenues[3] <- revenues[2] + revenues[3]

  if (simplify) {
    data.frame(
      type = c("constant", "da_optim", "id_optim"),
      revenue_abs = revenues,
      revenue_rel = revenues / revenues[1]
    )
  } else {
    no_optim$revenue <- revenues[1]
    da_optim$revenue <- revenues[2]
    id_optim$revenue <- revenues[3]

    list(constant = no_optim, da_optim = da_optim, id_optim = id_optim)
  }
}
