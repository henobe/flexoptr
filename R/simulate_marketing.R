#' Simulate different marketing scenarios for a set of parameters and prices
#' 
#' A single set of parameters, day ahead prices and intra day prices are used
#' to construct the performance of marketing the parameters without storage on
#' the day ahead market, with storage on the day ahead market, and with an intra
#' day marketing on top of day ahead.
#' 
#' @param parameters A named vector of parameters
#' @param da_prices A vector of day ahead prices, must be a multiple of 24
#' @param id_prices A data frame of intra day prices, different columns describe
#' different indexes.
#' @param id_index_names A vector of strings describing the order of columns 
#' for consecutive hours.
#' @param blocked_per_day An optional vector of 24 integers describing which the
#' power that is predetermined for certain hours and cannot be optimised.
#'
#' @return A list of results for each of the three scenarios
#' @export
#' @seealso \code{\link{format_id_prices}}, \code{\link{format_da_prices}},
#' \code{\link{optimise_schedule}}

simulate_marketing <- function(parameters, da_prices, id_prices, id_index_names,
                               blocked_per_day = NULL){
  zero_schedule <- rep(0, length(da_prices))
  if(!is.null(blocked_per_day)){
    if(!all_integer(blocked_per_day)){
      stop("Parameter blocked_per_day must only contain integer.")
    }
    if(length(blocked_per_day) != 24){
      stop("blocked_per_day must be of length 24, one value for each hour.")
    }
    blocked_schedule <- rep(blocked_per_day, length(da_prices) / 24)
    blocked_trades <- describe_trades(blocked_schedule, zero_schedule, da_prices) %>%
      append_trades(NULL, 0)
  } else {
    blocked_schedule <- zero_schedule
    blocked_trades <- NULL
  }
  
  no_optim <- simulate_no_storage(parameters, da_prices)
  
  da_optim <- optimise_schedule(schedule = blocked_schedule,
                                prices = format_da_prices(da_prices),
                                parameters = parameters,
                                shift = parameters["loss_rate"] * 24 - sum(blocked_per_day),
                                blocked = blocked_schedule)
  da_optim$trades <- rbind(da_optim$trades, blocked_trades, make.row.names=FALSE)
  
  id_optim <- optimise_schedule(schedule = da_optim$schedule,
                                prices = format_id_prices(id_prices, id_index_names) %>%
                                  `$`("lookout") %>%
                                  shorten(length(id_index_names)),
                                parameters = parameters,
                                shift = 0,
                                blocked = blocked_schedule)
  
  list(constant = no_optim, da_optim = da_optim, id_optim = id_optim)
}