#' Make an intra day price list
#'
#' Reformats a data frame of index prices into a format compatible with
#' \code{optimise_schedule}.
#'
#' Price data is typically formatted so that each row represents a product for a
#' certain time. For each row there could a column describing the volume
#' weighted average for the last hour, the last three hours and so on.
#'
#' With this function, this data is transformed into a list format, where each
#' element represents a time and contains the current prices at that point in
#' time for the next hours. The current hour is assumed to be untradeable and
#' therefore an NA is inserted for that time.
#'
#' @param pricetable a data frame with columns of price data. The order of the
#' rows is assumed to be chronological. One row represents one hour.
#' @param colnames a vector of strings. describes the names and order of columns
#' that contain the price data. The first element represents the price one hour
#' before market closure, the second element describes the price two hours
#' before market closure and so on. The names cannot be repeated, so each hour
#' needs to be coded into a separate column.
#'
#' @return the original data frame with the new price list as a further column
#' @export
#'
#' @seealso \code{\link{optimise_schedule}}
#' @examples
#' some_prices <- data.frame(
#'   time = c(1, 2, 3),
#'   id_1 = c(20, 30, 40),
#'   id_2 = c(22, 28, 39),
#'   id_3 = c(25, 27, 41)
#' )
#' format_id_prices(some_prices, c("id_1", "id_2", "id_3"))
format_id_prices <- function(pricetable, colnames) {
  set_price_at_time <- function(df, offsets, varname) {
    values <- `[[`(df, varname)
    offset <- offsets$offset[offsets$varnames == varname]

    values <- values[-(1:offset)]
    c(values, rep(NA, offset))
  }

  offsets <- data.frame(varnames = colnames, offset = seq_along(colnames))
  values <- lapply(offsets$varnames, set_price_at_time,
    df = pricetable,
    offsets = offsets
  )
  values <- values %>%
    unlist() %>%
    matrix(nrow = length(values), byrow = TRUE) %>%
    t()

  values <- cbind(NA, values) %>%
    split(seq(nrow(pricetable)))

  names(values) <- NULL
  pricetable$lookout <- values
  pricetable
}
