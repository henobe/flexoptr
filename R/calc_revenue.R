#' Simply calculate overall revenue of a trade data frame
#'
#' @param trade_df A data.frame as exported by \code{describe_trades}
#' 
#' @export
#' @return A single number describing the revenue
calc_revenue <- function(trade_df){
  sum(trade_df$volume * trade_df$prices)
}
