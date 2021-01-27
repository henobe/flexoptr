#' Shorten a vector
#'
#' @param x a vector
#' @param by an integer value, giving the non-negative number of elements that
#' should be left out, counted from the end of the vector.
#'
#' @return a vector
#' @export
#'
#' @examples
#' vec <- seq(10)
#' shorten(vec, 3)
shorten <- function(x, by){
  if(by >= length(x)) stop("By argument too large, cannot index vector")
  x[1:(length(x) - by)]
}
