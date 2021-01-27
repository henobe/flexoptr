#' Check whether all elements of a vector are integer
#'
#' @param x a vector
#'
#' @return a logical value
all_integer <- function(x){
  (all(sapply(x, function(x) x%%1 == 0)))
}
