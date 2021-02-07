#' Check whether all elements of a vector are integer
#'
#' @param x a vector
#'
#' @return a logical value
#' @noRd
all_integer <- function(x) {
  sapply(x, function(x) x %% 1 == 0) %>%
    all()
}
