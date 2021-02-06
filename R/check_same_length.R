#' Check all elements for length equality
#'
#' @param ... any number of vectors
#'
#' @return TRUE or FALSE
check_same_length <- function(...){
  sapply(list(...), length) %>%
    unique() %>%
    length() == 1
}
