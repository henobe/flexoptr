#' Find the index of list elements that are not NA
#'
#' @param x a list
#'
#' @return a vector of the numeric indexes where non-NA values are found
identify_non_na_elements <- function(x) {
  x %>%
    lapply(is.na) %>%
    sapply(all) %>%
    `!`() %>%
    which()
}
