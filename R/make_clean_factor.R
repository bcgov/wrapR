#' @title dirty string to factor with camel_case levels.
#' @param strng a string
#' @return a factor
#' @export
#' @import stringr
#' @import magrittr
make_clean_factor <- function(strng) {
  strng %>%
    stringr::str_replace_all("\t", "") %>%
    trimws() %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all(" ", "_") %>%
    stringr::str_replace_all("-", "_") %>%
    factor()
}
