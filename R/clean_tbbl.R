#' @title Wrapper function that calls clean_names and converts character columns to clean factors
#' @param tbbl a tibble
#' @return a clean tibble
#' @export
#' @import janitor
#' @import dplyr
#' @import magrittr
clean_tbbl <- function(tbbl) {
  tbbl %>%
    janitor::clean_names() %>%
    mutate(across(where(is.character), make_clean_factor))
}
