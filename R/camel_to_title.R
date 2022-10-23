#' @title convert all factors in a tibble to Title Case character/
#' @param tbbl a tibble
#' @return a tibble
#' @export
#' @import tibble
#' @import dplyr
camel_to_title <- function(tbbl) {
  tbbl %>%
    rapply(as.character, classes = "factor", how = "replace") %>%
    tibble() %>%
    mutate(across(where(is.character), make_title))
}
