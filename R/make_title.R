#' @title converts a camel_case string to Title Case.
#' @param strng a camel_case string
#' @return a Title Case string
#' @export
#' @import stringr
make_title <- function(strng){
  strng <- str_to_title(str_replace_all(strng,"_"," "))
}
