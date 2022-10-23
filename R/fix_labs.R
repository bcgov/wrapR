#' @title fix labels on a ggplot2 object
#' @param gg a ggplot2 object
#' @return a ggplot2 object
#' @seealso
#'  \code{\link[ggplot2]{labs}}
#'  \code{\link[stringr]{case}},\code{\link[stringr]{str_replace}}
#' @rdname fix_labs
#' @export
#' @importFrom ggplot2 labs
#' @importFrom stringr str_to_title str_replace_all
fix_labs <- function(gg){
  gg <- gg+
    ggplot2::labs(title=stringr::str_to_title(stringr::str_replace_all(gg$labels$title, "_", " ")),
                  x=stringr::str_to_title(stringr::str_replace_all(gg$labels$x, "_", " ")),
                  y=stringr::str_to_title(stringr::str_replace_all(gg$labels$y, "_", " ")),
                  colour=stringr::str_to_title(stringr::str_replace_all(gg$labels$colour, "_", " ")),
                  fill=stringr::str_to_title(stringr::str_replace_all(gg$labels$fill, "_", " ")),
                  edge_colour=stringr::str_to_title(stringr::str_replace_all(gg$labels$edge_colour, "_", " ")))
  return(gg)
}
