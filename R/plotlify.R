#' @title convert a ggplot to a plotly
#' @description takes a ggplot and converts it to a plotly object where user can specify font size and caption
#' @param a_ggplot a ggplot object
#' @param caption The caption
#' @param font_size the base font size
#' @return a plotly object
#' @rdname plotlify
#' @export
#' @importFrom plotly ggplotly
#' @importFrom plotly layout
#' @importFrom plotly config

plotlify <- function(a_ggplot, caption, font_size){
  plt <- a_ggplot+
    theme_minimal(base_size = font_size)
  plotly::ggplotly(plt)%>%
    plotly::layout(font=list(
      family = "Franklin Gothic"),
      margin = list(b=120,t=70, l=100),
      annotations = list(x = 1, y = -0.4, text = paste("<i>", caption, "</i>"),
                         showarrow = F, xref='paper', yref='paper',
                         xanchor='right', yanchor='auto', xshift=-100, yshift=100, font=list(size=10))
    )%>%
    plotly::config(
      toImageButtonOptions = list(
        format = "svg",
        width = 960,
        height = 720
      )
    )
}
