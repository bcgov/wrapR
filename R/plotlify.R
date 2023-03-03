#' @title convert a ggplot to a plotly
#' @description takes a ggplot and converts it to a plotly object where user can specify font size and caption
#' @param a_ggplot a ggplot object
#' @param caption The caption
#' @param font_size the base font size
#' @param pal the colour palette
#' @return a plotly object
#' @rdname plotlify
#' @export
#' @importFrom plotly ggplotly
#' @importFrom plotly layout
#' @importFrom plotly config

plotlify <- function(a_ggplot, caption, font_size, pal="Dark2"){

  a_ggplot[["labels"]][["fill"]] <-  paste0(a_ggplot[["labels"]][["fill"]],"   ")
  a_ggplot[["labels"]][["color"]] <-  paste0(a_ggplot[["labels"]][["color"]],"   ")

  if (pal == "Viridis") {
    plt <- a_ggplot+
      scale_colour_viridis_d() +
      scale_fill_viridis_d()+
      theme_minimal(base_size = font_size)
  } else {
    plt <- a_ggplot+
      scale_colour_brewer(palette = pal) +
      scale_fill_brewer(palette = pal)+
      theme_minimal(base_size = font_size)
  }

  myplot <- plotly::ggplotly(plt)%>%
    plotly::layout(font=list(
      family = "Franklin Gothic"),
      margin = list(b=120, l=100, r=100, t=80),
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

  for (i in 1:length(myplot$x$data)){
    if (!is.null(myplot$x$data[[i]]$name)){
      myplot$x$data[[i]]$name =  gsub("\\(","",str_split(myplot$x$data[[i]]$name,",")[[1]][1])
    }
  }
myplot
}
