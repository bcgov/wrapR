#' @title Make a choropleth map of BC economic regions
#' @param tbbl a tibble that contains (at least) the following 3 columns:
#' @param region the column of the tbbl that contains the economic regions: these MUST be camel_case i.e. mainland_south_west, vancouver_island_coast, north_coast_&_nechako, cariboo, kootenay, north_east, thompson_okanagan
#' @param thingy the column of the tbbl that contains the name of what is being plotted.
#' @param value the column of the tbble that contains the value of what is being plotted.
#' @param num_format whether the number being plotted should be formatted as a comma or percent
#' @rdname bc_reg_choro
#' @import dplyr
#' @import leaflet
#' @import stringr
#' @import bcmaps
#' @import sf
#' @import htmltools
#' @import scales
#' @export
bc_reg_choro<-function(tbbl, region, thingy, value, num_format) {
  tbbl <- tbbl%>%
    rename(region =  {{  region  }},
           thingy = {{  thingy  }},
           value = {{  value  }}
    )%>%
    ungroup()

  shape <- bc_reg_sf()

  tbbl <- shape%>%
    left_join(tbbl, multiple = "all")

  variable_plotted <- tbbl$thingy[1]

  pal <- colorNumeric("viridis", domain = tbbl$value)
  pal_rev <- colorNumeric("viridis", domain = tbbl$value, reverse = TRUE)

  if(num_format=="percent"){
    form_val <- scales::percent(tbbl$value, accuracy = .1)
  }else if(num_format=="comma"){
    form_val <- scales::comma(tbbl$value, accuracy = 100)
  }else{
    stop("num_format needs to be either percent or comma")
  }
  mytext <- paste(
    "Region: ", str_to_title(str_replace_all(tbbl$region,"_"," ")),"<br/>",
    str_to_title(str_replace_all(variable_plotted,"_"," ")), ": ", form_val, "<br/>",
    sep="") %>%
    lapply(htmltools::HTML)
  plt <- leaflet(tbbl,
          options = leafletOptions(
            attributionControl = FALSE
          )
  ) %>%
    setView(lng = -125, lat = 55, zoom = 5) %>%
    addProviderTiles("Esri.NatGeoWorldMap") %>%
    addPolygons(
      fillColor = ~ pal(value),
      color = "black",
      label=mytext,
      fillOpacity = .5,
      weight = 1
    )
  if(num_format=="percent"){
    plt%>%
      addLegend("topright",
                pal = pal_rev,
                values = ~ value,
                labFormat = labelFormat(
                  suffix="%",
                  transform = function(x) 100*sort(x, decreasing = TRUE)),
                title = str_to_title(str_replace_all(variable_plotted,"_"," ")))
  }else if(num_format=="comma"){
    plt%>%
      addLegend("topright",
                pal = pal,
                values = ~ value,
                title = str_to_title(str_replace_all(variable_plotted,"_"," ")))
  }else{
    stop("num_format needs to be either percent or comma")
  }
}
