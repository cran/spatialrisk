#' Create choropleth map
#'
#' @description Takes an object produced by \code{points_to_polygon()}, and creates the corresponding choropleth map.
#'
#' @param sf_object object of class sf
#' @param value column name to shade the polygons
#' @param id_name column name of ids to plot
#' @param mode choose between static ('plot' is default) and interactive map ('view')
#' @param n number of clusters (default is 7)
#' @param legend_title title of legend
#' @param palette palette name or a vector of colors. See tmaptools::palette_explorer() for the named palettes. Use a "-" as prefix to reverse the palette. The default palette is "viridis".
#'
#' @return tmap
#' @export choropleth
#'
#' @import sf
#' @import tmap
#' @import viridis
#'
#' @author Martin Haringa
#'
#' @examples
#' test <- points_to_polygon(nl_provincie, insurance, sum(amount, na.rm = TRUE))
#' choropleth(test)
#' choropleth(test, id_name = "areaname", mode = "view")
choropleth <- function(sf_object, value = "output", id_name = "areaname",
                            mode = "plot", n = 7, legend_title = "Clustering",
                            palette = "viridis"){

  if (mode == "view"){
    suppressMessages({
      tmap_mode("view")
    })

    output <- tm_shape(sf_object) +
      tm_polygons(value,
                  id = id_name,
                  palette = palette,
                  style = "fisher",
                  n = n,
                  title = legend_title,
                  alpha = .5) +
      tm_basemap(c("OpenStreetMap", "Esri.WorldGrayCanvas", "Esri.WorldTopoMap"))
  }

  else{
    suppressMessages({
      tmap_mode("plot")
    })
    output <- tm_shape(sf_object) +
      tm_polygons(value,
                  id = id_name,
                  palette = palette,
                  style = "fisher",
                  title = legend_title,
                  n = n,
                  lwd = .1) +
      tm_compass(position = c("right", "bottom")) +
      tm_scale_bar(position = c("left", "bottom"))
  }

  return(output)
}