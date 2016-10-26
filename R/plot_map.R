#' Import ESRI shapefiles into R data frame
#'
#' Import shapefile and convert into lat/lon coordinates to plot in R or Tableau
#'
#' View available projections with 'projInfo(type = 'proj')'
#' View available datum with 'projInfo(type = 'datum')'
#' View available ellipsoids with 'projInfo(type = 'ellps')'
#'
#' @author Laura Hughes, laura.d.hughes@gmail.com
#'
#' @import dplyr ggplot2 rgdal maptools
#'
#'
#' @export
#' @examples
#'

plot_map = function(df,
                    fill_var,
                    stroke_size = 0.15,
                    stroke_colour = 'white') {
  ggplot(df, aes_string(x = 'long', y = 'lat',
                  group = 'group', order = 'order',
                  fill = fill_var)) +
    geom_polygon() +
    geom_path(size = stroke_size,
              colour = stroke_colour) +
    coord_equal() +
     theme_void() +
    theme(legend.position = 'none')
}
