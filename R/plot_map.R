#' Plot a basic map
#'
#' Given a data frame containing lat/long, make a basic map
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
                    stroke_colour = 'white',
                    file_name = NA,
                    plot_width = 5,
                    plot_height = 5
                    ) {
  p = ggplot(df, aes_string(x = 'long', y = 'lat',
                  group = 'group', order = 'order',
                  fill = fill_var)) +
    geom_polygon() +
    geom_path(size = stroke_size,
              colour = stroke_colour) +
    coord_equal() +
     theme_void() +
    theme(legend.position = 'none')

  if(!is.na(file_name)) {
    llamar::save_plot(file_name, width = plot_width, height = plot_height)
  }

  return(p)
}
