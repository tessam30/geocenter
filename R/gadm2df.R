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


gadm2df = function(sp_df){
  sp_df@data$id = rownames(sp_df@data)

  # Convert the shape polygons into a series of lat/lon coordinates.
  poly_points = ggplot2::fortify(sp_df, region = "id")

  # Merge the polygon lat/lon points with the original data
  df = dplyr::left_join(poly_points, sp_df@data, by = "id")
}
