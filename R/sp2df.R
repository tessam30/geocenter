#' Convert a spatial data frame into a series of lat/lon coordinates
#'
#' @author Laura Hughes, laura.d.hughes@gmail.com
#'
#' @import ggplot2
#'
#'
#' @export
#' @examples
#'


sp2df = function(sp_df){
  # pull out the row names from the data and save it as a new column called 'id'
  sp_df@data$id = rownames(sp_df@data)

  # Convert the shape polygons into a series of lat/lon coordinates.
  poly_points = ggplot2::fortify(sp_df, region = "id")

  # Merge the polygon lat/lon points with the original data
  df = dplyr::left_join(poly_points, sp_df@data, by = "id")
}
