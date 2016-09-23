#' Import ESRI shapefiles into R
#'
#' Import shapefile into an R spatial polygons object. Useful for plotting within the leaflet package,
#' or for downstream use to convert into a series of lat/lon coordinates.
#'
#'
#' @author Laura Hughes, laura.d.hughes@gmail.com
#'
#' @import rgdal
#'
#' @param workingDir string containing the name of the folder containing the shapefile
#' @param layerName string containing the name of the shapefile, e.g. 'district_boundary' for 'district_boundary.shp'. Should not include '.shp'
#'
#' @export
#' @examples

# -- Function to import shapefiles --
read_shp = function(baseDir = getwd(),
                    folderName = NULL,
                    layerName) {
  # Check that the layerName doesn't contain any extensions
  # Check that layerName exists within the wd

  # Log the current working directory, to change back at the end.
  currentDir = getwd()

  # Change directory to the file folder containing the shape file
  setwd(paste0(baseDir, folderName))

  # the dsn argument of '.' says to look for the layer in the current directory.
  rawShp = readOGR(dsn = ".", layer = layerName)
}
