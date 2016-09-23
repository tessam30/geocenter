#' Import ESRI shapefiles into R
#'
#' Import shapefile and convert into lat/lon coordinates to plot in R or Tableau
#'
#' View available projections with 'projInfo(type = 'proj')'
#' View available datum with 'projInfo(type = 'datum')'
#' View available ellipsoids with 'projInfo(type = 'ellps')'
#'
#' @author Laura Hughes, laura.d.hughes@gmail.com
#'
#' @import ggplot2
#' @import rgdal
#' @import maptools
#' @import dplyr
#'
#' @param workingDir string containing the name of the folder containing the shapefile
#' @param layerName string containing the name of the shapefile, e.g. 'district_boundary' for 'district_boundary.shp'
#' @param exportData Boolean for whether to save the data as a .csv after importing
#' @param fileName string containing the name of the .csv to be exported; by default, will be the same as the inputted .shp file
#' @param labelVar string containing the variable name containing the names of the polygons (for instance, the names of the provinces, districts, etc.)
#' @param reproject Boolean specifying whether to reproject the data
#' @param projection CRS projection string to standardize the projection of the shapefile
#'
#' @export
#' @examples
#'
#'
#'

importShp = function(workingDir = getwd(),
                     layerName,
                     exportData = TRUE,
                     fileName = layerName,
                     labelVar = NA,
                     reproject = TRUE, projection = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") {

  # Check that the layerName doesn't contain any extensions
  # Check that layerName exists within the wd

  # Log the current working directory, to change back at the end.
  currentDir = getwd()

  # Change directory to the file folder containing the shape file
  setwd(workingDir)

  # the dsn argument of '.' says to look for the layer in the current directory.
  rawShp = rgdal::readOGR(dsn = ".", layer = layerName)

  if (reproject == TRUE) {
    # reproject the data
    projectedShp = spTransform(rawShp, CRS(projection))
  } else {
    projectedShp = rawShp
  }
  # pull out the row names from the data and save it as a new column called 'id'
  projectedShp@data$id = rownames(projectedShp@data)

  # Convert the shape polygons into a series of lat/lon coordinates.
  poly_points = ggplot2::fortify(projectedShp, region = "id")

  # Merge the polygon lat/lon points with the original data
  df = dplyr::left_join(poly_points, projectedShp@data, by = "id")


  # Pull out the centroids and the associated names.
  centroids = data.frame(coordinates(projectedShp)) %>% rename(long = X1, lat = X2)

  if (!is.na(labelVar)) {
    if (labelVar %in% colnames(projectedShp@data)) {
      # Merge the names with the centroids
      centroids = cbind(centroids, projectedShp@data[labelVar]) %>% rename_(label = labelVar)  # rename the column
    } else {
      warning("label variable for the centroids is not in the raw shapefile")
    }
  }

  # if the 'exportData' option is selected, save the lat/lon coordinates as a .csv
  if (exportData == TRUE) {
    write.csv(df, paste0(workingDir, "/", fileName, ".csv"))
    write.csv(centroids, paste0(workingDir, "/", fileName, "_centroids.csv"))
  }


  # Return the dataframe containing the coordinates and the centroids
  return(list(df = df, centroids = centroids))
}
