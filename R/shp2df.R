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
#' @param baseDir string containing the name of the base directotry containing the shapefile
#' @param folderName (optional) string containing the name of the folder within baseDir containing the shapefile. Not required if baseDir contains the full name of the folder containing the shapefile
#' @param layerName string containing the name of the shapefile, e.g. 'district_boundary' for 'district_boundary.shp'. Should not include '.shp'
#' @param exportData Boolean for whether to save the data as a .csv after importing
#' @param fileName string containing the name of the .csv to be exported; by default, will be the same as the inputted .shp file
#' @param labelVar string containing the variable name containing the names of the polygons (for instance, the names of the provinces, districts, etc.)
#' @param reproject Boolean specifying whether to reproject the data
#' @param projection CRS projection string to standardize the projection of the shapefile
#'
#' @export
#' @examples
#'


# -- Function to import shapefiles and convert to a ggplot-able object --
shp2df = function(baseDir = getwd(),
                  folderName = NULL,
                  layerName,
                  exportData = TRUE,
                  fileName = layerName,
                  getCentroids = TRUE,
                  labelVar = NA,
                  reproject = TRUE, projection = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") {

  # Check that the layerName doesn't contain any extensions
  # Check that layerName exists within the wd

  # Log the current working directory, to change back at the end.
  currentDir = getwd()

  # Read in the raw shapefile
  rawShp = read_shp(baseDir = baseDir, folderName = folderName, layerName = layerName)

  if (reproject == TRUE) {
    # reproject the data
    projectedShp = sp::spTransform(rawShp, sp::CRS(projection))
  } else {
    projectedShp = rawShp
  }
  # pull out the row names from the data and save it as a new column called 'id'
  projectedShp@data$id = rownames(projectedShp@data)

  # Convert the shape polygons into a series of lat/lon coordinates.
  poly_points = ggplot2::fortify(projectedShp, region = "id")

  # Merge the polygon lat/lon points with the original data
  df = dplyr::left_join(poly_points, projectedShp@data, by = "id")

  if (getCentroids == TRUE){
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
      write.csv(df, paste0(baseDir, "/", fileName, ".csv"))
      write.csv(centroids, paste0(baseDir, "/", fileName, "_centroids.csv"))
    }


    # Return the dataframe containing the coordinates and the centroids
    return(list(df = df, centroids = centroids))
  } else {
    # if the 'exportData' option is selected, save the lat/lon coordinates as a .csv
    if (exportData == TRUE) {
      write.csv(df, paste0(baseDir, "/", fileName, ".csv"))
    }

    # Reset the working directory
    setwd(currentDir)

    # Return the dataframe containing the coordinates and the centroids
    return(df)
  }
}
