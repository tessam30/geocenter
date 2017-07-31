#' Spatial join a data frame to polygons
#'
#' @description Imports a shp file and a data frame loaded into R, and does a spatial
#' join between the points in the data frame and polygons in the shapefile
#'
#' @import sf sp dplyr purr
#' @export
#'
#' @examples
#' n = 2000
#' df = data.frame(lon = sample(round(32.67152*100):round(35.91505*100), n, replace = TRUE)/100, lat = sample(round(-17.12721*100):round(-13.34228*100), n, replace = TRUE)/100)


library(sf)
library(ggplot2)
library(leaflet)
library(dplyr)
library(purrr)
library(llamar)

spatial_join = function (shp_file,
                         df,
                         proj_crs,
                         lat_var = 'lat',
                         lon_var = 'lon',
                         df_crs = '+proj=longlat +datum=WGS84 +no_defs',
                         binary_var = 'inPolygon',
                         merge2shp = TRUE,
                         plot_results = TRUE) {
  # Part 1. -- Shapefile import --
  # Read in the shapefile
  shp = st_read(shp_file)


  shp = shp %>%
    # create an object id to later merge the joined points to
    mutate(obj_id = 1:nrow(shp)) %>%
    # transform to the intended projection
    st_transform(proj_crs)


  # Part 2. -- Define data frame as a spatial object --
  # using sp package, define the variables that contain lat/long to convert to spatial object
  coordinates(df) = as.formula(paste0('~', lon_var, '+', lat_var))
  # convert from sp spatial obj to sf spatial obj
  df = st_as_sf(df)
  # define the projection for the coordinates.
  st_crs(df) = df_crs
  # re-project to common projection system
  df = st_transform(df, proj_crs)

  # Part 3. -- Do the spatial join to id which polygons are associated with which points. --
  isect = st_intersects(df, shp)

  # first replace all points which didn't join to a polygon as NA;
  # rest are associated with the obj_id (row number) of the polygon they match.
  # then bind the obj_id to the existing data frame
  df = bind_cols(df, data.frame(obj_id = unlist(lapply(isect,
                                                       function(x) {
                                                         ifelse(is_empty(x), NA, x)})))) %>%
    # create a binary variable, which is whether the point is located in the polygon
    mutate_(.dots = setNames('!is.na(obj_id)', binary_var))

  # Part 4. -- Merge the df to the shapefile --
  # ignore the actual coordinates of the polygon
  if(merge2shp == TRUE) {
    df = df %>%
      left_join(shp %>% as_data_frame() %>% select(-geometry), by = 'obj_id')
  }

  # run leaflet function
  if(plot_results == TRUE){
    print(plot_sp_join(df, shp))
  }

  return(df)

}
