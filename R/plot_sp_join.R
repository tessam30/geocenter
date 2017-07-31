#' Create leaflet map to check spatial join
#' @description after performing a spatial join using spatial_join,
#' creates an interactive leaflet map to check that the polygon/point matching looks correct
#' @import leaflet llamar
#' @export

plot_sp_join = function (df, shp,
                          base_map = providers$Esri.WorldGrayCanvas,
                          radius = 100) {

  # define categorical color palette
  colPal = leaflet::colorFactor(palette = rep(category20, 5), levels = 1:nrow(shp))

  # create leaflet map
  leaflet() %>%
    addProviderTiles(base_map) %>%
    addPolygons(data = shp, color = ~colPal(obj_id), weight = 1) %>%
    addCircles(data = df, color = ~colPal(obj_id),
               radius = radius, fillOpacity = 1, stroke = FALSE)
}
