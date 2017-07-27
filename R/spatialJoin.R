library(sf)
x = st_read('~/Downloads/Malawi-FTF-ZOI/Malawi_FTF_ZOI.shp')
x = x %>% mutate(obj_id = 1:nrow(x))

n = 1000

library(sp)
y = data.frame(x = sample(round(32.67152*100):round(35.91505*100), n, replace = TRUE)/100,
               y = sample(round(-17.12721*100):round(-13.34228*100), n, replace = TRUE)/100)

coordinates(y) = ~x+y
y = st_as_sf(y)
st_crs(y) = '+proj=longlat +datum=WGS84 +no_defs'
y = st_transform(y, '+proj=longlat +datum=WGS84 +no_defs')

isect = st_intersects(y,x)

y = bind_cols(y, data.frame(obj_id = unlist(lapply(isect, function(x) { ifelse(is_empty(x), NA, x)}))))


ggplot(x, aes(fill = as.factor(obj_id))) +
  geom_sf() +
  geom_sf(shape = 21, color = 'black', data = y, size = 0.25)

library(leaflet)

colPal = leaflet::colorFactor(palette = rep(category20, 5), levels = 1:nrow(x))

leaflet() %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addPolygons(data = x, color = ~colPal(obj_id), weight = 1) %>% 
  addCircles(data = y, color = ~colPal(obj_id), opacity = 1, radius = 100, fillOpacity = 1, stroke = FALSE)
