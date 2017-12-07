#library(sf)
#library(tidyverse)
#load("~/Documents/GitHub/geocenter/data/ctry_codes.rda")

#world = st_read('~/Documents/USAID/worldshapefilestatedeptwmayrocreatingwestbankgaza/WorlPoly_5mile_dissolved_v2.shp')

#st_buffer(world, dist = 10)

#indiv_plot = function(geo, country) {

#  crs = ctry_codes %>% filter(Name == !!country) %>% pull(crs)

#  geo = st_transform(geo %>% filter(Name == country), crs)

#  dims = st_bbox(geo)

#  max_dims = max(dims['ymax'] - dims['ymin'])

#  buffered = st_buffer(geo, 0.1*max_dims)

#  ggplot(geo, aes(fill = REGION)) +
#    geom_sf() +
 #   geom_sf(data = buffered, fill = NA, colour = 'red') +
  #  scale_fill_discrete(guide = FALSE) +
   # ggtitle(stringr::str_to_title(country)) +
    #theme_void()
#}

#countries = c('AFGHANISTAN', 'GEORGIA', 'JORDAN')
#maps = lapply(countries, function(x) indiv_plot(world, x))

#multiplot(maps[[1]], maps[[2]], maps[[3]], cols = 1)
#ggsave(plot = x, filename = '~/Documents/USAID/test.pdf')
