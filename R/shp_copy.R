ner = st_read('~/Documents/Niger/geodata/niger_dhs_2012.shp')

regions = as.character(ner$DHSREGEN)


shp_copy = function(shp, regions) {

  for(i in 1:length(regions)){
    if(i > 1){
      shp_copy = shp %>%
        mutate(region = regions[i],
               fill = region == DHSREGEN) %>%
        rbind(shp_copy)
    } else {
      shp_copy = shp %>%
        mutate(region = regions[i],
               fill = region == DHSREGEN)
    }
  }

  return(shp_copy)

}

x = shp_copy(ner, regions)
ner2 = rbind(ner %>% mutate(region = 'Agadez', fill = region == DHSREGEN), ner %>% mutate(region = 'Diffa', fill = region == DHSREGEN))

facet_maps = function(shp,
                      facet_var = 'region',
                      fill_var = 'fill',
                      sort_order = NULL,
                      yes_colour = ftfOrange,
                      no_colour = grey15K,
                      stroke_colour = grey60K,
                      stroke_size = 0.25,
                      ncol = 1) {

  if(!is.null(sort_order)) {
    facet_formula = as.formula(paste0('~ forcats::fct_relevel(', facet_var, ', sort_order)'))
  } else{
    facet_formula = as.formula(paste0('~', facet_var))
  }

  ggplot(shp, aes_string(fill = fill_var)) +
    geom_sf(size = stroke_size, colour = stroke_colour) +
    scale_fill_manual(values = c("TRUE" = yes_colour, "FALSE" = no_colour), guide = FALSE) +
    facet_wrap(facet_formula, ncol = ncol) +
    theme_void()
}
