#' Lookup CRS country-level projection string based on country name or code
#'
#' @description Matches *exactly* (though case insensitive) to find the CRS PROJ4 string for a
#' suitable equal area projection for a given country. PROJ4 strings provided from http://projectionwizard.org/
#'
#' @import stringr dplyr tidyr
#' @export
#'
#' @param country country name or id.
#'
#' @examples
#' # Cases and punctuation are ignored.
#' lookup_crs("Côte d'Ivoire")
#' lookup_crs("TIMOR-LESTE")
#' lookup_crs("timor leste")
#' # Also takes ISO codes
#' lookup_crs("RWA")
#' # If no exact match to a country is found, it'll suggest close options.
#' lookup_crs("SUD")

lookup_crs  = function(ctry) {
  # convert to all lower case, and use exact regex matching
  sel_ctry = paste0("^", str_clean(ctry, ' '), "$")

  # convert lookup table to a long format:
  lookup  = ctry_codes %>%
    gather(code_type, country, -REGION, -capital, -crs) %>%
    mutate(country_clean = str_clean(country, ' '))

  # search for a match
  matched = lookup %>%
    filter(str_detect(country_clean, sel_ctry)) %>%
    group_by(crs) %>%
    summarise(country_names = list(unique(country)))

  if (nrow(matched) == 1) {

    if(length(matched$country_names[[1]]) > 1) {
      print(paste0("Found a match for ", ctry, ", a.k.a. ", paste(setdiff(matched$country_names[[1]], ctry), collapse = ', ')))
    } else {
      print(paste0("Found a match for ", ctry))
    }

    return(matched$crs)

  } else if (nrow(matched) > 1){
    print("Multiple countries found. Which did you mean?")


    lapply(1:nrow(matched), function(x) print(paste0("Found a match for ", ctry, ", a.k.a. ",
                                                     paste(setdiff(matched$country_names[[x]], ctry), collapse = ', '))))
  } else {
    print("No matches found.")

    inexact = lookup %>%
      filter(str_detect(country_clean, str_clean(ctry, ' '))) %>%
      distinct(country) %>%
      pull(country)

    if(length(inexact > 0)) {
      print(paste0("Did you mean ", inexact, "?"))
    }
  }

}
