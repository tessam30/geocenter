#'
#' @export
#'


str_clean = function(string, replacement) {
  str_to_lower(str_replace_all(string, c("\\'" = replacement,
                                         "\\-" = replacement,
                                         "\\," = replacement,
                                         '\\"' = replacement,
                                         "\\!"= replacement)))
}
