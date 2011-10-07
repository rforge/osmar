#' @include get.R
#' @include source.R
{}


source_osm_xapi <- function(url = "") {
  osmsource(list(url = url), "xapi")
}
