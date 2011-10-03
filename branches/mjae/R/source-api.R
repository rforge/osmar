#' @include get.R
{}


osmsource_api <- function(url = "http://api.openstreetmap.org/api/0.6/map") {
  osmsource(list(url = url), "api")
}


get_osm_data.api <- function(source, what, ...) {
  if ( size(what) >= 0.25 )
    stop("BoundingBox is bigger than 0.25-Square-Degrees\n")

  request <- osm_request(source, what)
  response <- getURL(request, .encoding="UTF-8")

  response
}


setMethod("osm_request", signature = c("api", "bbox"),
function(source, what, ...) {
  sprintf("%s?bbox=%s,%s,%s,%s", source$url,
          what[1], what[2], what[3], what[4])
})

