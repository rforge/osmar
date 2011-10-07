#' @include get.R
#' @include source.R
{}


osmsource_api <- function(url = "http://api.openstreetmap.org/api/0.6/") {
  osmsource(list(url = url), "api")
}


get_osm_data.api <- function(source, what, ...) {
  request <- osm_request(source, what)
  response <- getURL(request, .encoding = "UTF-8")

  response
}



setMethod("osm_request", signature = c("api", "bbox"),
function(source, what, ...) {
  if ( size(what) >= 0.25 )
    stop("BoundingBox is bigger than 0.25-Square-Degrees\n")

  sprintf("%smap/?bbox=%s,%s,%s,%s", source$url,
          what["left"], what["bottom"], what["right"], what["top"])
})



setMethod("osm_request", signature = c("api", "element"),
function(source, what, ...) {
  element <- class(what)[1]
  sprintf("%s%s/%s", source$url, element, what["id"])
})

