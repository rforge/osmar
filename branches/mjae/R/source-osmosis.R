#' @include get.R
{}


osmsource_osmosis <- function(file, osmosis = "osmosis") {
  osmsource(list(file = file, osmosis = osmosis), "osmosis")
}


get_osm_data.osmosis <- function(source, what, ...) {
  destination <- tempfile()
  request <- osm_request(source, what, destination)

  response <- system(request)

  readLines(destination)
}


setMethod("osm_request", signature = c("osmosis", "bbox"),
function(source, what, destination, ...) {
  p1 <- sprintf("--read-xml enableDateParsing=no file=%s",
                source$osmosis, source$file)
  p2 <- sprintf("--bounding-box top=%s left=%s bottom=%s right=%s",
                what[1], what[2], what[3], what[4])

  sprintf("%s %s %s --write-xml file=%s",
          source$osmosis, p1, p2, destination)
})

