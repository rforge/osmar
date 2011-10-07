#' @include get.R
#' @include source.R
{}


osmsource_osmosis <- function(file, osmosis = "osmosis") {
  osmsource(list(file = file, osmosis = osmosis), "osmosis")
}


get_osm_data.osmosis <- function(source, what, ...) {
  destination <- tempfile()

  request <- osm_request(source, what, destination)
  ret <- system(request, ...)
  response <- readLines(destination)

  response
}


setMethod("osm_request", signature = c("osmosis", "bbox"),
function(source, what, destination, ...) {
  fin <- sprintf("--read-xml enableDateParsing=no file=%s", source$file)
  fout <- sprintf("--write-xml file=%s", destination)

  args <- sprintf("--bounding-box top=%s left=%s bottom=%s right=%s",
                  what["top"], what["left"], what["bottom"], what["right"])

  sprintf("%s %s %s %s", source$osmosis, fin, args, fout)
})



setMethod("osm_request", signature = c("osmosis", "element"),
function(source, what, ...) {
  stop("Not implemented yet.")

  fin <- sprintf("--read-xml enableDateParsing=no file=%s", source$file)
  fout <- sprintf("--write-xml file=%s", destination)

  args <- ""


  sprintf("%s %s %s %s", source$osmosis, fin, args, fout)
})




### Special omsosis requester:

setOldClass(c("osmosis_args"))

osmosis_args <- function(...) {
  structure(paste(..., collapse = " "), class = "osmosis_args")
}

setMethod("osm_request", signature = c("osmosis", "osmosis_args"),
function(source, what, destination, ...) {
  fin <- sprintf("--read-xml enableDateParsing=no file=%s", source$file)
  fout <- sprintf("--write-xml file=%s", destination)

  args <- unclass(what)

  sprintf("%s %s %s %s", source$osmosis, fin, args, fout)
})


