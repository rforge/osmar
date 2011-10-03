
osmsource <- function(obj, subclass) {
  structure(obj, class = c(subclass, "osmsource"))
}


get_osm_data <- function(source, ...) {
  UseMethod("get_osm_data")
}


setOldClass(c("api", "osmosis"))
setOldClass(c("bbox"))

setGeneric("osm_request",
function(source, what, ...) {
  standardGeneric("osm_request")
})

