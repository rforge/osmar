#' @include as-osmar-elements.R



subclass <- function(obj, subclass) {
  structure(obj, class = c(subclass, class(obj)))
}



### OSM parsing: #####################################################

element_parse <- function(obj, element) {
  elem <- list()
  elem$elements <- getNodeSet(obj, path = sprintf("//%s", element))
  elem$IDs <- sapply(elem$elements, xmlGetAttr, "id")

  subclass(elem, sprintf("%s_parsed", element))
}



osm_parse <- function(x) {
  stopifnot(class(x) %in% c("XMLInternalElementNode",
                            "XMLInternalNode",
                            "XMLAbstractNode" ))

  osm <- list()
  osm$nodes <- element_parse(x, "node")
  osm$ways <- element_parse(x, "way")
  osm$relations <- element_parse(x, "relation")

  subclass(osm, "osm_parsed")
}



### OSMAR object construction: #######################################

osmar_elemclass <- function(obj, subclass) {
  stopifnot(all(sapply(obj, class) == "data.frame"))
  subclass(obj, c(subclass, "osmar_element"))
}



osmar_class <- function(obj) {
  stopifnot(length(obj) == 3)
  #stopifnot(sapply(obj,
  # function(k) class(k)[1])==c("NODE", "WAY", "RELATION"))

  subclass(obj, "osmar")
}



#' Convert OSM-XML to an osmar object
#'
#' Convert a given OSM-XML object (as parsed by
#' \code{\link[XML]{xmlParse}}) to an osmar object.
#'
#' @param xml An OSM-XML object
#'
#' @return
#'   A list (with class attribute \code{osmar}) with three elements:
#'
#'   \describe{
#'
#'     \item{\code{node}}{...}
#'
#'     \item{\code{way}}{...}
#'
#'     \item{\code{relation}}{...}
#'
#'   }
#'
#' @aliases osmar
#'
#' @export
as.osmar <- function(xml) {
  osm_parsed <- osm_parse(xmlRoot(xml))

  osm_data <- extract_data(osm_parsed)
  osm_attr <- extract_attr(osm_parsed)
  osm_ref <- extract_ref(osm_parsed)

  osmar <- list()

  osmar$node <- osmar_elemclass(list(osm_attr$nodeattr,
                                     osm_data$nodedata), "node")

  osmar$way <- osmar_elemclass(list(osm_attr$wayattr,
                                    osm_data$waydata,
                                    osm_ref$wayref), "way")

  osmar$relation <- osmar_elemclass(list(osm_attr$relationattr,
                                         osm_data$relationdata,
                                         osm_ref$relationref), "relation")

  osmar_class(osmar)
}
