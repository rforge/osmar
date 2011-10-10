osm_parse <- function(x){
  stopifnot(class(x) %in% c("XMLInternalElementNode","XMLInternalNode","XMLAbstractNode" ))

  osmnodes<- element_parse(x, "node")
  osmways<- element_parse(x, "way")
  osmrelations<- element_parse(x, "relation")
  structure(list(osmnodes, osmways, osmrelations), class="osm_parsed")
}

element_parse <- function(obj, element) {
  elements <- getNodeSet(obj, path=sprintf("//%s", element))
  IDs <- sapply(elements, xmlGetAttr, "id")
  structure(list(elements=elements, IDs=IDs), class = sprintf("%s_parsed", element))
}