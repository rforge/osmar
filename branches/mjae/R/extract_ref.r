extract_ref <- function(x){
  UseMethod("extract_ref")
}

extract_ref.osm_parsed <- function(parsed){
  wayref <- extract_ref(parsed[[2]])
  relationref <- extract_ref(parsed[[3]])
  list(wayref=wayref, relationref=relationref)
}

extract_ref.way_parsed <-
extract_ref.relation_parsed <- function(wparsed){
  XMLclone<- lapply(wparsed$elements, xmlClone)
  XMLclone<- removeKids(XMLclone, "tag")
  XMLclone<- XMLclone[which(sapply(XMLclone, xmlSize)!=0)]
  do.call("rbind", lapply(XMLclone, xml2long, "member"))
}