extract_ref <- function(x){
  UseMethod("extract_ref")
}

extract_ref.osm_parsed <- function(parsed){
  wayref <- extract_ref(parsed[[2]])
  relationref <- extract_ref(parsed[[3]])
  list(wayref=wayref, relationref=relationref)
}

extract_ref.way_parsed <- function(wparsed){
  XMLclone<- lapply(wparsed$elements, xmlClone)
  XMLclone<- removeKids(XMLclone, "tag")
  XMLclone<- XMLclone[which(sapply(XMLclone, xmlSize)!=0)]
  if(length(XMLclone)==0) 
    return(data.frame(id=character(), ref=character()))
  do.call("rbind", lapply(XMLclone, xml2long, "member"))
}

extract_ref.relation_parsed <- function(rparsed){
  XMLclone<- lapply(rparsed$elements, xmlClone)
  XMLclone<- removeKids(XMLclone, "tag")
  XMLclone<- XMLclone[which(sapply(XMLclone, xmlSize)!=0)]
  if(length(XMLclone)==0) 
    return(data.frame(id=character(), ref=character(), role=character()))
  do.call("rbind", lapply(XMLclone, xml2long, "member"))
}