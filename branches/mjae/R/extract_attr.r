extract_attr <- function(x){
  UseMethod("extract_attr")
}

extract_attr.osm_parsed <- function(parsed){
  nodeattr <- extract_attr(parsed[[1]])
  wayattr <- extract_attr(parsed[[2]])
  relationattr <- extract_attr(parsed[[3]])
  list(nodeattr=nodeattr, wayattr=wayattr, relationattr=relationattr)
}

extract_attr.node_parsed<- function(elparsed){
  ret <- lapply(elparsed$elements, xmlAttrs)
  if(any(sapply(ret, length)!=9)){
    ret<-as.data.frame(do.call("smartbind", ret))
  } else{
    ret<-as.data.frame(do.call("rbind", ret))
  }
  ret$timestamp <- strptime(ret$timestamp, format="%Y-%m-%dT%H:%M:%S")
  ret$lat<- as.numeric(as.character(ret$lat))
  ret$lon<- as.numeric(as.character(ret$lon))
  ret
}

extract_attr.way_parsed<-
extract_attr.relation_parsed <- function(elparsed){
  ret <- lapply(elparsed$elements, xmlAttrs)
  if(any(sapply(ret, length)!=7)){
    ret<-as.data.frame(do.call("smartbind", ret))
  } else{
    ret<-as.data.frame(do.call("rbind", ret))
  }
  ret$timestamp <- strptime(ret$timestamp, format="%Y-%m-%dT%H:%M:%S")
  ret
}