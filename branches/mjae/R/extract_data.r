extract_data <- function(x){
  UseMethod("extract_data")
}

extract_data.osm_parsed <- function(parsed){
  nodedata <- extract_data(parsed[[1]])
  waydata <- extract_data(parsed[[2]])
  relationdata <- extract_data(parsed[[3]])
  list(nodedata=nodedata, waydata=waydata, relationdata=relationdata)
}

extract_data.node_parsed <- function(nparsed){
  values<-nparsed$elements[which(sapply(nparsed$elements,xmlSize)!=0)]
  ##auswahl der nodes MIT daten
  if(length(values)==0) 
    return(data.frame(id=character(), k=character(), v=character()))
  do.call("rbind", lapply(values, xml2long, "data"))
}

extract_data.way_parsed <- function(wparsed){
  XMLclone<- lapply(wparsed$elements, xmlClone)
  XMLclone<- removeKids(XMLclone, "nd")
  XMLclone<- XMLclone[which(sapply(XMLclone, xmlSize)!=0)]
  if(length(XMLclone)==0) 
    return(data.frame(id=character(), k=character(), v=character()))
  do.call("rbind", lapply(XMLclone, xml2long, "data"))
}

extract_data.relation_parsed <- function(rparsed){
  XMLclone<- lapply(rparsed$elements, xmlClone)
  XMLclone<- removeKids(XMLclone, "member")
  XMLclone<- XMLclone[which(sapply(XMLclone,xmlSize)!=0)]
  if(length(XMLclone)==0) 
    return(data.frame(id=character(), k=character(), v=character()))
  do.call("rbind", lapply(XMLclone, xml2long, "data"))
}

xml2long <- function(x, dfType){
  size<-xmlSize(x)
  ret<-data.frame(id=character(size))
  ret$id<-rep(xmlGetAttr(x, "id"), each=size)

  if(dfType=="data")
    colnames<-c("k", "v")
  if(dfType=="member")
    colnames<-names(xmlAttrs(xmlChildren(x)[[1]]))

  ret[colnames]<-character(size)
  for(i in 1:length(colnames))
    ret[colnames[i]]<-xmlSApply(x, xmlGetAttr, colnames[i])
  ret
}

removeKids <- function(XML, kidsname){
    ## gibt den XML nodes ohne children einer bestimmten Art zurück
  lapply(XML, function(x) removeChildren(x, kids=which(names(x)==kidsname)))
}