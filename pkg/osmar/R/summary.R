summary.Node<- function(object,...){
  x<-object
  ret<-list(noNode=numeric())
  ret$noNode<-c(total=numeric(1), withData=numeric(1))
  
  if(is.character(x[[2]])==FALSE){  
    ret$noNode[1]   <-summary(x[[2]])$npoints
    ret$nodesBbox   <-summary(x[[2]])$bbox
    ret$nodeTimes    <-c(oldest=min(x[[2]]$timestamp), newest=max(x[[2]]$timestamp))
    ret$topNodeUser <-summary(x[[2]]$user)[order(summary(x[[2]]$user), decreasing=TRUE)]
  } else {ret$noNode[1]<-0}
  if(is.character(x[[1]])==FALSE){
    ret$noNode[2]       <- length(x[[1]]$id[!duplicated(x[[1]]$id)])
    ret$nodeAmenity     <- extractValues("amenity",x[[1]])
    ret$nodeBuilding    <- extractValues("building",x[[1]])
    ret$nodeRailway     <- extractValues("railway", x[[1]])    
    ret$nodePlace       <- extractValues("place", x[[1]])
    ret$nodeAllValue    <-extractValuesAll(x[[1]], c("railway", "amenity", "place", "building"))
  } else {ret$noNode[2] <-0}
  class(ret)<-"summary.Node"
  ret
}

summary.Way<- function(object,...){
  x<-object
  ret<-list(noWay=numeric())
  ret$noWay<-c(total=numeric(1), withData=numeric(1))
  
  if(is.character(x[[2]])==FALSE){
    ret$noWay[1]      <-nrow(x[[2]])
    if(class(x[[2]])[1]=="SpatialLinesDataFrame")
      ret$waysBbox   <-summary(x[[2]])$bbox
    ret$waysTimes    <-c(oldest=min(x[[2]]$timestamp), newest=max(x[[2]]$timestamp))
    ret$topWayUser <-summary(x[[2]]$user)[order(summary(x[[2]]$user), decreasing=TRUE)]
  } else {ret$noNode[1]<-0}
  if(is.character(x[[1]])==FALSE){
    ret$noWay[2] <- length(unique(x[[1]]$id))
    ret$Streets     <-list(highway=extractValues("highway", x[[1]]),
                            railway  =extractValues("railway", x[[1]]),
                            tunnel   =extractValues("tunnel", x[[1]]),
                            bridge   =extractValues("bridge", x[[1]]))
    ret$wayNature   <-list(natural=extractValues("natural", x[[1]]),
                            landuse=extractValues("landuse", x[[1]]),
                            waterway=extractValues("waterway", x[[1]]))
    ret$wayBuilding <-extractValues("building",x[[1]])
    ret$wayAmenity  <-extractValues("amenity", x[[1]])

    ret$wayAllValue <-extractValuesAll(x[[1]],
                                        c("highway","railway","tunnel","bridge",
                                          "natural","landuse","waterway","building",
                                          "amenity"))
  } else {ret$noWay[2] <-0}
  class(ret)<-"summary.Way"
  ret
}

summary.Relation<- function(object,...){
  x<-object
  ret<-list(noRelation=numeric())
  ret$noRelation<-c(total=numeric(1), withData=numeric(1))
  
  if(is.character(x[[1]])==FALSE){
    ret$noRelation[1]   <-nrow(x[[1]])
    ret$RelationTimes   <-c(oldest=min(x[[1]]$timestamp), newest=max(x[[1]]$timestamp))
    ret$topRelationUser <-summary(x[[1]]$user)[order(summary(x[[1]]$user), decreasing=TRUE)]
  }else{ret$noRelation[1]<-0}
  
  if(is.character(x[[2]])==FALSE){
    ret$noRelation[2]     <- length(unique(x[[2]]$id))
    ret$relationType        <- extractValues("type", x[[2]])
    if("route" %in% names(ret$relationType))
      ret$relationRoute     <- extractValues("route", x[[2]])
    ret$relationBoundary    <- extractValues("boundary", x[[2]])

    ret$relationAllValue    <- extractValuesAll(x[[2]], c("type", "route", "boundary")) 
  }else{ret$noRelation[2]<-0}
  class(ret)<-"summary.Relation"
  ret
}

summary.OSM <- function(object,...){
  x<-object
  ret<-list(nodeSummary=list(), waySummary=list(), relationSummary=list())

  ret$nodeSummary<- summary(x$Node)
  ret$waySummary<- summary(x$Way)
  ret$relationSummary<-summary(x$Relation)
  class(ret)<-"summary.OSM"
  ret
}

#xml2<-getBboxXML(bbox2coords(c(11.580809,48.15086),c(2000,2000)), URL=TRUE)
#StatRed2<-getOSMObject(xml2, reduced=FALSE)   ##76.33
#y<-StatRed2
#
#summary(y$Node)
#summary(y$Way)
#summary(y$Relation)
#summary(y)
##
#