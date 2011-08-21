findTime<- function(x, time="", what="", time2=""){
  stopifnot(class(x)[1]=="OSM")
  stopifnot(what %in% c("older", "newer", "between"))
  stopifnot(class(time)[1] %in% c("POSIXlt", "POSIXct"))
  
  if(what=="older"){
    nodeIDs     <- subset(x$Node[[2]]@data, timestamp < time)$id
    wayIDs      <- subset(x$Way[[2]]@data, timestamp < time)$id
    relationIDs <- subset(x$Relation[[1]], timestamp < time)$id
    return(findID(x, c(nodeIDs, wayIDs, relationIDs)))
  }
  if(what=="newer"){
    nodeIDs     <- subset(x$Node[[2]]@data, timestamp > time)$id
    wayIDs      <- subset(x$Way[[2]]@data, timestamp > time)$id
    relationIDs <- subset(x$Relation[[1]], timestamp > time)$id
    return(findID(x, c(nodeIDs, wayIDs, relationIDs)))
  }
  if(what=="between"){
    stopifnot(class(time2)[1] %in% c("POSIXlt", "POSIXct"))
    nodeIDs     <- subset(x$Node[[2]]@data, timestamp>=time & timestamp<time2)$id
    wayIDs      <- subset(x$Way[[2]]@data, timestamp>=time & timestamp<time2)$id
    relationIDs <- subset(x$Relation[[1]], timestamp>=time & timestamp<time2)$id
    return(findID(x, c(nodeIDs, wayIDs, relationIDs)))
  }
}

#xml2<-getBboxXML(bbox2coords(c(11.579341,48.15102),c(500,500)), URL=TRUE)
#StatRed2<-getOSMObject(xml2, reduced=TRUE)   ##9.59
#y<-StatRed2
#thisyear<-strptime("2011.01.01", format="%Y.%m.%d")
#oneone<-findTime(x, time=thisyear, "older")
#plot(x, nodes=FALSE)
#plot(oneone$Node, col=2, add=TRUE)
#plot(oneone$Way,col=2, add=TRUE)
#
#june1<-strptime("2011.06.01", format="%Y.%m.%d")
#june2<-strptime("2011.06.30", format="%Y.%m.%d")
#juneosm<-findTime(y, time=june1, "between", time2=june2)
#plot(y, nodes=FALSE)
#plot(juneosm$Node, col=2, add=TRUE)
#plot(juneosm$Way,col=2, add=TRUE)

