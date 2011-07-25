merge.wayDataFrame<-merge.relationDataFrame<-merge.nodeDataFrame <-
merge.wayMember<- merge.relationMember <-
merge.relationMeta <-function(x,y,...){
  z<-list(x,y,...)
  ret<-do.call("rbind", z)
  ret<-unique(ret)
  return(ret)
}

merge.SpatialPointsDataFrame<- function(x,y,...){
  z<-list(x,y,...)
  nodesmeta<-do.call("rbind", lapply(z, function(k) k@data))
  nodesmeta<-nodesmeta[!duplicated(nodesmeta$id),]

  factorCols<-names(nodesmeta)[sapply(nodesmeta, is.factor)]
  nodesmeta[,factorCols] <- droplevels(nodesmeta[,factorCols])
  nodescoords<-cbind(nodesmeta$lon, nodesmeta$lat)
  nodescoords<-data.frame(lon=nodesmeta$lon, lat=nodesmeta$lat, row.names=nodesmeta$id)
  nodeSP<- SpatialPointsDataFrame(coords=nodescoords, proj4string=z[[1]]@proj4string,
                                      data=as.data.frame(nodesmeta), match.ID=TRUE)
  return(nodeSP)
}

merge.SpatialLinesDataFrame <- function(x,y,...){
  z<-list(x,y,...)
  merglines<-unlist(lapply(z, function(k) k@lines))
  merglines<-merglines[!duplicated(sapply(merglines, function(k) k@ID))]
  mergdata<-do.call("rbind",lapply(z, function(k) k@data))
  ret<-SpatialLinesDataFrame(SpatialLines(merglines, proj4string=z[[1]]@proj4string),
                                      data=mergdata, match.ID=TRUE)
  return(ret)
}

merge.Node <- function(x,y,...){
  z<-list(x,y,...)
  nodeData<-do.call("merge", lapply(z, function(k) k[[1]]))
  nodesSP<-do.call("merge", lapply(z, function(k) k[[2]]))
  ret<-Node(nodeData, nodesSP)
  ret
}

merge.Way <- function(x,y,...){
  z<-list(x,y,...)
  wayData<-do.call("merge", lapply(z, function(k) k[[1]]))
  waySP<-do.call("merge", lapply(z, function(k) k[[2]]))
  wayMem<-do.call("merge", lapply(z, function(k) k[[3]]))
  ret<-Way(wayData, waySP, wayMem)
  ret
}

merge.Relation<- function(x,y,...){
  z<-list(x,y,...)
  relationMeta<-do.call("merge", lapply(z, function(k) k[[1]]))
  relationData<-do.call("merge", lapply(z, function(k) k[[2]]))
  relationMem<-do.call("merge", lapply(z, function(k) k[[3]]))
  ret<-Relation(relationMeta, relationData, relationMem)
  ret
}

merge.OSM<- function(x,y,...){
  z<-list(x,y,...)
  node<-do.call("merge", lapply(z, function(k) k$Node))
  way<-do.call("merge", lapply(z, function(k) k$Way))
  relation<-do.call("merge", lapply(z, function(k) k$Relation))
  return(OSM(node,way,relation))
}

##mergen
#xmlhed<-getBboxXML(bbox2coords(c(11.543387,48.164253),c(1500,1500)), URL=TRUE)
#xmlnord<-getBboxXML(bbox2coords(c(11.563404,48.160733),c(1500,1500)), URL=TRUE)
#xmlleo<-getBboxXML(bbox2coords(c(11.549284,48.15787),c(1500,1500)), URL=TRUE)
#hed<-getOSMObject(xmlhed, reduced=FALSE)
#nord<-getOSMObject(xmlnord, reduced=FALSE)
#leo<-getOSMObject(xmlleo, reduced=FALSE)
#
#hednd<-hed$Node
#nordnd<-nord$Node
#leond<-leo$Node
#hedway<-hed$Way
#nordway<-nord$Way
#leoway<-leo$Way
#hedrel<-hed$Relation
#nordrel<-nord$Relation
#leorel<-leo$Relation
#
#merge(hed,leo,nord)->muc
#plot(muc)
#  six<-strptime("2006.01.01", format="%Y.%m.%d")
#  ten<-strptime("2011.01.01", format="%Y.%m.%d")
#  elev<-strptime("2011.07.10", format="%Y.%m.%d")
#  years<-c(seq(from=six, to=ten, length=6),as.POSIXct(elev))
#  year<-vector("list",6)
#  for(i in 1:6)
#    year[[i]]<-findTime(muc, time=years[i], time2=years[i+1], what="between")
#  plot(muc, col="grey")
#  for(i in 6:1)
#    plot(year[[i]]$Node, add=TRUE, col=i, cex=0.7)
#  legend(legend=years[1:6], x="topright", fill=1:6, cex=1)
#
#