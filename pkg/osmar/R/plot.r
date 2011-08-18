plot.Node <- function(x, pch=20, data="no", ptcol="red", ...){
  stopifnot(class(x[[2]])[1]=="SpatialPointsDataFrame")
  stopifnot(data %in% c("no","yes","only"))
  if(data=="no"){
    pch<-pch 
    return(plot(x[[2]], pch=pch, ...))
  }
  if(data=="yes"){
    plot(x[[2]], pch=pch,...)
    nodesWithData<-unique(x[[1]]$id)
    nodes<-findID(x, nodesWithData)
    return(plot(nodes, pch=8, col=ptcol, add=TRUE))
  }
  if(data=="only"){
    nodesWithData<-unique(x[[1]]$id)
    nodes<-findID(x, nodesWithData)
    return(plot(nodes, pch=pch, col=ptcol, ...))
  }
}

plot.Way<- function(x, ...){
  stopifnot(class(x[[2]])[1]=="SpatialLinesDataFrame")
  plot(x[[2]], ...)
}

plot.Relation<- function(x,...){
  cat("No Spatial information existent", collapse="\n")
}

plot.OSM<-function(x, nodes=FALSE, ptcol="red",...){
  if(class(x$Way[[2]])=="SpatialLinesDataFrame"){
    plot(x$Way,...)
    if(nodes==TRUE)
      plot(x$Node, data="only", ptcol=ptcol, add=TRUE)
  } else {
    stopifnot(class(x$Node[[2]])[1]=="SpatialPointsDataFrame")
    plot(x$Node, data="no",...)
  }
}


#xml<-getBboxXML(bbox2coords(c(11.581262,48.150626),c(500,500)), URL=TRUE)
#uni<-getOSMObject(xml, reduced=TRUE)    ##11.03
#uni2<-getOSMObject(xml, reduced=FALSE)  ##7.53

#plot(uni)
#plot(uni, nodes=FALSE)
#plot(uni2)
#plot(uni2,nodes=FALSE)  
#op<-par(mfrow=c(2,2))
#  plot(uni$Node)
#  plot(uni$Node, data="yes")
#  plot(uni$Node, data="only")
#par(op)
#
#plot(uni$Way)
#plot(uni$Relation)
#
#findID(uni2, "1396537", full=TRUE, what="relation")
#
#getElementXML("1396537", "relation", full=TRUE)->egarten
#egart<-getOSMObject(egarten)
#findID(egart, "1396537", full=TRUE, what="relation", check=TRUE) ##passt
#