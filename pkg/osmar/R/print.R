print.OSM<- function(x,...){
  str(x, max.level=1)
}

print.Node<-function(x,...){
  str(x, max.level=1)
}

print.Way<-function(x,...){
  str(x, max.level=1)
}

print.Relation<-function(x,...){
  str(x, max.level=1)
}

print.summary.Node<- function(x,...){
  x<-x[!names(x) %in% c("nodeAllValue", "topNodeUser")]
  print(x)
}

print.summary.Way<- function(x,...){
  x<-x[!names(x) %in% c("wayAllValue", "topWayUser")]
  print(x)
}

print.summary.Relation<- function(x,...){
  x<-x[!names(x) %in% c("relationAllValue", "topRelationUser")]
  print(x)
}