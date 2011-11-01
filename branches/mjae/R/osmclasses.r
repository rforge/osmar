osmar_classes<- function(obj, element){
  stopifnot(all(sapply(obj, class)=="data.frame"))
  structure(obj, class = c(element, "OSM"))
}

OSM<- function(obj){
  stopifnot(length(obj)==3)
  stopifnot(sapply(obj, function(k) class(k)[1])==c("NODE", "WAY", "RELATION"))
  structure(list(NODE=obj[[1]], WAY=obj[[2]], RELATION=obj[[3]]), class="OSM")
}

