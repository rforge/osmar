removeKids <-
function(XML, kidsname){
    ## gibt den XML nodes ohne children einer bestimmten Art zur�ck
  ret<- lapply(XML, function(x) removeChildren(x, kids=which(names(x)==kidsname)))
  ret
}

