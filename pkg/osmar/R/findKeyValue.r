findKeyValue <- function(x, key="", value="", found=TRUE){
  stopifnot(class(x)[[1]]=="OSM")
  stopifnot(sum(nchar(c(key,value)))!=0)
  osmDat<-rbind(x$Node[[1]],x$Way[[1]],x$Relation[[2]])
  if(nchar(key)!=0 & nchar(value)==0){
    tempIDs<-subset(osmDat, k==key)$id
    if(found==TRUE)
      cat(paste(c("found IDs", length(tempIDs)), collapse=": "), sep="\n")
    return(findID(x, tempIDs))
  }
  if(nchar(key)==0 & nchar(value)!=0){
    tempIDs<-subset(osmDat, v==value)$id
    if(found==TRUE)
      cat(paste(c("found IDs", length(tempIDs)), collapse=": "), sep="\n")
    return(findID(x, tempIDs))
  }
  if(nchar(key)!=0 & nchar(value)!=0){
    tempIDs<-subset(osmDat, k==key & v==value)$id
    if(found==TRUE)
      cat(paste(c("found IDs", length(tempIDs)), collapse=": "), sep="\n")
    return(findID(x, tempIDs))
  }   
}

#xml<-getBboxXML(bbox2coords(c(11.581262,48.150626),c(500,500)), URL=TRUE)
#uni<-getOSMObject(xml, reduced=FALSE)  #7.53
#findKeyValue(uni, value="N40")->N40
#findKeyValue(uni, value="traffic_signals")->ampeln
#findKeyValue(uni, key="building")->buildings
#findKeyValue(uni, value="yes")->ja
