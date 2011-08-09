getBboxXML <-
function(coords, URL=FALSE){
    ##  coords  = 4 Grenzen, die die BoundingBox bilden.
    ##              in der Form c(left [lon], bottom [lat], right [lon], top [lat])
    
  if( ((coords[1]-coords[3])*(coords[2]-coords[4])) >=0.25)
    return(cat("BoundingBox is bigger than 0.25-Square-Degrees", sep="\n"))
  request<-paste("http://api.openstreetmap.org/api/0.6/map?bbox=",coords[1],",",coords[2],",",coords[3],",",coords[4], sep="")
  response<-getURL(request, .encoding="UTF-8")  ##class character
                              ## Umwandlung in XML Datei->
  xml<- xmlParse(response)   ##class "XMLInternalDocument" "XMLAbstractDocument" "oldClass"
                              ## Anzeigen des Kerns
  core<- xmlRoot(xml)        ##class "XMLInternalElementNode" "XMLInternalNode"        "XMLAbstractNode"
  if(URL==TRUE) cat(paste(c("Request: \"", request, "\""), collapse=""), sep="\n")
  if(xmlName(core)=="osm")
    return(core)
  if(xmlName(core)=="html"){
    cat(xmlValue(getNodeSet(core, path="head")[[1]]),
        xmlValue(getNodeSet(core, path="body")[[1]]), sep="\n")
  }
}

#coords<-bbox2coords(c(11.579341,48.15102),c(5000,5000))  ### too many nodes
#coords<-bbox2coords(c(11.579341,48.15102),c(50,50))
#