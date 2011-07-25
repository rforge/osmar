getElementXML <-
function(ID, element="", full=FALSE){
    ##  rückgabe des XML-Files eines Elements.
    ##  bei mehreren IDs Rückgabe dieser IDs
    #####
    ## ID       = ID des elements
    ## element  = welches Element soll ausgegeben werden. "node"|"way"|"relation"
    ## full     = nur bei "way"|"relation" UND einer ID möglich
    ##          = -way: Ausgabe von jedem zugehörigen Node
    ##          = -relation: Ausgabe von jedem Node, Ways(inklusive Nodes) & Relations(exklusive ways/nodes)
  stopifnot(element %in% c("node", "way", "relation"))
  if(length(ID)==1){
      ## Test ob eine odere mehrere IDs
    request<- paste("http://api.openstreetmap.org/api/0.6", element, ID, sep="/")
    if(element!="node" & full==TRUE)
      request<-paste(request,"full", sep="/")
  } else{
    elements<-paste(element,"s?", element,"s=",sep="")
    request<- paste("http://api.openstreetmap.org/api/0.6/", elements, paste(ID, collapse=",") , sep="")
  }
  print(request)
  response<-getURL(request)  ##class character
                              ## Umwandlung in XML Datei->
  if(nchar(response) %in% c(0,1)) return(warning("empty response"))
  xml<- xmlParse(response)   ##class "XMLInternalDocument" "XMLAbstractDocument" "oldClass"
                              ## Anzeigen des Kerns
  core<- xmlRoot(xml)        ##class "XMLInternalElementNode" "XMLInternalNode"        "XMLAbstractNode"
  core
}

#getElementXML("22", "relation")