getOSMObject <-
function(XML, reduced=FALSE, crs=CRS("+init=epsg:4326")){
    ##  reduced = bei TRUE,Reduzierung nur auf Daten, die in der bbox sind. bei FALSE konvertierung der ganzen XML in OSM-Objekt
    ##  crs     = Angabe eines anderern CoordinateReferenceSystems
 
  if(reduced==TRUE){
    bbox<-formatBbox(XML, borders=TRUE)        # 68.03
  } else{
    bbox<-formatBbox(XML, borders=FALSE)       # 0.89
  }
    ## Liste mit 6 Elementen bboxnodes(+ID), bboxways(+ID), bboxrelations(+ID) 
  nodes_1        <-  getXMLData(bbox[["bboxnodes"]])                     #  2.48
  nodes_2        <-  node2SPDF(bbox[["bboxnodes"]], crs)                 #  1.13
  ways_1         <-  getXMLData(bbox[["bboxways"]])                      # 11.73
  if(class(nodes_2)=="SpatialPointsDataFrame"){
    ways_2       <-  way2SLDF(bbox[["bboxways"]], nodes_2@data, crs)     # 13.48
    }else{ ways_2  <- getXMLMeta(bbox[["bboxways"]])}                    #  0.19  
  ways_3         <-  getXMLMember(bbox[["bboxways"]])                    # 10.32
  relations_1    <-  getXMLMeta(bbox[["bboxrelations"]])                 #  0
  relations_2    <-  getXMLData(bbox[["bboxrelations"]])                 #  1.45
  relations_3    <-  getXMLMember(bbox[["bboxrelations"]])               #  2.61
 
  ret<- OSM(Node(nodes_1, nodes_2),
            Way(ways_1, ways_2, ways_3),
            Relation(relations_1, relations_2, relations_3))
  ret
}

  # zeiten für 
  # xml2<-getBboxXML(bbox2coords(c(11.579341,48.15102),c(1500,1500)), URL=TRUE)