formatBbox <-
function(x, borders){
  stopifnot(class(x) %in% c("XMLInternalElementNode","XMLInternalNode","XMLAbstractNode" ))
  
  if(borders==FALSE){
    bboxnodes<-gNode(x)
    bboxnodesID<-getElementsID(bboxnodes)
    
    bboxways<-gWay(x)
    bboxwaysID<-getElementsID(bboxways)
    
    bboxrelations<-gRelation(x)
    bboxrelationsID<-getElementsID(bboxrelations)
    
    ret<-list(bboxnodes, bboxnodesID, bboxways, bboxwaysID, bboxrelations, bboxrelationsID)
    names(ret)<-c("bboxnodes", "bboxnodesID", "bboxways", "bboxwaysID", "bboxrelations", "bboxrelationsID")
    return(ret)
    
  } else{
    Bounds<- bounds(x)                                  # 0
    boundspath<-paste("//node[@lon>=\"", Bounds["minlon"], "\" and @lat>=\"", Bounds["minlat"],
                  "\" and @lon<=\"", Bounds["maxlon"] , "\" and @lat<=\"", Bounds["maxlat"] ,"\"]", sep="") 
    
    bboxnodes<-getNodeSet(x, path=boundspath)                             # 0.61
    attr(bboxnodes, "element")<-"node"
    bboxnodesID<-getElementsID(bboxnodes)                                 # 0.59
    
    bboxways<-gWay(x)                                                     # 0
    bboxwaysID<-getElementsID(bboxways)                                   # 0.12
    
    bboxnoderelations<-getNodeSet(x, path=id2path(c(bboxwaysID,bboxnodesID), operator="or"))
        ## relations, in denen bboxnodes vorkommen                        #78.31
    bboxwayrelations<-getNodeSet(x, path=id2path(bboxwaysID, operator="or"))
        ## relations, in denen bboxways vorkommen                         #16.04 
    bboxrelationsID<-unlist(c(getElementsID(bboxwayrelations), getElementsID(bboxnoderelations)))
          ## unlist falls eins leer ist                                   
    bboxrelationsID<-bboxrelationsID[!duplicated(bboxrelationsID)]
    bboxrelationrelations<-getNodeSet(x, path=id2path(bboxrelationsID, operator="or")) #0.84
    bboxrelationsID<-c(bboxrelationsID, getElementsID(bboxrelationrelations))
    bboxrelations<-c( bboxwayrelations,bboxnoderelations,bboxrelationrelations)
    bboxrelations<-bboxrelations[!duplicated(bboxrelations)]
    attr(bboxrelations, "element")<- "relation"
    
    ret<-list(bboxnodes, bboxnodesID, bboxways, bboxwaysID, bboxrelations, bboxrelationsID)
    names(ret)<-c("bboxnodes", "bboxnodesID", "bboxways", "bboxwaysID", "bboxrelations", "bboxrelationsID")
    return(ret)
  }
}

  # zeiten für 
  # x<-getBboxXML(bbox2coords(c(11.579341,48.15102),c(1500,1500)), URL=TRUE)
