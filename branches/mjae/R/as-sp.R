#' Convert osmar object to sp object
#'
#' Convert an osmar object to a \link[sp]{sp} object.
#'
#' @param obj An \code{\link{osmar}} object
#'
#' @return A \link[sp]{sp} object
#'
#' @note Not yet implemented!
#'
#' @export

as_sp <- function(obj, ...) 
  UseMethod("as_sp")


sp_fun <- function(obj, subclass)
  structure(obj, class=c(subclass, "sp"))


as_sp.osmar <- function(obj, what, 
      crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")) {
  stopifnot(require("sp"))
  stopifnot(what %in% c("points","lines","polygons"))
  obj <- sp_fun(obj, what)
  ret <- as_sp(obj, crs)
  ret
}

as_sp.points <- function(obj, crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")){
    ## no possibility for multipoints-object (point-relations e.g.)
  if(check_if_full(obj)[1]==FALSE) stop("no nodes")
  dat <- obj$nodes$attrs
  coords <- data.frame(lon=dat$lon, lat=dat$lat, row.names=dat$id)
  ret<- SpatialPointsDataFrame(coords= coords, proj4string=crs, data=dat, match.ID="id")
  ret
}

as_sp.lines <- function(obj, crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")){
  fullcheck <- check_if_full(obj)
  if(fullcheck[1]==FALSE) stop("no nodes")
  if(fullcheck[2]==FALSE) stop("no ways")
  way_ids <- unique(obj$ways$refs$id)
  way_lns <- vector("list", length(way_ids))
  for(i in 1:length(way_lns))
    way_lns[[i]]<- Lines(ways_nodes2Line(way_ids[i], obj$ways, obj$nodes),
                      way_ids[i])                       
  
  if(fullcheck[3]==FALSE) return(make_SLDF(obj, way_lns, crs, "way"))
  rel_ids <- unique(obj$relations$refs$id)
  rel_lns <- vector("list", length(rel_ids))
  for(i in 1:length(rel_ids))
    rel_lns[[i]] <- Lines(rels_ways_nodes2Line(rel_ids[i], obj$relations,
                                               obj$ways, obj$nodes),
                          rel_ids[i])
  ret <- make_SLDF(obj, c(way_lns, rel_lns), crs, "relation")
  ret  
}  

rels_ways_nodes2Line <- function(relID, rels, ways, nodes){
  ref<- subset(rels$refs, id==relID)
  wayref<- subset(ref, type=="way")$ref
  wayln<-lapply(wayref, "ways_nodes2Line", ways, nodes)
#  relref<- subset(ref, type=="relation")$ref
#  falls ways der relations noch eingebaut werden sollen
  wayln <- wayln[!is.na(wayln)]
  wayln
}

ways_nodes2Line <- function(wayID, ways, nodes){
  nds <- subset(ways$refs, id==wayID)$ref
  if(length(nds)==0) return(NA)
  geo <- nodes$attrs[match(nds,nodes$attrs$id), c("lon","lat")]
  ret <- Line(geo)
  ret
}

make_SLDF <- function(obj, lns, crs, what){
  lns <- remove_emptyLines(lns)
  splns<- SpatialLines(lns, proj4string=crs)
  if(what=="way") 
    dat <- cbind(obj$ways$attrs,type=as.factor("way"))
  if(what=="relation")
    dat <- rbind(cbind(obj$ways$attrs,type=as.factor("way")),
               cbind(obj$relations$attrs,type=as.factor("relation")))
  ret <- SpatialLinesDataFrame(splns, data=dat, match.ID="id")
  ret  
}

remove_emptyLines <- function(LINES)
  LINES<- LINES[sapply(1:length(LINES), function(k) length(LINES[[k]]@Lines))!=0]  

check_if_full <- function(obj){
  ret <- as.logical(c(nrow(obj$nodes$attrs), nrow(obj$ways$attrs), 
                      nrow(obj$relations$attrs)))
  names(ret) <- c("nodes","ways", "relations")  
  ret
}

## there could be same ID for ways and relations. idea for a function
#reformat_id <- function(obj){
#  obj[["nodes"]][["attrs"]][["id"]]<- as.factor(pretext_id(obj[["nodes"]][["attrs"]][["id"]], "n_"))
#  obj[["nodes"]][["tags"]][["id"]]<- as.factor(pretext_id(obj[["nodes"]][["tags"]][["id"]], "n_")) 
#  for(j in 1:3)
#    obj[["ways"]][[j]][["id"]]<- as.factor(pretext_id(obj[["ways"]][[j]][["id"]], "w_"))
#  for(j in 1:3)
#    obj[["relations"]][[j]][["id"]]<- as.factor(pretext_id(obj[["relations"]][[j]][["id"]], "r_"))    
#  obj
#}
#pretext_id <- function(ids, what)
#  ids <- paste(what, ids, sep="")  
    
as_sp.polygons <- function(obj, crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
  cat("to become acquainted with", "\n")
