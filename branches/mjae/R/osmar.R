

#' @S3method print osmar
print.osmar <- function(x, ...) {
  elem_obs <- sapply(x, function(y) nrow(y[[1]]))

  cat("osmar object\n")
  cat(paste(elem_obs, " ", names(elem_obs), sep = "", collapse = ", "), "\n")

  invisible(x)
}



#' @S3method summary osmar
summary.osmar <- function(object, ...) {
  warning("Not yet implemented!")
}



### Finding methods: #################################################

#' @export
attrs <- function(condition) {
  structure(list(condition = substitute(condition)), what = "attrs")
}

#' @export
tags <- function(condition) {
  structure(list(condition = substitute(condition)), what = "tags")
}

#' @export
refs <- function(condition) {
  structure(list(condition = substitute(condition)), what = "refs")
}


#' @S3method node list
node.list <- function(object) {
  structure(object, element = "node")
}

#' @S3method way list
way.list <- function(object) {
  structure(object, element = "way")
}

#' @S3method relation list
relation.list <- function(object) {
  structure(object, element = "relation")
}


#' @export
find <- function(object, condition) {
  stopifnot(is_osmar(object))
  stopifnot(attr(condition, "element") %in% c("node", "way", "relation"))

  handler <- sprintf("find_%s", attr(condition, "element"))
  do.call(handler, list(object, condition))
}



find_node <- function(object, ...) {
  UseMethod("find_node")
}

find_node.osmar <- function(object, ...) {
  find_node.nodes(object$nodes, ...)
}

find_node.nodes <- function(object, condition) {
  #stopifnot(class(condition) == "call")
  stopifnot(attr(condition, "what") %in% c("attrs", "tags"))

  what <- attr(condition, "what")
  id <- subset(object[[what]], eval(condition$condition), select = id)$id

  if ( length(id) == 0 )
    id <- as.numeric(NA)

  id
}



find_way <- function(object, ...) {
  UseMethod("find_way")
}

find_way.osmar <- function(object, ...) {
  find_way.ways(object$ways, ...)
}

find_way.ways <- function(object, condition) {
  stopifnot(attr(condition, "what") %in% c("attrs", "tags", "refs"))

  what <- attr(condition, "what")
  id <- subset(object[[what]], eval(condition$condition), select = id)$id

  if ( length(id) == 0 )
    id <- as.numeric(NA)

  id
}



find_relation <- function(object, ...) {
  UseMethod("find_relation")
}

find_relation.osmar <- function(object, ...) {
  find_relation.relations(object$relations, ...)
}

find_relation.relations <- function(relations, condition) {
  stopifnot(attr(condition, "what") %in% c("attrs", "tags", "refs"))

  what <- attr(condition, "what")
  id <- subset(object[[what]], eval(condition$condition), select = id)$id

  if ( length(id) == 0 )
    id <- numeric(NA)

  id
}



### Find nearest node with given conditions:

#' @export
find_node_nearest <- function(object, id, condition) {
  stopifnot(is_osmar(object))

  node <- subset_nodes(object$nodes, id)

  element <- attr(condition, "element")

  cand_ids <- find(object, condition)
  cand_ids <- do.call(element, list(cand_ids))
  cand_ids <- find_complete(object, cand_ids)
  cand_nodes <- subset_nodes(object$nodes, cand_ids$node_ids)

  dist <- distm(node$attrs[, c("lon", "lat")],
                cand_nodes$attrs[, c("lon", "lat")])

  cand_nodes$attrs[which.min(dist), "id"]
}



### Find complete osmar object:

#' @export
find_complete <- function(object, ids) {
  stopifnot(is_osmar(object))

  handler <- sprintf("find_%s_complete", attr(ids, "element"))
  do.call(handler, list(object, as.vector(ids)))
}



find_node_complete <- function(object, ids = NULL) {
  stopifnot(is_osmar(object))
  list(node_ids = ids, way_ids = NULL, relation_ids = NULL)
}



find_way_complete <- function(object, ids = NULL) {
  ## TODO: check if way id is in object
  stopifnot(is_osmar(object))

  node_ids <- subset_ways(object$ways, ids)$refs$ref
  list(node_ids = node_ids, way_ids = ids, relation_ids = NULL)
}



find_relation_complete <- function(object, ids = NULL) {
  ## TODO: check if relation id is in object
  stopifnot(is_osmar(object))

  refs <- subset_relations(object$relations, ids)$refs

  way_ids <- subset(refs, type == "way")$ref
  node_ids <- subset(refs, type == "node")$ref

  ret <- find_way_complete(object, way_ids)
  ret$node_ids <- c(ret$node_ids, node_ids)
  ret$relation_ids <- ids

  ret
}




### Subsetting methods: ##############################################

subset_nodes <- function(x, ids) {
  #x$attrs <- subset(x$attrs, id %in% ids)     # Subet should be in order of the ids
  x$attrs <- x$attrs[match(ids, x$attrs$id), ]
  x$attrs$user <- x$attrs$user[, drop = TRUE]

  x$tags <- subset(x$tags, id %in% ids)
  x$tags$k <- droplevels(x$tags$k)
  x$tags$v <- droplevels(x$tags$v)

  x
}

subset_ways <- function(x, ids) {
  x$attrs <- subset(x$attrs, id %in% ids)
  x$attrs$user <- x$attrs$user[, drop = TRUE]

  x$tags <- subset(x$tags, id %in% ids)
  x$tags$k <- droplevels(x$tags$k)
  x$tags$v <- droplevels(x$tags$v)

  x$refs <- subset(x$refs, id %in% ids)

  x
}

subset_relations <- function(x, ids) {
  x$attrs <- subset(x$attrs, id %in% ids)
  x$attrs$user <- x$attrs$user[, drop = TRUE]

  x$tags <- subset(x$tags, id %in% ids)
  x$tags$k <- droplevels(x$tags$k)
  x$tags$v <- droplevels(x$tags$v)

  x$refs <- subset(x$refs, id %in% ids)
  x$refs$type <- droplevels(x$refs$type)
  x$refs$role <- droplevels(x$refs$role)

  x
}



#' @S3method subset osmar
subset.osmar <- function(x, node_ids = NULL, way_ids = NULL, relation_ids = NULL,
                         ids = list(node_ids = node_ids, way_ids = way_ids,
                                    relation_ids = relation_ids)) {

  x$nodes <- subset_nodes(x$nodes, ids$node_ids)
  x$ways <- subset_ways(x$ways, ids$way_ids)
  x$relations <- subset_relations(x$relations, ids$relation_ids)

  x
}



### Combining methods: ###############################################

#' @S3method c osmar
c.osmar <- function(...) {
  ## TODO: object[[1]] attributes?
  objects <- list(...)

  stopifnot(are_osmar(objects))

  c_parts <- function(w1, w2) {
    do.call(rbind, lapply(objects, "[[", c(w1, w2)))
  }

  objects[[1]]$nodes$attrs <- c_parts("nodes", "attrs")
  objects[[1]]$nodes$tags <- c_parts("nodes", "tags")

  objects[[1]]$ways$attrs <- c_parts("ways", "attrs")
  objects[[1]]$ways$tags <- c_parts("ways", "tags")
  objects[[1]]$ways$refs <- c_parts("ways", "refs")

  objects[[1]]$relations$attrs <- c_parts("relations", "attrs")
  objects[[1]]$relations$tags <- c_parts("relations", "tags")
  objects[[1]]$relations$refs <- c_parts("relations", "refs")

  objects[[1]]
}



### Plotting methods: ################################################

#' @export
plot_nodes <- function(object, add = FALSE, ...) {
  stopifnot(is_osmar(object))

  coords <- object$nodes[[1]][, c("lat", "lon")]

  if ( add )
    points(coords, ...)
  else
    plot(coords, ...)
}



#' @export
plot_ways <- function(object, add = FALSE, xlab = "lat", ylab = "lon", ...) {
  stopifnot(is_osmar(object))

  dat <- merge(object$ways[[3]], object$nodes[[1]],
                by.x = "ref", by.y = "id", sort = FALSE, all.x = TRUE)

  rlat <- range(dat$lat, na.rm = TRUE)
  rlon <- range(dat$lon, na.rm = TRUE)

  dat <- split(dat, dat$id)

  if ( !add ) {
    plot(1, type = "n", xlim = rlat, ylim = rlon,
         xlab = xlab, ylab = ylab)
  }

  for ( coord in dat ) {
    if ( nrow(coord) >= 2 ) {
      lines(coord[, c("lat", "lon")], ...)
    }
  }
}



#' @S3method plot osmar
plot.osmar <- function(object,
                       way_args = list(col = gray(0.7)),
                       node_args = list(pch = 19, cex = 0.1, col = gray(0.3)), ...) {

  do.call(plot_ways, c(list(object), way_args))
  do.call(plot_nodes, c(list(object, add = TRUE), node_args))
}


