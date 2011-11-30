

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
  structure(substitute(condition), what = "attrs")
}

#' @export
tags <- function(condition) {
  structure(substitute(condition), what = "tags")
}

#' @export
refs <- function(condition) {
  structure(substitute(condition), what = "refs")
}



#' @export
find_node <- function(object, ...) {
  UseMethod("find_node")
}

#' @S3method find_node osmar
find_node.osmar <- function(object, ...) {
  find_node.nodes(object$nodes, ...)
}

#' @S3method find_node nodes
find_node.nodes <- function(object, condition) {
  stopifnot(class(condition) == "call")
  stopifnot(attr(condition, "what") %in% c("attrs", "tags"))

  what <- attr(condition, "what")
  id <- subset(object[[what]], eval(condition), select = id)$id

  if ( length(id) == 0 )
    id <- as.numeric(NA)

  id
}



#' @export
find_way <- function(object, ...) {
  UseMethod("find_way")
}

#' @S3method find_way osmar
find_way.osmar <- function(object, ...) {
  find_way.ways(object$ways, ...)
}

#' @S3method find_way ways
find_way.ways <- function(object, condition) {
  stopifnot(class(condition) == "call")
  stopifnot(attr(condition, "what") %in% c("attrs", "tags", "refs"))

  what <- attr(condition, "what")
  id <- subset(object[[what]], eval(condition), select = id)$id

  if ( length(id) == 0 )
    id <- as.numeric(NA)

  id
}



#' @export
find_relation <- function(object, ...) {
  UseMethod("find_relation")
}

#' @S3method find_relation osmar
find_relation.osmar <- function(object, ...) {
  find_relation.relations(object$relations, ...)
}

#' @S3method find_relation relations
find_relation.relations <- function(relations, condition) {
  stopifnot(class(condition) == "call")
  stopifnot(attr(condition, "what") %in% c("attrs", "tags", "refs"))

  what <- attr(condition, "what")
  id <- subset(object[[what]], eval(condition), select = id)$id

  if ( length(id) == 0 )
    id <- numeric(NA)

  id
}



### Find complete osmar object:

#' @export
find_way_complete <- function(object, ids = NULL) {
  ## TODO: check if way id is in object

  node_ids <- subset_ways(object$ways, ids)$refs$ref
  list(node_ids = node_ids, way_ids = ids, relation_ids = NULL)
}



#' @export
find_relation_complete <- function(object, ids = NULL) {
  ## TODO: check if relation id is in object

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
  x$attrs <- subset(x$attrs, id %in% ids)
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

#' S3method c osmar
c.osmar <- function(...) {
  ## TODO: object[[1]] attributes?
  objects <- list(...)

  stopifnot(all("osmar" %in% sapply(objects, class)))

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








