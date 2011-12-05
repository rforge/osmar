#' @include osm-descriptors.R
{}



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



#' Find element for a given condition
#'
#' @details
#'   The basis of an \code{\link{osmar}} object are
#'   \code{data.frame}s; therefore the \code{condition} principally
#'   follows the rules for \code{\link[base]{subset}}: logical
#'   expression indicating elements or rows to keep.
#'
#'   Furthermore, one has to define on which element and which data
#'   of the \code{\link{osmar}} object the condition applies:
#'   \code{element(data(condition))}, see \code{\link{osm_descriptors}}.
#'
#' @examples
#'   \dontrun{
#'     muc <- get_osm(center_bbox(11.575278, 48.137222, 200, 200)
#'     find(muc, node(tags(v == "Marienplatz")))
#'     find(muc, node(attrs(id == 19475890)))
#'     find(muc, way(tags(k == "highway" & v == "pedestrian")))
#'   }
#'
#' @param object An \code{\link{osmar}} object
#' @param condition A condition for the element to find; see details
#'   section.
#'
#' @return The ID of the the element
#'
#' @family finding
#'
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



### Find all elements:

#' Find all elements related to an ID
#'
#' For a given ID these functions return all IDs of related elements.
#'
#' @details
#'   \code{find_down} finds all elements downwards the hierarchy:
#'
#'   \tabular{rrr}{
#'
#'     node     \tab -> \tab node\cr
#'
#'     way      \tab -> \tab way + node\cr
#'
#'     relation \tab -> \tab relation + way + node\cr
#'
#'   }
#'
#' @param object An \code{\link{osmar}} object
#' @param ids A vector of IDs tagged whether they are \code{node},
#'   \code{way}, or \code{relation}
#'
#' @return A list with the three elements \code{node_ids},
#'   \code{way_ids}, \code{relation_ids}
#'
#' @examples
#'   \dontrun{
#'     muc <- get_osm(center_bbox(11.575278, 48.137222, 200, 200)
#'     o1 <- find(muc, way(tags(k == "highway" & v == "pedestrian")))
#'
#'     find_down(muc, way(o1))
#'     find_up(muc, way(o1))
#'   }
#'
#' @family finding
#'
#' @export
find_down <- function(object, ids) {
  stopifnot(is_osmar(object))

  handler <- sprintf("find_down_%s", attr(ids, "element"))
  do.call(handler, list(object, as.vector(ids)))
}



find_down_node <- function(object, ids = NULL) {
  stopifnot(is_osmar(object))
  list(node_ids = ids, way_ids = NULL, relation_ids = NULL)
}



find_down_way <- function(object, ids = NULL) {
  ## TODO: check if way id is in object
  stopifnot(is_osmar(object))

  node_ids <- subset_ways(object$ways, ids)$refs$ref
  list(node_ids = node_ids, way_ids = ids, relation_ids = NULL)
}



find_down_relation <- function(object, ids = NULL) {
  ## TODO: check if relation id is in object
  stopifnot(is_osmar(object))

  refs <- subset_relations(object$relations, ids)$refs

  way_ids <- subset(refs, type == "way")$ref
  node_ids <- subset(refs, type == "node")$ref

  ret <- find_down_way(object, way_ids)
  ret$node_ids <- c(ret$node_ids, node_ids)
  ret$relation_ids <- ids

  ret
}



#' @details
#'   \code{find_up} finds all elements upwards the hierarchy:
#'
#'   \tabular{rrr}{
#'
#'     node     \tab -> \tab node + way + relation\cr
#'
#'     way      \tab -> \tab way + relation\cr
#'
#'     relation \tab -> \tab relation\cr
#'
#'   }
#'
#' @rdname find_down
#'
#' @export
find_up <- function(object, ids) {
  stopifnot(is_osmar(object))

  handler <- sprintf("find_up_%s", attr(ids, "element"))
  do.call(handler, list(object, as.vector(ids)))
}



find_up_node <- function(object, ids = NULL) {
  way_ids <- subset(object$ways$refs, ref %in% ids)$id
  rel_ids <- subset(object$relations$refs, type == "node" & ref %in% ids)$id

  list(node_ids = ids, way_ids = way_ids, relation_ids = rel_ids)
}



find_up_way <- function(object, ids = NULL) {
  rel_ids <- subset(object$relations$refs, type == "way" & ref %in% ids)$id
  list(node_ids = NULL, way_ids = ids, relation_ids = rel_ids)
}



find_up_relation <- function(object, ids = NULL) {
  stopifnot(is_osmar(object))
  list(node_ids = NULL, way_ids = NULL, relation_ids = ids)
}



### Find nearest node with given conditions:

#' Find nearest node with given conditions
#'
#' For a given ID, find nearest node (geographical distance) with
#' given conditions.
#'
#' @param object An \code{\link{osmar}} object
#' @param id An node ID
#' @param condition Condition for the element to find; see
#'   \code{\link{find}}
#'
#' @return A node ID or \code{NA}
#'
#' @examples
#'   \dontrun{
#'     muc <- get_osm(center_bbox(11.575278, 48.137222, 200, 200)
#'     id <- find(muc, node(tags(v == "Marienplatz")))[1]
#'
#'     find_nearest_node(muc, id, way(tags(k == "highway" & v == "pedestrian")))
#'   }
#'
#' @family finding
#'
#' @importFrom geosphere distm
#' @export
find_nearest_node <- function(object, id, condition) {
  stopifnot(is_osmar(object))

  node <- subset_nodes(object$nodes, id)

  element <- attr(condition, "element")

  cand_ids <- find(object, condition)
  cand_ids <- do.call(element, list(cand_ids))
  cand_ids <- find_down(object, cand_ids)
  cand_nodes <- subset_nodes(object$nodes, cand_ids$node_ids)

  dist <- distm(node$attrs[, c("lon", "lat")],
                cand_nodes$attrs[, c("lon", "lat")])

  cand_nodes$attrs[which.min(dist), "id"]
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



#' Subset an osmar object
#'
#' @param x An \code{\link{osmar}} object
#' @param node_ids Node ID vector
#' @param way_ids Way ID vector
#' @param relation_ids Relation ID vector
#' @param ids A list composed of \code{node_ids}, \code{way_ids},
#'   \code{relation_ids}; for easier usage with results from
#'   \code{\link{find_up}} and \code{\link{find_down}}
#' @param ... Ignored
#'
#' @return An \code{\link{osmar}} object containing the specified
#'   elements
#'
#' @examples
#'   \dontrun{
#'     muc <- get_osm(center_bbox(11.575278, 48.137222, 200, 200)
#'     id <- find(muc, node(tags(v == "Marienplatz")))
#'
#'     subset(muc, node_ids = id)
#'
#'     subset(muc, ids = find_up(muc, node(id)))
#'   }
#'
#' @method subset osmar
#'
#' @S3method subset osmar
subset.osmar <- function(x, node_ids = NULL, way_ids = NULL, relation_ids = NULL,
                         ids = list(node_ids = node_ids, way_ids = way_ids,
                                    relation_ids = relation_ids), ...) {

  x$nodes <- subset_nodes(x$nodes, ids$node_ids)
  x$ways <- subset_ways(x$ways, ids$way_ids)
  x$relations <- subset_relations(x$relations, ids$relation_ids)

  x
}



### Combining methods: ###############################################

#' Combine osmar objects
#'
#' Combine two or more \code{\link{osmar}} objects.
#'
#' @param ... \code{\link{osmar}} objects to be concatenated
#'
#' @return An \code{\link{osmar}} object based on the provided objects
#'
#' @examples
#'   \dontrun{
#'     muc <- get_osm(center_bbox(11.575278, 48.137222, 200, 200)
#'     o1 <- subset(muc, node_ids = find(muc, node(tags(v == "Marienplatz"))))
#'     o2 <- subset(muc, ids = find_down(muc, way(c(96619179, 105071000))))
#'
#'     o1
#'     o2
#'     c(o1, o2)
#'   }
#'
#' @method c osmar
#'
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

merge_ways_nodes <- function(ways, nodes) {
  colnames(ways) <- sprintf("w%s", colnames(ways))
  colnames(nodes) <- sprintf("n%s", colnames(nodes))

  m <- match(ways$wref, nodes$nid)

  dat <- cbind(ways, nodes[m, ])
  dat <- na.omit(dat)

  dat$nid <- NULL
  colnames(dat) <- substring(colnames(dat), 2)

  dat
}



#' Plot osmar object
#'
#' Simple plotting functions to visualize \code{\link{osmar}}
#' objects. Note that for more complex plots, we suggest to convert
#' the objects into \code{sp} and use their plotting functionality.
#'
#' @param x An \code{\link{osmar}} object
#' @param way_args A list of parameters for plotting ways
#' @param node_args A list of parameters for plotting nodes
#' @param ... Ignored
#'
#' @method plot osmar
#'
#' @S3method plot osmar
plot.osmar <- function(x,
                       way_args = list(col = gray(0.7)),
                       node_args = list(pch = 19, cex = 0.1, col = gray(0.3)), ...) {

  do.call(plot_ways, c(list(x), way_args))
  do.call(plot_nodes, c(list(x, add = TRUE), node_args))
}



#' @param add New plot device or plot on existing onde
#' @rdname plot.osmar
#'
#' @export
plot_nodes <- function(x, add = FALSE, ...) {
  stopifnot(is_osmar(x))

  coords <- x$nodes[[1]][, c("lat", "lon")]

  if ( add )
    points(coords, ...)
  else
    plot(coords, ...)
}



#' @param xlab A x-axis label
#' @param ylab A y-axis label
#' @rdname plot.osmar
#'
#' @export
plot_ways <- function(x, add = FALSE, xlab = "lat", ylab = "lon", ...) {
  stopifnot(is_osmar(x))

  dat <- merge_ways_nodes(x$ways[[3]], x$nodes[[1]])

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

