#' @include osm-descriptors.R
{}



subset_nodes <- function(x, ids) {
  #x$attrs <- subset(x$attrs, id %in% ids)  # Subet should be in order of the ids
  x$attrs <- x$attrs[match(ids, x$attrs$id, nomatch=FALSE), ]
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
  #x$refs <- x$refs[match(ids, x$refs$id), ]

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
#'     muc <- get_osm(center_bbox(11.575278, 48.137222, 200, 200))
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
