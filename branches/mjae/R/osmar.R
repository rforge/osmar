

#' @S3method print osmar
print.osmar <- function(x, ...) {
  id <- attr(x, "identifier")
  what <- class(id)[1]

  cat("osmar object by ", what, ":\n", sep = "")
  cat(paste(names(id), "=", round(id, 2), collapse = ", "), "\n")

  invisible(x)
}



#' @S3method summary osmar
summary.osmar <- function(object, ...) {
  ## TODO: change to print.summary.osmar

  elem_obs <- lapply(object, function(x) nrow(x[[1]]))
  elem_obs <- as.data.frame(elem_obs)
  rownames(elem_obs) <- "Elements"

  print(object)
  cat("\n")
  print(elem_obs)

  invisible(object)
}



find_node <- function(object, ...) {
  UseMethod("find_node")
}

find_node.osmar <- function(object, ...) {
  find_node.nodes(object$nodes, ...)
}

find_node.nodes <- function(object, condition, by = c("attrs", "tags")) {
  by <- match.arg(by)
  condition <- substitute(condition)

  id <- subset(object[[by]], eval(condition), select = id)$id

  if ( length(id) == 0 )
    id <- NA_character_

  id
}



find_way <- function(object, ...) {
  UseMethod("find_way")
}

find_way.osmar <- function(object, ...) {
  find_way.ways(object$ways, ...)
}

find_way.ways <- function(object, condition, by = c("attrs", "tags", "refs")) {
  by <- match.arg(by)
  condition <- substitute(condition)

  id <- subset(object[[by]], eval(condition), select = id)$id

  if ( length(id) == 0 )
    id <- NA_character_

  id
}



find_relation <- function(object, ...) {
  UseMethod("find_relation")
}

find_relation.osmar <- function(object, ...) {
  find_relation.relations(object$relations, ...)
}

find_relation.relations <- function(relations, condition,
                                    by = c("attrs", "tags", "refs")) {
  by <- match.arg(by)
  condition <- substitute(condition)

  id <- subset(relations[[by]], eval(condition), select = id)$id

  if ( length(id) == 0 )
    id <- NA_character_

  id
}



subset_by_nodes <- function(x, ids) {

}

