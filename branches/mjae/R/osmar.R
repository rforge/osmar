

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



### Finding and subsetting methods: ##################################

attrs <- function(condition) {
  structure(substitute(condition), what = "attrs")
}

tags <- function(condition) {
  structure(substitute(condition), what = "tags")
}

refs <- function(condition) {
  structure(substitute(condition), what = "refs")
}





find_node <- function(object, ...) {
  UseMethod("find_node")
}

find_node.osmar <- function(object, ...) {
  find_node.nodes(object$nodes, ...)
}

find_node.nodes <- function(object, condition) {
  stopifnot(class(condition) == "call")
  stopifnot(attr(condition, "what") %in% c("attrs", "tags"))

  what <- attr(condition, "what")
  id <- subset(object[[what]], eval(condition), select = id)$id

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
  stopifnot(class(condition) == "call")
  stopifnot(attr(condition, "what") %in% c("attrs", "tags", "refs"))

  what <- attr(condition, "what")
  id <- subset(object[[what]], eval(condition), select = id)$id

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
  stopifnot(class(condition) == "call")
  stopifnot(attr(condition, "what") %in% c("attrs", "tags", "refs"))

  what <- attr(condition, "what")
  id <- subset(object[[what]], eval(condition), select = id)$id

  if ( length(id) == 0 )
    id <- numeric(NA)

  id
}



subset_by_nodes <- function(x, id) {

}

