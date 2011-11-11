
#' Convert osmar object to igraph
#'
#' Convert an osmar object to an igraph (see
#' \link[igraph]{igraph-package}).
#'
#' @param obj An \code{\link{osmar}} object
#'
#' @return An igraph \code{graph} object
#'
#' @note Not yet implemented!
#'
#' @export
as_igraph <- function(obj) {
  stopifnot(class(obj) == "osmar")
  stopifnot(require("igraph"))

  stop("Not yet implemented")
}

