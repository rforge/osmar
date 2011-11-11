

#' Convert osmar object to sp object
#'
#' Convert an osmar object to a \link[sp]{sp} object.
#'
#' @param obj An \code{\link{osmar}} object
#' @param ... Ignored
#'
#' @return A \link[sp]{sp} object
#'
#' @note Not yet implemented!
#'
#' @export
as_sp <- function(obj, ...) {
  stopifnot(class(obj) == "osmar")

  stop("Not yet implemented")
}

