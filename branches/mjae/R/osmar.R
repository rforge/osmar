

#' @S3method print osmar
print.osmar <- function(x, ...) {
  id <- attr(x, "identifier")
  what <- class(id)[1]

  cat("Osmar object by ", what, ":\n", sep = "")
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
