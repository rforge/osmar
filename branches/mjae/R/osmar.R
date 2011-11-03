

#' @S3method print osmar
print.osmar <- function(x, ...) {
  bbox <- attr(x, "bbox")

  cat("Osmar object\n\n")
  cat("Bounding box:", paste(round(bbox, 2), collapse = ", "), "\n")

  invisible(x)
}


#' @S3method summary osmar
summary.osmar <- function(object, ...) {
  ## TODO: change to print.summary.osmar
  
  elem_obs <- lapply(object, function(x) nrow(x[[1]]))
  elem_obs <- as.data.frame(elem_obs)
  rownames(elem_obs) <- "Observations"

  print(object)
  cat("\n")
  print(elem_obs)

  invisible(object)
}
