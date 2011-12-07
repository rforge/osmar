#' @include osm-descriptors.R
{}



#' @S3method summary osmar
summary.osmar <- function(object, ...) {
  warning("Not yet implemented!")
}



### Summarize nodes: #################################################

#' @S3method summary nodes
summary.nodes <- function(object, ...) {
  ret <- list()

  ret$nelements <- nrow(object$attrs)
  ret$ntags <- nrow(object$tags)

  ret$bbox <- list(lat = range(object$attrs$lat),
                   lon = range(object$attrs$lon))

  ret$key <- sort(table(object$tags$k), decreasing = TRUE)
  ret$key <- data.frame(Key = names(ret$key),
                        Freq = unname(ret$key),
                        stringsAsFactors = FALSE)
  rownames(ret$key) <- NULL

  ret$val <- sort(table(object$tags$v), decreasing = TRUE)
  ret$val <- data.frame(Value = names(ret$val),
                        Freq = unname(ret$val),
                        stringsAsFactors = FALSE)
  rownames(ret$val) <- NULL

  ret$keyval <- as.data.frame(table(Key = object$tags$k,
                                    Value = object$tags$v))
  ret$keyval <- ret$keyval[ret$keyval$Freq > 0, ]
  ret$keyval <- ret$keyval[order(-ret$keyval$Freq), ]
  rownames(ret$keyval) <- NULL

  structure(ret, class = c("summary.nodes", class(ret)))
}



#' @S3method print summary.nodes
print.summary.nodes <- function(x, max.print = 10, ...) {
  cat("osmar$nodes object\n")
  cat(x$nelements, "nodes,", x$ntags, "tags", "\n\n")

  cat("Latitude :", paste(c("min", "max"), round(x$bbox$lat, 6),
                         sep = " = ", collapse = ", "), "\n")
  cat("Longitude:", paste(c("min", "max"), round(x$bbox$lon, 6),
                          sep = " = ", collapse = ", "), "\n\n")

  print(x$keyval[seq(min(max.print, nrow(x$keyval))), ])
}



### Summarize ways: ##################################################
