\name{as_osmar}
\alias{as_osmar}
\alias{osmar}
\title{Convert OSM-XML to an osmar object}
\usage{
  as_osmar(xml)
}
\arguments{
  \item{xml}{An OSM-XML object}
}
\value{
  A list (with class attribute \code{osmar}) with three
  elements:

  \describe{

  \item{\code{node}}{...}

  \item{\code{way}}{...}

  \item{\code{relation}}{...}

  }
}
\description{
  Convert a given OSM-XML object (as parsed by
  \code{\link[XML]{xmlParse}}) to an osmar object.
}

