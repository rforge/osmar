\name{bbox}
\alias{bbox}
\alias{center_bbox}
\alias{get_osm_elements}
\alias{node}
\alias{relation}
\alias{way}
\title{Get OSM elements}
\usage{
  bbox(left, bottom, right, top)

  center_bbox(center_lon, center_lat, width, height)

  node(id)

  way(id)

  relation(id)
}
\arguments{
  \item{left}{Minimum longitude}

  \item{bottom}{Minimum latitude}

  \item{right}{Maximum longitude}

  \item{top}{Maximum latitutde}

  \item{center_lon}{Center longitude}

  \item{center_lat}{Center latitude}

  \item{width}{Box width}

  \item{height}{Box height}

  \item{id}{OSM ID}
}
\description{
  Utility functions to specify \emph{what} to get from the
  OSM data source. These are the request elements which
  work for every source, see the specific sources for
  specialized elements.
}
\seealso{
  \code{\link{get_osm}}
}

