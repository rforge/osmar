\name{OSM-class}
\alias{OSM}
\alias{OSM-class}

\title{
  Class for OpenStreeMap information
}
\description{
  This function checks it the order of given listelement is right, names the listelements and makes class "OSM" of these elements.
}
\usage{
  OSM(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
    three elements: (1)\code{\link{Node-class}},(2)\code{\link{Way-class}},(3)\code{\link{Relation-class}}
  }
}
\details{
  builds a list of three elements and names the elements "Node", "Way" and "Relation"
}
\value{
  Object of type OSM is returned
}
\section{Methods}{
  \describe{
    \item{plot}{for more details see \code{\link{plot.OSM}}}
    \item{summary}{summary of spatial information and important keys/values}
    \item{findID}{subset of a specific ID}
    \item{findKeyValue}{subset of a specific key or value}
    \item{findTime}{subset of specific changing times}
  }
}

\references{
%% ~put references to the literature/web site here ~
}
\author{
  Thomas Schlesinger
}
\note{
  easiest way to make "OSM" class is by using \code{\link{getOSMObject}}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{getOSMObject}}
  \code{\link{Node-class}}
  \code{\link{Way-class}}
  \code{\link{Relation-class}}
}
\examples{

## its best to look at source code of the getOSMObject-function by just typing   getOSMObject in RConsole
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
