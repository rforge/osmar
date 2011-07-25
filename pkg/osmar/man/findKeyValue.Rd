\name{findKeyValue}
\alias{findKeyValue}

\title{subset of a specific key or/and value}
\description{
  Given a \code{\link{OSM-class}} this function searches for specific elements with given
  key or value and builds a new OSM Object with these new elements.
}
\usage{findKeyValue(x, key = "", value = "")}

\arguments{
  \item{x}{Object of \code{\link{OSM-class}}}
  \item{key}{character matching the key to be extracted. It can be given alone or 
              in combination with \command{value}}
  \item{value}{character matching the value to be extracted. It can be given alone or
              in combination with\command{key}}
}
\details{
  Basicly this function searches through the Node, Way and Relation objects in \command{x}
  and finds the IDs with the given \command{key} and/or \command{value}.
  Then the IDs are given to \code{\link{findID}} and an \code{\link{OSM-class}} is build.
  Very similar to \code{\link{findTime}}.  
}
\value{object of type \code{\link{OSM-class}}}
\author{Thomas Schlesinger}
\note{}

\seealso{
  \code{\link{findID}}
  \code{\link{findTime}}
}
\examples{
  coords<-bbox2coords(c(11.579341,48.15102),c(500,500)) 
  xml<- getBboxXML(coords, URL=TRUE)
  osm<-getOSMObject(xml, reduced=TRUE)
  
  ligths<-findKeyValue(osm, value="traffic_signals")
  class(ligths)
  plot(osm)
  plot(ligths, col=2, add=TRUE)
  
  roads<-findKeyValue(osm, key="highway")
  plot(roads, col=3, lwd=2, add=TRUE)
  
  bike<-findKeyValue(osm, key="highway", value="cycleway")
  plot(bike, col=4, lwd=3, add=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
