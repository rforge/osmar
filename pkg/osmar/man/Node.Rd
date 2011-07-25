\name{Node-class}
\alias{Node}
\alias{Node-class} 
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Class for Node information
}
\description{
  Makes a S3-class object called "Node"
}
\usage{
  Node(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
    expects two elements. 1st element being a "nodeDataFrame" or "character", 2nd element a \linkS4class{SpatialPointsDataFrame} or "character"
  }  
}
\details{
  Function is not meant to be used by users.
}
\value{
  S3-class called "Node" with 2 elements: (1)Data and (2) a
  \linkS4class{SpatialPointsDataFrame} with geographical information about the Nodes. If no Nodes 
  where in the XML-File the 2 elements will be character strings.
}
\section{Methods}{
  \describe{
    \item{plot}{for more details see \code{\link{plot.Node}}}
    \item{summary}{summary of spatial information and important keys/values}
    \item{findID}{subset of a specific ID}
  }
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
  Thomas Schlesinger
}
\note{
%%  ~~further notes~~
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{getOSMObject}}
  \code{\link{Way-class}}
  \code{\link{Relation-class}}
  \code{\link{OSM-class}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
