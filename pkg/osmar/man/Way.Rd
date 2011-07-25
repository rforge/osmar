\name{Way-class}
\alias{Way}
\alias{Way-class} 
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Class for Way information
}
\description{
  Makes a S3-class object called "Way"
}
\usage{
  Way(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
    expects three elements. 1st element being a "WayDataFrame" or "character", 
    2nd element a \linkS4class{SpatialLinesDataFrame}, "wayMeta"-dataframe or "character" and
    3rd element a "wayMember"-dataframe.
  }  
}
\details{
  Function is not meant to be used by users.
}
\value{
  S3-class called "Way" with 3 elements: 
  (1)Data,
  (2)\linkS4class{SpatialLinesDataFrame} with geographical information about the Ways
  and (3) a data.frame containing the NodeIDs which are referenced in a Way. 
  If no Nodes are available, a SpatialLinesDataFrame can not be build. In this case MetaInformation
  about the Ways are stored in the second slot. If no Ways where in the XML-File the 3 elements will
  be character strings. 
}
\section{Methods}{
  \describe{
    \item{plot}{for more details see \code{\link{plot.Way}}}
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
  \code{\link{Node-class}}
  \code{\link{Relation-class}}
  \code{\link{OSM-class}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
