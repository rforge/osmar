\name{Relation-class}
\alias{Relation}
\alias{Relation-class} 
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Class for Relation information
}
\description{
  Makes a S3-class object called "Relation"
}
\usage{
  Relation(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
    expects three elements. 1st element being a "RelationMeta"-dataframe or "character", 
    2nd element a "RelationDataFrame" or "character" and
    3rd element a "RelationMember"-dataframe.
  }  
}
\details{
  Function is not meant to be used by users.
}
\value{
  The third element is a S3-class called "Relation" with 3 elements. Relations don't
  have Spatial information which can be converted into an object of the sp package.
  (1) MetaInformation about the Relations itself. These usually is stored in the SpatialObjects (in case of Node and Way),
  (2) Data and
  (3) data.frame containing the members and their role in a relation.
  If no Relations where in the XML-File the 3 elements will be character strings. 
}
\section{Methods}{
  \describe{
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
  \code{\link{Way-class}}
  \code{\link{OSM-class}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
