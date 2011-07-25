\name{plot-osmar}
\alias{plot.OSM}
\alias{plot.Node}
\alias{plot.Way}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Plotting Object of osmar-package
}
\description{
  
}
\usage{
  \method{plot}{Node}(x, pch=20, data="no",...)
  \method{plot}{Way}(x, ...)
  \method{plot}{OSM}(x, nodes=FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Object of \code{\link{Node-class}}, \code{\link{Way-class}} or \code{\link{OSM-class}}}
  \item{pch}{point character code for Nodes. see \code{\link{points}}}
  \item{data}{three options: (1) "no"= all Nodes will be plotted, 
                (2) "yes"= Nodes with additional data will be marked with pch=8 and col="red"
                (3) "only"= only Nodes with data will be plotted}
  \item{nodes}{Nodes with additional data will be marked with col="red"}
  \item{\dots}{Arguments to be passed to plot-methods}
}
\details{
  \describe{
    \item{plot.Node}{uses generic plot method for \linkS4class{SpatialPoints}}
    \item{plot.Way}{uses generic plot method for \linkS4class{SpatialLines}}
    \item{plot.OSM}{uses plot.Way or in case of no Ways plot.Node}
  }
}
\value{
  plot window with graphics
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
  xml<-getBboxXML(bbox2coords(c(11.581262,48.150626),c(500,500)), URL=TRUE)
  uni<-getOSMObject(xml, reduced=TRUE)    ##11.03
  uni2<-getOSMObject(xml, reduced=FALSE)  ##7.53
  
  plot(uni, nodes=TRUE)
  plot(uni, nodes=FALSE)
  plot(uni2, nodes=TRUE)
  plot(uni2,nodes=FALSE)  
  op<-par(mfrow=c(2,2))
    plot(uni$Node)
    plot(uni$Node, data="yes")
    plot(uni$Node, data="only")
  par(op)
  
  plot(uni$Way)
  plot(uni$Relation)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
