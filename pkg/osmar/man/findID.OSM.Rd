\name{findID.OSM}
\alias{findID.OSM}
\alias{findID}
\title{subset of a specific ID}
\description{
  Given a \code{\link{OSM-class}} this function searches for specific element IDs and 
  builds a new OSM Object with these new elements.
}
\usage{
  findID.OSM(x, ID, full = FALSE, what = "", check = TRUE)
}

\arguments{
  \item{x}{Object of \code{\link{OSM-class}}}
  \item{ID}{needs a vector of character-strings. Each character string describing 
            one ID of an element. If \command{full==TRUE} the length of given vector can only be one}
  \item{full}{logical; if \command{FALSE} only the given IDs will be subsetted; 
              if \command{TRUE} every member referenced in \command{ID} and existing in \command{x} will be subsetted.
              \command{what} has to be specified.}
  \item{what}{character string: if "way" the full way will be subsetted and if "relation" the full
              relation will be subsetted.}
  \item{check}{logical: if \command{TRUE} a check will be made if all referencend members are in \command{x}
                and a message appears.}
}
\details{
  This function combines the three generic function \command{findID.[Node|Way|Relation]}. So there is
  no need in using these.
  
  After searching \command{ID} in Node, Way and Relation objects they will be combined into an
  OSM-object.
  
  \command{full} is used like the parameter \command{full} in \code{\link{getElementXML}}. 
  In case of "relation" it returns the referenced relations, ways [and their nodes] and nodes.
  In case of "way" it returns the referenced nodes.
  
  If you see that returned elements are incomplete, it is recommended to use \code{\link{getElementXML}}
  in combination with \code{\link{getOSMObject}} to get the full data. 
}
\value{object of type \code{\link{OSM-class}}}
\references{}
\author{Thomas Schlesinger}
\note{}

\seealso{
  \code{\link{OSM-class}}
  \code{\link{getElementXML}}
  \code{\link{getOSMObject}}
  \code{\link{findKeyValue}}
  \code{\link{findTime}}
}
\examples{
  xml<-getBboxXML(bbox2coords(c(11.579341,48.15102),c(500,500)), URL=TRUE)
  osm<-getOSMObject(xml, reduced=TRUE) 
  ##simple
  oneway<-findID(osm, c("100024927"))
  summary(oneway$Way)$noWay
  
  moreway<-findID(osm, c("93192031","17969172","80392625"))
  summary(moreway$Way)$noWay
  
  onerelation<-findID(osm, c("1619276"))
  summary(onerelation$Relation)$noRelation
  
  morerelation<-findID(osm, c("1619276", "1619275", "64019"))
  summary(morerelation$Relation)$noRelation
  
  mixed<-findID(osm, c("100024927","1619276","1318876598"))
  summary(mixed$Node)$noNode
  summary(mixed$Way)$noWay
  summary(mixed$Relation)$noRelation
  
  ##full
  fullrelation<-findID(osm, c("207678"), full=TRUE, what="relation")
    #25 (of 26) nodes are missing
    #68 (of 77) ways are missing
    #0 (of 1) relations are missing
  
  fullway<-findID(osm, c("80175400"), full=TRUE, what="way")
    #0 (of 6) nodes are missing
    #0 (of 1) ways are missing     
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
