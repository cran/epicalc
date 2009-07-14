\name{markVisits}
\alias{markVisits}
\title{Mark visits of subjects in a long format}
\description{Mark visits of subjects in a longitudinal data frame}
\usage{markVisits(id, time)
}
\details{
Visit numbers are essential in longitudinal data analysis. This function make it easy for R user to do so.

If visits marked by this function is going to be further used for calculation of lag difference, there must not be any missing visit in the data set.
 }
\note{This was created from combination of the functions 'rle' and 'sapply'}
\arguments{
	\item{id}{subject identification field}
	\item{time}{time of visit}
}
\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'rle', 'sapply'}
\examples{
## Data frame
data(Sitka, package="MASS")
use(Sitka)

## Classical R methods
list1 <- rle(tree)
list1
visit1 <- unlist(sapply(X=list1$lengths, FUN=function(x) 1:x, simplify=FALSE))
visit1

## Do it again by Epicalc
visit2 <- markVisits(id=tree, time=Time)
visit2
}
\keyword{database}
