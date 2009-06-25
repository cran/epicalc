\name{lagVar}
\alias{lagVar}
\title{Create a vector of lagged or subsequent value}
\description{Create a vector of lagged or subsequent value in a long form longitudinal data}
\usage{lagVar(var, id, time, lag.unit=1)
}
\details{Data must be in long format having variable to create the lag, id and time.

The default value of lag.unit is 1. When the number is negative, the next measured is created instead. }

\arguments{
	\item{var}{variable to create the lag}
	\item{id}{subject identification field}
	\item{time}{time of measurement}
	\item{lag.unit}{lag number of visits}
}
\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'lag'}
\examples{
## Identification of the tree that became smaller during followup
data(Sitka, package="MASS")
use(Sitka)
lag1.size <- lagVar(var=size, id=tree, time=Time, lag=1)
data.frame(tree=tree, time=Time, size=size, lag1.size=lag1.size) [1:20,]
# Answer
data.frame(Time, tree, size, lag1.size) [which(lag1.size > size),]

# Alternatively
next.size <- lagVar(size, tree, Time, lag=-1)
data.frame(tree=tree, time=Time, size=size, next.size=next.size) [1:20,]
data.frame(Time, tree, size, next.size) [which(size > next.size),]

}
\keyword{database}
