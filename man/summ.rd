\name{summ}
\alias{summ}
\title{Summary with graph}
\description{Summary of data frame in a convenient table. Summary of a variable with statistics and graph}
\usage{
summ(x=.data, by=NULL, graph=TRUE, box=FALSE)
}
\details{For data frames, 'summ' gives basic statistics of each variable in the data frame. The other arguments are ignored.

For single vectors, a sorted dot chart is also provided, if graph=TRUE (default).}
\arguments{
	\item{x}{'x' can be a data frame or a vector. 'summ()' is the same as 'summ(.data)'}
	\item{by}{a stratification variable, valid only when x is a vector}
	\item{graph}{automatic plot (sorted dot chart) if 'x' is a vector}
	\item{box}{add a boxplot to the graph (by=NULL)}
}
\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'summary', 'use', 'des'}
\examples{
data(Oswego)
use(Oswego)
summ()
summ(age)
summ(age, box=TRUE)
summ(age, by=sex)
}
\keyword{database}
