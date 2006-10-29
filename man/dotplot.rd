\name{dotplot}
\alias{dotplot}
\title{Dot plot}
\description{Plot of frequency in dots}
\usage{
dotplot(x, bin = 40, by = NULL, ...)
}
\arguments{
	\item{x}{a vector. Allowed types also include "Date" and "POSIXct"}
	\item{bin}{number of bins for the range of 'x'}
	\item{by}{stratification variable}
	\item{...}{graphical parameters for the dots when there is no stratification}
}
\details{A 'dotplot' is similar to a histogram but without the bins. Each dot represents one record. Attributes of the dots can be further specified in '...' when there is no strafication. Otherwise, the dots are plotted as a diamond shape and the colours are automatically chosen based on the number of strata.}
\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'summ', 'hist'}
\examples{
a <- rep(1:2, 250)
b <- rnorm(500,mean=a)
dotplot(b)
dotplot(b, by=a)
}
\keyword{aplot}