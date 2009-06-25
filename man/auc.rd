\name{auc}
\alias{auc}
\title{Area under time-concentration curve}
\description{Compute area under time-concentration curve for individuals}
\usage{auc(conc, time, id=NULL)
}
\details{This function compute auc using simple trapezoid summation.

id=NULL is used when concentration (conc) and time are from only one subject.

 }
\arguments{
	\item{conc}{concentration}
	\item{time}{time point where the concentration was measured}
	\item{id}{subject identification}
}
\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'auc' in 'PK' package}
\examples{
# Using 'by' and 'sapply' to compute individual auc of Indometh data
tmp <- by(data=Indometh, INDICES = Indometh$Subject, FUN = function(x) auc(conc=x$conc, time=x$time, id=NULL))
sapply(tmp, as.numeric)

# A better way to compute
use(Indometh)
auc(conc=conc, time=time, id=Subject)
}
\keyword{database}
