\name{expand}
\alias{expand}
\title{Expand an aggregated data frame}
\description{Expand an 'aggregate'd data frame into a case-by-case format based on the values specified in a column}
\usage{expand(aggregate.data, index.var = "Freq", retain.freq = FALSE)
}
\details{An aggregated data frame has one variable (colunm) indicating the number or frequency of replication of subjects having the same values of other variables as the index record.

'expand' replicates the row using the value in 'index.var' as the number of replications.

'retain.freq' indicates whether the 'index.var', which is the frequency, should be retained. }
\note{The aggregated data frame is not changed. Remember to assign the result}
\arguments{
	\item{aggregate.data}{an aggregate data frame having a variable indicating the replication of subjects having that combination of characteristics, which are indicated by other variables}
	\item{index.var}{name of a variable indicating frequency of replication}
	\item{retain.freq}{whether the index variable or frequency variable should be retained in the return data frame}
}
\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'table', 'xtabs', 'aggregate'}
\examples{
## Expanding an aggregate data frame
data(ANCtable)
des(ANCtable)
a <- expand(ANCtable)
des(a)

## Aggregating a case-by-case data frame
data(ANCdata)
use(ANCdata)
des()
id <- 1:nrow(ANCdata)
aggregate.numeric(id, by=list(Death=death, Anc=anc, Clinic=clinic), 
	FUN="count")
}
\keyword{database}
