\name{Oswego}
\docType{data}
\alias{Oswego}
\title{Dataset from an outbreak of food poisoning in US}
\description{
This dataset contains 75 records of persons under investigation
for the cause of acute food poisoning after a dinner party.
}
\usage{data(Oswego)}
\format{A data frame containing 75 observations and 20 variables.}
\source{EpiInfo package}
\references{
See: \url{http://www.cdc.gov/eis/casestudies/casestudyex.htm}.
}
\examples{
zap()
data(Oswego)
use(Oswego)
pyramid(age, sex)
}
\keyword{datasets}
    