\name{Timing exercise}
\docType{data}
\alias{timing}
\title{Dataset on timing to bed, wake up and arrival at the workshop}
\description{
This dataset came from an interview survey on the workshop attendants
on 2004-12-14. Variables include demographic characteristics, number
of children (taken care by the women attendants), bed time, wake up
time, and arrival time at the workshop.

Note that the date of bed time was 2004-12-13 if the respondent went
to bed before midnight. 
}
\usage{data(timing)}
\format{A data frame containing 18 observations and 11 variables.}
\examples{
data(timing)
use(timing)
des()
arrival.time <- ISOdatetime(year=2004, month=12, day=14, hour=arrhr,
	min=arrmin, sec=0)
summ(arrival.time, by= gender)
}
\keyword{datasets}
    