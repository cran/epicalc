\name{Timing exercise}
\alias{timing}
\docType{data}
\title{ Dataset on time going to bed, waking up and arrival at the workshop}
\description{
This dataset came from an interview survey on the workshop attendants
on 2004-12-14.

Note that the date of bed time was 2004-12-13 if the respondent went
to bed before midnight. 
}
\usage{data(timing)}
\format{
  A data frame with 18 observations on the following 11 variables.
  \describe{
    \item{\code{id}}{a numeric vector}
    \item{\code{gender}}{a factor with levels \code{male} \code{female}}
    \item{\code{age}}{a numeric vector}
    \item{\code{marital}}{a factor with levels \code{single} \code{married} \code{others}}
    \item{\code{child}}{a numeric vector}
    \item{\code{bedhr}}{a numeric vector}
    \item{\code{bedmin}}{a numeric vector}
    \item{\code{wokhr}}{a numeric vector}
    \item{\code{wokmin}}{a numeric vector}
    \item{\code{arrhr}}{a numeric vector}
    \item{\code{arrmin}}{a numeric vector}
  }
}
\examples{
data(timing)
use(timing)
des()
arrival.time <- ISOdatetime(year=2004, month=12, day=14, hour=arrhr,
	min=arrmin, sec=0)
summ(arrival.time, by= gender)
}
\keyword{datasets}
