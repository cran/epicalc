\name{Sleepiness}
\alias{sleep3}
\docType{data}
\title{ Sleepiness in a workshop}
\description{ Sleepiness among participants in a workshop}
\usage{data(sleep3)}
\format{
  A data frame with 15 observations on the following 8 variables.
  \describe{
    \item{\code{id}}{a numeric vector}
    \item{\code{gender}}{a factor with levels \code{male} \code{female}}
    \item{\code{dbirth}}{a Date for birth date}
    \item{\code{sleepy}}{a numeric vector for any experience of sleepiness in the class: 0=no, 1=yes}
    \item{\code{lecture}}{a numeric vector for ever felt sleepy during a lecture: 0=no, 1=yes}
    \item{\code{grwork}}{a numeric vector for ever felt sleepy during a group work: 0=no, 1=yes}
    \item{\code{kg}}{a numeric vector}
    \item{\code{cm}}{a numeric vector}
  }
}
\examples{
data(sleep3)
use(sleep3)
des()
}
\keyword{datasets}
