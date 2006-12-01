\name{Cancer survival}
\docType{data}
\alias{compaq}
\title{Dataset on cancer survival}
\description{
This dataset contains 

Variable       Description

id             identification number

hospital       hospital   

status         status of subject 0=censored, 1=failed            

stage          stage of the disease

agegr          Age group (years): <40, 40-49, 50-59, 60+ 

ses            socio-economic status: Rich, high-middle class, poor-middle class, poor.            

year           Duration of followup in years           
}
\usage{data(compaq)}
\format{A data frame containing 1064 observations and 7 variables.}
\examples{
data(compaq)
use(compaq)
des()
}
\keyword{datasets}
    