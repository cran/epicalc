\name{ANC Table}
\docType{data}
\alias{anctable}
\title{Dataset on effect of new ANC method on mortality (as a table)}
\description{
This dataset presents frequency of various combination 
of methods of antenatal care in two clinics and the perinatal mortality.
}
\usage{
data(anctable)
}
\format{
  A data frame with 8 observations on the following 4 variables.
  \describe{
    \item{\code{death}}{a numeric vector: 1=yes, 2=no}
    \item{\code{anc}}{a numeric vector indicating antenatal care type: 1=old 2=new }
    \item{\code{clinic}}{a numeric vector indicating clinic code}
    \item{\code{Freq}}{a numeric vector for frequency}
  }
}
\examples{
data(anctable)
use(anctable)
des()
glm1 <- glm(death==1 ~ factor(anc) ,weights=Freq, family=binomial)
logistic.display(glm1)
glm2 <- glm(death==1 ~ factor(anc) + factor(clinic) ,weights=Freq, family=binomial)
logistic.display(glm2)
lrtest(glm1, glm2)
}
\keyword{datasets}
    