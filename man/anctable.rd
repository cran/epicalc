\name{ANC Table}
\docType{data}
\alias{ANCtable}
\title{Dataset on effect of new ANC method on mortality (as a table)}
\description{
This dataset contains frequency of various combinations 
of methods of antenatal care in two clinics with the outcome being 
perinatal mortality.
}
\usage{
data(ANCtable)
}
\format{
  A data frame with 8 observations on the following 4 variables.
  \describe{
    \item{\code{death}}{a numeric vector: 1=no, 2=yes}
    \item{\code{anc}}{a numeric vector indicating antenatal care type: 1=old 2=new }
    \item{\code{clinic}}{a numeric vector indicating clinic code: 1=clinic A, 
    2=clinic B}
    \item{\code{Freq}}{a numeric vector of frequencies}
  }
}
\examples{
data(ANCtable)
use(ANCtable)
death <- death==2
anc <- factor(anc); levels(anc) <- c("old", "new")
clinic <- factor(clinic); levels(clinic) <- c("A","B") 
glm1 <- glm(death ~ anc ,weights=Freq, family=binomial)
logistic.display(glm1)
glm2 <- glm(death ~ anc + clinic ,weights=Freq,	family=binomial)
logistic.display(glm2)
lrtest(glm1, glm2)
rm(death, anc, clinic)
}
\keyword{datasets}
    