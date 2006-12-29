\name{Ectopic pregnancy}
\alias{ectopic}
\docType{data}
\title{ Dataset of a case-control study looking at history of 
abortion as a risk factor for ectopic pregnancy}
\description{
This case-control study has one case series and two control groups.

The subjects were recruited based on three types of pregnancy outcome  (outc): 

EP = ectopic pregnancy

IA = women coming for induced abortion

Deli = women admitted for full-term delivery                 

Two exposure variables of interest are:

hia      Previous induced abortion

gravi    Gravidity (number of pregnacies)
}
\usage{data(ectopic)}
\format{
  A data frame with 723 observations on the following 4 variables.
  \describe{
    \item{\code{id}}{a numeric vector}
    \item{\code{outc}}{a factor with levels \code{EP} \code{IA} \code{Deli}}
    \item{\code{hia}}{a factor with levels \code{never IA} \code{ever IA}}
    \item{\code{gravi}}{a factor with levels \code{1-2} \code{3-4} \code{>4}}
  }
}

\examples{
data(ectopic)
library(nnet)
use(ectopic)
multi1 <- multinom(outc ~ hia + gravi)
summary(multi1)
mlogit.display(multi1)

# Changing referent group of outcome
ep <- outc == "EP"
ia <- outc == "IA"
deli <- outc == "Deli"
multi2 <- multinom(cbind(ia, ep, deli) ~ hia + gravi)
summary(multi2)
mlogit.display(multi2)
}
\keyword{datasets}
