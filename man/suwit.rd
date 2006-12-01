\name{Hookworm and blood loss}
\docType{data}
\alias{suwit}
\title{Hookworm infection and blood loss: SEAJTM 1970}
\description{
A study using radio-isotope to examine daily blood loss
and number of hookworms infecting the patients.

worm = number of hookworm

bloss = estimated daily blood loss (ml/day)
}
\usage{data(suwit)}
\format{A data frame containing 15 observations and 3 variables.}
\source{Areekul, S., Devakul, K., Viravan, C., Harinasuta, C. 1970 
Studies on blood loss, iron absorption and iron reabsorption 
in hookworm patients in Thailand. 
\emph{Southeast Asian J Trop Med Pub Hlth} \bold{1(4)}: 519-523.}
\examples{
data(suwit)
use(suwit)
des()
plot(worm, bloss, type="n")
text(worm, bloss, labels=id)
abline(lm(bloss ~ worm), col="red")
}
\keyword{datasets}
    