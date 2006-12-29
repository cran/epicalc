\name{Hookworm and blood loss}
\alias{suwit}
\docType{data}
\title{ Hookworm infection and blood loss: SEAJTM 1970}
\description{
A study using radio-isotope to examine daily blood loss
and number of hookworms infecting the patients.

worm = number of hookworm

bloss = estimated daily blood loss (ml/day)
}
\usage{data(suwit)}
\format{
  A data frame with 15 observations on the following 3 variables.
  \describe{
    \item{\code{id}}{a numeric vector}
    \item{\code{worm}}{a numeric vector}
    \item{\code{bloss}}{a numeric vector}
  }
}
\source{Areekul, S., Devakul, K., Viravan, C., Harinasuta, C. 1970 
Studies on blood loss, iron absorption and iron reabsorption 
in hookworm patients in Thailand. 
\emph{Southeast Asian J Trop Med Pub Hlth} \bold{1(4)}: 519-523.}
\references{
  ~~ possibly secondary sources and usages ~~
}
\examples{
data(suwit)
use(suwit)
des()
plot(worm, bloss, type="n")
text(worm, bloss, labels=id)
abline(lm(bloss ~ worm), col="red")
}
\keyword{datasets}
