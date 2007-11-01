\name{Hakimi's data}
\docType{data}
\alias{Hakimi}
\title{Dataset on effect of training personnel on neonatal mortality}
\description{
Subset of a dataset from an intervention trial of education on personnel and the effect on neonatal mortality. Non-fatal records were randomly selected from the original dataset, just for practice and interpretation of interaction term.

dead = neonatal dead: 1=yes, 0=no

treatment = intervention programme: 1=yes, 2=no

malpres = malpresentation of fetus: 1=yes, 0=no

}
\usage{data(Hakimi)}
\format{A data frame containing 456 observations and 4 variables.}
\examples{
data(Hakimi)
use(Hakimi)
des()
cc(dead, treatment)
mhor(dead, treatment, malpres)
}
\keyword{datasets}
    