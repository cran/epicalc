\name{poisgof}
\alias{poisgof}
\title{Goodness of fit test for modeling of count data}
\description{Poisson and negative binomial regression are used for modeling count data. The command tests the deviance against degrees of freedom in the model determining whether there is overdispersion.}
\usage{
poisgof (model)
}
\arguments{
	\item{model}{A Poisson or negative binomial model}
}
\details{
To test the significance of overdispersion of the errors of a Poisson or negative binomial model, the deviance is test against degrees of freedom using chi-squared distribution. A low P value indicates significant overdispersion.
}
\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{`glm'}
\examples{
library(MASS)
quine.pois <- glm(Days ~ Sex/(Age + Eth*Lrn), data = quine, family=poisson)
poisgof(quine.pois)
quine.nb1 <- glm.nb(Days ~ Sex/(Age + Eth*Lrn), data = quine)
poisgof(quine.nb1)
}
\keyword{htest}
