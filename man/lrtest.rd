\name{lrtest}
\alias{lrtest}
\title{Likelihood ratio test}
\description{Likelihood ratio test for 'glm'}
\usage{
lrtest (model1, model2, print=TRUE)
}
\details{Likelihood ratio test checks the difference between -2*logLikelihood of the two models against the change in degrees of freedom using a chi-squared test. It is best applied to a model from 'glm' to test the effect of a factor with more than two levels. The records used in the dataset for both models MUST be the same. 
}
\arguments{
	\item{model1, model2}{Two models having the set of same records and the same type ('family' and 'link') of modelling. }
	\item{print}{whether the results will be printed}
}
\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'glm', 'logLik', 'deviance'}
\examples{
model0 <- glm(case ~ induced + spontaneous, family=binomial, data=infert)
model1 <- glm(case ~ induced, family=binomial, data=infert)
lrtest (model0, model1)
lrtest (model1, model0) # same answer
lrtest (model1, model0) -> a
a
}
\keyword{htest}
