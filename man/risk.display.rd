\name{Risk.display}
\alias{logistic.display}
\alias{idr.display}
\alias{mlogit.display}
\alias{ordinal.or.display}
\title{Tables for multivariate odds ratio, incidence density etc}
\description{Display of various epidemiological modelling results in a medically understandable format}
\usage{
logistic.display(logistic.model, alpha = 0.05, decimal = 3) 
idr.display(count.model, decimal = 3, alpha = 0.05) 
mlogit.display(multinom.model, decimal = 2, alpha = 0.05) 
ordinal.or.display(ordinal.model, decimal = 3, alpha = 0.05)  
}
\details{R provides several epidemiological modelling techniques. The functions above display these results in a format easier for medical people to understand.
}
\arguments{
	\item{logistic.model}{a model from a logistic regression}
	\item{alpha}{significance level}
	\item{decimal}{number of decimal places displayed}
	\item{count.model}{a model from a Poisson or negative binomial regression}
	\item{multinom.model}{a model from multinomial or polytomous regression}
	\item{ordinal.model}{a model from an ordinal logistic regression}
}
\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'glm', 'confint'}
\examples{
model0 <- glm(case ~ induced + spontaneous, family=binomial, data=infert)
summary(model0)
logistic.display(model0)

library(MASS)
library(nnet)

# Ordinal logistic regression
options(contrasts = c("contr.treatment", "contr.poly"))
house.plr <- polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
house.plr
ordinal.or.display(house.plr)

# Polytomous or multinomial logistic regression
house.multinom <- multinom(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
summary(house.multinom)
mlogit.display(house.multinom, alpha=.01) # with 99 percent confidence limits.
}
\keyword{database}
