\name{Risk.display}
\alias{logistic.display}
\alias{regress.display}
\alias{idr.display}
\alias{mlogit.display}
\alias{ordinal.or.display}
\alias{tableGlm}
\title{Tables for multivariate odds ratio, incidence density etc}
\description{Display of various epidemiological modelling results in a medically understandable format}
\usage{
logistic.display(logistic.model, alpha = 0.05, crude = TRUE, crude.p.value = FALSE, 
    decimal = 2) 
regress.display(regress.model, alpha = 0.05, crude = FALSE, crude.p.value = FALSE, 
    decimal = 2) 
idr.display(count.model, decimal = 3, alpha = 0.05) 
mlogit.display(multinom.model, decimal = 2, alpha = 0.05) 
ordinal.or.display(ordinal.model, decimal = 3, alpha = 0.05)  
tableGlm (model, modified.coeff.array, decimal) 
}
\details{R provides several epidemiological modelling techniques. The functions above display these results in a format easier for medical people to understand.

The function 'tableGlm' is not for general use. It is called by 'logistic.display' and 'regress.display' to receive the 'modified.coeff.array' and produce the output table.

The output from 'logistic.display' and 'regress.display' are ready to write (using 'write.csv') to a .csv file which can then be copied to Word document for a manuscript. This approach can substantially reduce time and errors due conventional manual copying.
}
\arguments{
	\item{logistic.model}{a model from a logistic regression}
	\item{regress.model}{a model from linear regression}
	\item{alpha}{significance level}
	\item{crude}{whether crude odds ratios should also be displayed}
	\item{crude.p.value}{whether crude P value should also be displayed}
	\item{decimal}{number of decimal places displayed}
	\item{count.model}{a model from a Poisson or negative binomial regression}
	\item{multinom.model}{a model from multinomial or polytomous regression}
	\item{ordinal.model}{a model from an ordinal logistic regression}
  \item{model}{model passed from logistic.display or regress.display to tableGlm}
  \item{modified.coeff.array}{array modified by from coefficient array and sent to the function 'tableGlm' to produce the output}
}
\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\value{'logistic.display' and 'regress.display' each produces an output table. See 'details'.}
\seealso{'glm', 'confint'}
\examples{
model0 <- glm(case ~ induced + spontaneous, family=binomial, data=infert)
summary(model0)
logistic.display(model0)

data(ANCdata)
glm1 <- glm(death ~ anc + clinic, family=binomial, data=ANCdata)
logistic.display(glm1)
 
library(MASS)
model1 <- glm(Origin ~ Weight + AirBags + DriveTrain, family=binomial, data=Cars93)
logistic.display(model1, decimal=3, crude.p.value=TRUE)

reg1 <- lm(Price ~ Weight + AirBags + DriveTrain, data=Cars93)
regress.display(reg1)

reg2 <- glm(Price ~ Weight + AirBags + DriveTrain, data=Cars93)
regress.display(reg2)


library(nnet)

# Ordinal logistic regression
options(contrasts = c("contr.treatment", "contr.poly"))
house.plr <- polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
house.plr
ordinal.or.display(house.plr)

# Polytomous or multinomial logistic regression
house.multinom <- multinom(Sat ~ Infl + Type + Cont, weights = Freq, 
	data = housing)
summary(house.multinom)
mlogit.display(house.multinom, alpha=.01) # with 99 percent confidence limits.
}
\keyword{database}
