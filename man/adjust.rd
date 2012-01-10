\name{adjust}
\alias{adjust}
\title{Adjusted and standardized mean, proportion and rate}
\description{Computation of adjusted or standardized mean, proportion and rate after generalized linear modelling}
\usage{adjust(adjust = NULL, by, model, standard=NULL, offset=FALSE, 
	type = c("response", "link"), se.fit=TRUE, alpha=.05, 
	ci=FALSE,  ...)
}
\arguments{
	\item{adjust}{expression, indicating independent variable(s) of the model to be adjusted for}
	\item{by}{a list of elements for the grouping variables. The elements of the list will be coerced to factors (if they are not already factors).}
	\item{model}{object of class 'glm' on which the adjustment computation is based}
	\item{standard}{a vector controlling standard values for coefficients in the model}
	\item{offset}{whether the predict results will include the offset term of the model being used for prediction}
	\item{type}{ the type of prediction required.  The default is "response", which uses the scale of the original response variable. For a binomial model, the default estimates are predicted probabilities. For a Poisson model the default estimates are predicted incidence rates if 'offset=FALSE', and predicted counts otherwise.  The alternative "link" transforms the estimates back to the same scale as the linear predictor.}
	\item{se.fit}{whether standard errors to the linear predictors should be returned}
	\item{alpha}{significance level}
	\item{ci}{whether the confidence intervals should be computed}
	\item{...}{additional arguments passed on to other methods}
}
\details{Crude means, proportions and rates among different groups are not readily suitable for comparison among subgroups due to potential confounding. Generalized linear modelling (glm) handles potential confounding and provides coefficients indicating the level of difference between the specific group and the referent group after adjustment for other independent variables in the model. 

The current function 'adjust' adds on information to an existing 'glm' model. It gives predicted values of subgroups specified in 'by' list. The returned predicted values, if type="response", reflect the magnitude (of mean, proportion and rate) of each subgroup after adjustment for the variable(s) specified in the 'adjust' argument. 

The estimated values are based on each adjusted variable being equal to its grand mean value in the dataset of the model. Variables not included in the 'adjust' argument are set to mean values of each subgroup.

Standardization is meant to fix the variable(s) with value(s) specified in the vector 'standard' instead of subgroup mean (when 'adjust' is NULL) or grand mean (when 'adjust' is specified. If there is any conflict between 'adjust' and 'standard', the latter will override the former.

For adjustment, simply give variable name(s) in the 'adjust' argument. For standardization, the argument must be a vector of the same length as the model coefficients less one (since the Intercept term is already standardized as 1). All elements of this vector except those to be standardized should be 'NA'.}

\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'glm', 'predict.glm', 'confint'}
\examples{
library(MASS)
use(Cars93)
des()
model1 <- glm(Price ~ Origin + Horsepower + DriveTrain, family=gaussian)
table(Origin, DriveTrain)
# Crude mean price by Origin and DriveTrain
Table.crude.means <- tapply(Price, list(Origin, DriveTrain), mean)
# Adjusted mean price
adjust(Horsepower, list(Origin, DriveTrain), model=model1)
a <- adjust(Horsepower, list(Origin, DriveTrain), model=model1)
Table.adjusted.means <- xtabs(mean ~ Origin + DriveTrain, data=a)
# Compare crude means with adjusted means of subgroups
Table.crude.means
Table.adjusted.means

# Price by category of DriveTrain adjusted for Horsepower & Origina
adjust(c(Horsepower,Origin), list(DriveTrain), model=model1)

## Now for crude and adjusted probabilities of having manual transmission
manual <- Man.trans.avail =="Yes"
model2 <- glm(manual ~ Origin + Horsepower + DriveTrain, family=binomial)
Table.crude.probabilities <- tapply(manual, list(Origin, DriveTrain), mean)
adjust(Horsepower, by=list(Origin, DriveTrain), model = model2)
b <- adjust(Horsepower, list(Origin, DriveTrain), model = model2)
Table.adjusted.probabilities <- xtabs(probability ~ Origin + DriveTrain, data=b)
# What is the breakdown of probability of having manual transmission 
# if all cars in each subgroup  have 180 horse power?
model2$coefficients # 'Horsepower' is the second variable.
c <- adjust(by=list(Origin, DriveTrain), model=model2, standard=c(NA,180,NA,NA))
Table.standardized.probabilities <- xtabs(probability ~ Origin + DriveTrain, data=c)

# Compare crude and adjusted probabilities
Table.crude.probabilities
Table.adjusted.probabilities
Table.standardized.probabilities

# Age-sex- standardized attack rate
data(Oswego)
use(Oswego)
sex <- factor(sex, labels=c("famale","male"))
pack()
tabpct(sex, ill, percent="row") 
# Crude attack rate = 68.2 percent in males and 51.6 percent in females
agegr <- pyramid(age, sex, bin=30)$ageGroup
lr1 <- glm(ill ~ sex * agegr, family=binomial)
# Assuming a standard population have equal number of male and female
# and uniform distribution of agegr thus the probability is 
# .5 in each sex, 1/2 in each agegr and 1/6  in each age-sex group.
lr1$coefficients # Coefficients of 'agegr' are 3 to 6
adjust(by=list(sex=sex), model=lr1, standard=c(.5,rep(1/3,2),rep(1/6,2)))
# Age- & sex- standardized attack rate=59.9 percent

zap()
data(Montana)
use(Montana)
agegr <- factor(agegr, labels=c("40-49","50-59","60-69","70-79"))
label.var(agegr, "age group")
period <- factor(period, labels=c("1938-1949","1950-1959","1960-1969","1970-1977"))
label.var(period, "period of working")
start <- factor(start, labels=c("pre 1925", "1925 and after"))
label.var(start, "starting year")
model3 <- glm(respdeath ~ agegr + period + start, offset=log(personyrs), family=poisson)
agg <- aggregate.data.frame(.data[,1:2], list(period=period, start=start), mean)
crude.count <- agg[,3]
Table.crude.count <- xtabs(respdeath ~ period + start, data=agg)
crude.personyrs <- agg[,4]
Table.personyrs <- xtabs(personyrs ~ period + start, data=agg)
crude.rate <- agg[,3]/agg[,4]
Table.crude.rate <- xtabs(crude.rate ~ period + start, data=agg)

adjust(adjust=agegr, by=list(period, start), model3)
c <- adjust(adjust=agegr, by=list(period, start), model3)
Table.adjusted.rate <- xtabs(rate ~ period + start, data=c)
d <- adjust(adjust=agegr, by=list(period, start), model3, offset=TRUE, ci=TRUE)
Table.adjusted.count <- xtabs(count ~ period + start, data=d)

# Compare crude and adjusted counts
Table.crude.count
Table.adjusted.count

# Compare crude and adjusted rates
Table.crude.rate
Table.adjusted.rate
}
\keyword{database}
