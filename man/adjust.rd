\name{adjust}
\alias{adjust}
\title{Adjusted mean, proportion and rate}
\description{Computation of adjusted mean, proportion and rate after generalized linear modelling}
\usage{adjust(adjust = NULL, by, model, offset=FALSE, 
	type = c("response", "link"), se.fit=TRUE, alpha=.05, 
	ci=FALSE,  ...)
}
\arguments{
	\item{adjust}{expression, indicating independent variable(s) of the model to be adjusted for}
	\item{by}{a list of elements for the grouping variables. The elements of the list will be coerced to factors (if they are not already factors).}
	\item{model}{object of class 'glm' on which the adjustment computation is based}
	\item{offset}{whether the predict results will include the offset term of the model being used for prediction}
	\item{type}{ the type of prediction required.  The default is "response", which uses the scale of the original response variable. For a binomial model, the default estimates are predicted probabilities. For a Poisson model the default estimates are predicted incidence rates if 'offset=FALSE', and predicted counts otherwise.  The alternative "link" transforms the estimates back to the same scale as the linear predictor.}
	\item{se.fit}{whether standard errors to the linear predictors should be returned}
	\item{alpha}{significance level}
	\item{ci}{whether the confidence intervals should be computed}
	\item{...}{additional arguments}
}
\details{Crude means, proportions and rates among different groups are not readily suitable for comparison among subgroups due to potential confounders. Generalized linear modelling (glm) solves the problem of confounders and provides coefficients indicating the level of difference between the specific group and the referent group after adjustment for other independent variables in the model. 

The current function 'adjust' adds on information to an existing 'glm' model. It gives predicted values of subgroups specified in 'by' list. The returned predicted values, if type="response", reflect the magnitude (of mean, proportion and rate) of each subgroup after adjustment for the variable(s) specified in the 'adjust' argument. 

The estimated values are based on each adjusted variable being equal to its grand mean value in the dataset of the model. Variables not included in the 'adjust' argument are set to mean values of each subgroup.}

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
Table.crude.means <- tapply(Price, list(Origin, DriveTrain), mean)
adjust(Horsepower, list(Origin, DriveTrain), model=model1)
a <- adjust(Horsepower, list(Origin, DriveTrain), model=model1)
Table.adjusted.means <- xtabs(mean ~ Origin + DriveTrain, data=a)
# Compare crude means with adjusted means of subgroups
Table.crude.means
Table.adjusted.means

## Now for crude and adjusted probabilities of having manual transmission
manual <- Man.trans.avail =="Yes"
model2 <- glm(manual ~ Origin + Horsepower + DriveTrain, family=binomial)
Table.crude.probabilities <- tapply(manual, list(Origin, DriveTrain), mean)
adjust(Horsepower, list(Origin, DriveTrain), model2)
b <- adjust(Horsepower, list(Origin, DriveTrain), model2)
Table.adjusted.probabilities <- xtabs(probability ~ Origin + DriveTrain, data=b)
# Compare crude and adjusted probabilities
Table.crude.probabilities
Table.adjusted.probabilities


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
