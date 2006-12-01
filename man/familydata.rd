\name{Family data}
\docType{data}
\alias{familydata}
\title{Anthropometric and financial data of a hypothetical family }
\description{
This dataset contains 11 records of family members. Variables include
financial anthropometry and money carried by individuals.
}
\usage{data(familydata)}
\format{A data frame containing 11 observations and 6 variables with
variable description.}
\examples{
data(familydata)
use(familydata)
des()
summ()
age2 <- age^2
plot(age, money, log="y")
dots.of.age <- seq(0,80,0.01)
new.data.frame <- data.frame(age=dots.of.age, age2=dots.of.age^2)
lm1 <- lm(log(money) ~ age + age2)
dots.of.money <- predict.lm(lm1, new.data.frame)
lines(dots.of.age, exp(dots.of.money), col="blue")
}
\keyword{datasets}
    