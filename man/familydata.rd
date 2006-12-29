\name{familydata}
\alias{familydata}
\docType{data}
\title{ A hypothetical family}
\description{ Anthropometric and financial data of a hypothetical family}
\usage{data(familydata)}
\format{
  A data frame with 11 observations on the following 6 variables.
  \describe{
    \item{\code{code}}{a character vector}
    \item{\code{age}}{a numeric vector}
    \item{\code{ht}}{a numeric vector}
    \item{\code{wt}}{a numeric vector}
    \item{\code{money}}{a numeric vector}
    \item{\code{sex}}{a factor with levels \code{F} \code{M}}
  }
}
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
