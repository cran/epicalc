\name{aggregate plot}
\alias{aggregatePlot}
\title{Plot summary statistics of a numeric variable by group}
\description{Split the numeric variable into subsets, plot summary statistics for each}
\usage{
aggregatePlot(x, by, grouping = NULL, FUN = c("mean", "median"), 
    error = c("se", "ci", "sd", "none"), alpha = 0.05, line.width = 1, 
    line.col = "auto", bin.time = 4, bin.method = c("fixed", 
        "quantile"), legend = "auto", xlim = "auto", ylim = "auto", 
    ...) 
}
\arguments{
       \item{x}{a numeric variable}
       \item{by}{a list like 'by' argument for 'aggregate.numeric' when, otherwise a single numeric or integer variable which will be X axis for time line graph}
       \item{grouping}{stratification variable for time line graph}
       \item{FUN}{either "mean" or "median"}
       \item{error}{statistic for error lines on top of the bar plots, which can be 'se', 'sd', or surrounding the point estimates, which can be 'ci' or 'none'. When FUN = "median", the error would only automatically be IQR or can be specified as "none'.}
       \item{alpha}{levels of significance when the error bars are chosen to be "ci"}
       \item{line.width}{relative width of the lines}
       \item{line.col}{colour(s) of the error lines and the time lines}
       \item{bin.time}{number bins in the time line graph}
       \item{bin.method}{method to allocate time variable into time bin, either with 'fixed' interval or equally distributed sample size based on 'quantile' function.}
       \item{legend}{presence of automatic legend for the time line graph}
       \item{xlim}{limits of X axis}
       \item{ylim}{limits of Y axis}
       \item{...}{additional arguments passed on to  graphic parameters}
}
\details{This function diplays aggregate value of 'x' by a factor or a continuous variable most commonly related to time.

When 'by' is of class 'factor', bar plots with error values are displayed.

When 'by' is a continuous variable (typically implying time), a (time) line graph is displayed.

Both types of aggregatePlot have error arguments. Choices are 'se' and 'sd' for the bar plots and 'ci' and IQR for both bar plots and time line graph. All these could be suppressed with 'error="none".

'bin.time' and 'bin.method' are exclusively for 'by' continuous variable when it does not have regular values. This condition is automatically and silently detected by 'aggregatePlot' before 'bin.method' chooses the method for aggregation and time.bin determines the number of bin.
}
\author{Virasakdi Chongsuvivatwong
       \email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'aggregate', 'summ' and 'tapply'}
\examples{
data(Compaq)
use(Compaq)                                       
aggregatePlot(x=year, by=list(HOSPITAL = hospital, STAGE = stage))
legend(x=7,y=6,legend=c("Public","Private"), fill=c("grey45","grey75"), cex=1.3)
title(main="Means and standard errors of follow-up duration \nby stage and type of hospital")

data(Sitka, package="MASS")
use(Sitka)
aggregatePlot(x=size, by=Time)
aggregatePlot(x=size, by=Time, grouping=treat)
aggregatePlot(x=size, by=Time, grouping=treat, FUN="median", 
  line.col=3:4, line.width =2)
# The above shows median and IQR
# Compare with boxplot below
boxplot(size ~ treat + Time, col = 3:4, las=3)

data(BP)
use(BP); des()
age <- as.numeric(as.Date("2008-01-01") - birthdate)/365.25
aggregatePlot(x=sbp, by=age)
aggregatePlot(x=sbp, by=age, grouping=saltadd)
aggregatePlot(x=sbp, by=age, grouping=saltadd, bin.method="quantile")
aggregatePlot(x=sbp, by=age, grouping=saltadd, line.width=3, 
  line.col=c("blue","green"))
title(main="Mean and 95\%CI of SBP by age", xlab="years",ylab="mm.Hg"
)
points(age[saltadd=="no"], sbp[saltadd=="no"], col="blue")
points(age[saltadd=="yes"], sbp[saltadd=="yes"], pch=18, col="green")
}
\keyword{database}

