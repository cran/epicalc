\name{aggregate plot}
\alias{aggregate.plot}
\title{Plot summary statistics of a numeric variable by group}
\description{Split a numeric variable into subsets, plot summary statistics for each}
\usage{
\method{aggregate}{plot}(x, by, grouping = NULL, FUN = c("mean", "median"), 
    error = c("se", "ci", "sd", "none"), alpha = 0.05, lwd = 1, 
    lty = "auto", line.col = "auto", bin.time = 4, bin.method = c("fixed", 
        "quantile"), legend = "auto", legend.site = "topright", 
    legend.bg = "white", xlim = "auto", ylim = "auto", bar.col = "auto", 
    cap.size = 0.02, lagging = 0.007, main = "auto", ...) 
}
\arguments{
       \item{x}{a numeric variable}
       \item{by}{a list of grouping elements for the bar plot, or a single numeric or integer variable which will form the X axis for the time line graph}
       \item{grouping}{further stratification variable for the time line graph}
       \item{FUN}{either "mean" or "median"}
       \item{error}{statistic to use for error lines (either 'se' or 'sd' for barplot, or 'ci' or 'none' for time line graph). When FUN = "median", can only be 'IQR' (default) or 'none'.}
       \item{alpha}{level of significance for confidence intervals}
       \item{lwd}{relative width of the "time" lines. See 'lwd' in ?par}
       \item{lty}{type of the "time" lines. See 'lty' in ?par}
       \item{line.col}{colour(s) of the error and time lines}
       \item{bin.time}{number bins in the time line graph}
       \item{bin.method}{method to allocate the "time" variable into bins, either with 'fixed' interval or equally distributed sample sizes based on quantiles}
       \item{legend}{presence of automatic legend for the time line graph}
       \item{legend.site}{a single character string indicating location of the legend. See details of ?legend}
       \item{legend.bg}{background colour of the legend}
       \item{xlim}{X axis limits}
       \item{ylim}{Y axis limits}
       \item{bar.col}{bar colours}
       \item{cap.size}{relative length of terminating cross-line compared to the range of X axis}
       \item{lagging}{lagging value of the error bars of two adjecant categories at the same time point. The value is result of dividing this distance with the range of X axis}
       \item{main}{main title of the graph}
       \item{...}{additional graphic parameters passed on to other methods}
}
\details{This function plots aggregated values of 'x' by a factor (barplot) or a continuous variable (time line graph).

When 'by' is of class 'factor', a bar plot with error bars is displayed.

When 'by' is a continuous variable (typically implying time), a time line graph is displayed.

Both types of plots have error arguments. Choices are 'se' and 'sd' for the bar plot and 'ci' and IQR for both bar plot and time line graph. All these can be suppressed by specifying 'error'="none".

'bin.time' and 'bin.method' are exclusively used when 'by' is a continuous variable and does not have regular values (minimum frequency of 'by' <3). This condition is automatically and silently detected by 'aggregate.plot' before 'bin.method' chooses the method for aggregation and bin.time determines the number of bins.

If 'legend = TRUE" (by default), a legend box is automatically drawn on the "topright" corner of the graph. This character string can be changed to others such as, "topleft", "center", etc (see examples).

'cap.size' can be zero, if there is not error bar cap needed.
}
\author{Virasakdi Chongsuvivatwong
       \email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'aggregate.data.frame', 'aggregate.numeric', 'tapply'}
\examples{
data(Compaq)
use(Compaq)                                       
aggregate.plot(x=year, by=list(HOSPITAL = hospital, STAGE = stage))
aggregate.plot(x=year, by=list(HOSPITAL = hospital, STAGE = stage), error="sd")
aggregate.plot(x=year, by=list(HOSPITAL = hospital, STAGE = stage), error="ci")
# moving legend and chaging bar colours
aggregate.plot(x=year, by=list(HOSPITAL = hospital, STAGE = stage), error="ci",
  legend.site = "topleft", bar.col = c("red","blue"))
# manual creation of legend
aggregate.plot(x=year, by=list(HOSPITAL = hospital, STAGE = stage), legend = FALSE)
legend(x=7,y=6,legend=c("Public","Private"), fill=grey.colors(2), cex=1.3)
aggregate.plot(x=year, by=list(HOSPITAL = hospital, STAGE = stage), FUN = "median", legend.site = "topleft")

# Example with regular time intervals (all frequencies > 3)
data(Sitka, package="MASS")
use(Sitka)
tab1(Time, graph=FALSE) # all frequencies > 3
aggregate.plot(x=size, by=Time)
aggregate.plot(x=size, by=Time, cap.size = 0) # Note change of error bars
aggregate.plot(x=size, by=Time, grouping=treat)
# For with black and white presentation
aggregate.plot(x=size, by=Time, grouping=treat, lty = 1:2, line.col = c(1,1))
aggregate.plot(x=size, by=Time, grouping=treat, FUN="median", 
  line.col=3:4, lwd =2)
# Compare with boxplot below
boxplot(size ~ treat + Time, col = 3:4, las=3)

# Example with irregular time intervals (some frequencies < 3)
data(BP)
use(BP); des()
age <- as.numeric(as.Date("2008-01-01") - birthdate)/365.25
pack()
tab1(age, graph=FALSE) 
aggregate.plot(x=sbp, by=age)
aggregate.plot(x=sbp, by=age, grouping=saltadd)
aggregate.plot(x=sbp, by=age, grouping=saltadd, bin.method="quantile")
aggregate.plot(x=sbp, by=age, grouping=saltadd, lwd=3, 
  line.col=c("blue","green"))
aggregate.plot(x=sbp, by=age, grouping=saltadd, lwd=3, 
  line.col=c("blue","green") , main = NULL)
title(main="Effect of age and salt adding on SBP", xlab="years",ylab="mm.Hg")
points(age[saltadd=="no"], sbp[saltadd=="no"], col="blue")
points(age[saltadd=="yes"], sbp[saltadd=="yes"], pch=18, col="green")

## For a binary outcome variable
data(Outbreak)
use(Outbreak)
recode(vars = age, old.value = 99, new.value = NA)
aggregate.plot(diarrhea, by=age, bin.time=5)
diarrhea1 <- factor(diarrhea)
levels(diarrhea1) <- c("no","yes")
pack()
aggregate.plot(diarrhea1, by=age, bin.time=5)

}
\keyword{database}

