\name{dotplot}
\alias{dotplot}
\title{Dot plot}
\description{Plot of frequency in dots}
\usage{
dotplot(x, bin = "auto", by = NULL, xmin = NULL, xmax = NULL, 
    time.format = NULL, time.step = NULL, ...) 
}
\arguments{
	\item{x}{a numeric vector. Allowed types also include "Date" and "POSIXct"}
	\item{bin}{number of bins for the range of 'x'}
	\item{by}{stratification variable}
	\item{xmin}{lower bound of x in the graph}
	\item{xmax}{upper bound of x in the graph}
	\item{time.format}{format for time or date at the tick marks}
	\item{time.step}{a character string indicating increment of the sequence of tick marks}
	\item{...}{graphical parameters for the dots when there is no stratification}
}
\details{'dotplot' in Epicalc is similar to a histogram. Each dot represents one record. Attributes of the dots can be further specified in '...' when there is no strafication. Otherwise, the dots are plotted as a diamond shape and the colours are automatically chosen based on the current palette and the number of strata.

When 'bin="auto"' (by default), and the class of the vector is 'integer', 'bin' will be automatically set to max(x)-min(x)+1. This strategy is also applied to all other time and date variables. Users can try other values if the defaults are not to their liking. See the example of 'timeExposed' below.

The argument 'xmin' and 'xmax' indicate the range of x to be displayed on the graph. These two arguments are independent from the value of 'bin', which controls only the number of columns for the original data range. 

Dotplot usually starts the first tick mark on the X-axis at 'xmin' (or min(x) if the 'xmin' is not specified). The argument 'time.step' is typically a character string, containing one of '"sec"', '"min"', '"hour"', '"day"', '"DSTday"', '"week"', '"month"' or '"year"'.  This can optionally be preceded by an integer and a space, or followed by '"s"', such as '"2 weeks"'.

Setting proper 'xmin', 'xmax' and 'time.step' can improve the location of tick marks on the X-axis. The 'time.format' argument can then be given to further improve the graph. See the last two examples for a better understanding.}
\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'summ', 'hist', 'seq.Date' and 'seq.POSIXt'}
\examples{
a <- rep(1:2, 250)
b <- rnorm(500,mean=a)
dotplot(b)
dotplot(b,by=a)

# For the commands below,
# if dates in X axis are not readable, 
# try omitting '#' from the next line
# Sys.setlocale("LC_ALL", "C")

# The number of dots in each column is the frequency
# of 'x' for the exact value on the X axis.
data(Outbreak)
use(Outbreak)
class(age) # numeric
dotplot(age) # 40 columns
age.as.integer <- as.integer(age)
dotplot(age.as.integer)
# 'bin' is the number of columns in the data range.
# Specifying 'min' and 'max' only expands or truncates
# the range of the X axis and has no effect on the distribution
# of the dots inside the data range.
dotplot(age.as.integer, xmin=0, xmax=150) # Just for demonstration.
dotplot(age.as.integer, xmin=0, xmax=70) # the "99"s are now out of the plot.

# Dotplot of a time variable
timeExposed <- ISOdatetime(year=1990, month=8, day=25,
	hour=substr(exptime,9,10), min=substr(exptime,11,12),sec=0)
range(timeExposed, na.rm=TRUE)
max(timeExposed, na.rm=TRUE)-min(timeExposed, na.rm=TRUE)

# The unit of difference between the two time points is "hour"
# Therefore, bin="auto" will produce a dotplot by hour
dotplot(timeExposed) 

# In fact, there are details in 'min'
min <- substr(exptime, 11, 12)
tab1(min, graph=FALSE)

# To create a half-hourly dotplot
# 11:00 to 21:10 requires 21 columns for half-hour intervals.
dotplot(timeExposed, bin=21) 

# For a dotplot of every 15-minutes 41 columns is required
dotplot(timeExposed, bin=41) 

# To display exposure half-hourly in the past 24 hours
dotplot(timeExposed, bin=21, 
	xmin=ISOdatetime(1990,8,25,0,0,0),
	xmax=ISOdatetime(1990,8,26,0,0,0),
	time.step="2 hours", time.format="\%H:\%M")

## Wide range of a variable with 'Date' class
data(BP)
use(BP); des()
dotplot(birthdate)
range(birthdate) #  "1930-11-14" and "1975-12-08"

# There are too many days between these two points of time.
# Users may want to reduce the number of bin, say to 40
dotplot(birthdate, bin=40)

# Setting 'xmin', 'xmax', 'time.step and 'time.format'
# to mark nicer date ticks.
dotplot(birthdate, bin=40, xmin=as.Date("1930-01-01"),
	xmax=as.Date("1976-01-01"), time.step="5 years", 
	time.format="\%Y")
}
\keyword{aplot}