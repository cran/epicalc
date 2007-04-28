\name{aggregate.numeric}
\alias{aggregate.numeric}
\title{Compute summary statistics of a numeric variable}
\description{Split the numeric variable into subsets, computes summary statistics for each, and return the results in a data frame.}
\usage{
\method{aggregate}{numeric}(x, by, FUN=c("length","mean","median","sd","min","max"), 
	na.rm=TRUE, length.warning=TRUE, ...)
}
\arguments{
       \item{x}{a numeric variable}
       \item{by}{a list of grouping elements, each as long as the variables in 'x'.  Names for the grouping variables are provided if they are not given. The elements of the list will be coerced to factors (if they are not already factors).}
       \item{FUN}{scalar functions to compute the summary statistics which can be applied to all data subsets.}
       \item{na.rm}{whether missing values will be removed during the computation of the statistics.}
       \item{length.warning}{warning if x has any missing value}
       \item{...}{additional arguments to 'aggregate'}
}
\details{This is the 'aggregate' method for objects inheriting from class '"numeric"'.

If Epicalc is loaded, applying 'aggregate' to a numeric variable 'x' will call 'aggregate.numeric'. If 'x' is a data frame, 'aggregate.data.frame' will be called as in the usual case of R.

If the Epicalc package is not loaded, 'aggregate', from the stats package, coerces numeric variables (including 'ts' objects) into a data frame and calls 'aggregate.data.frame'.

The 'FUN' argument in 'aggregate.data.frame' can accept only one function.

'aggregate.numeric' takes a different approach. More than one function can be suppplied to the 'FUN' argument, however it can only be applied to a single numeric variable.

'aggregate' in Epicalc is 'backward compatible' with the 'aggregate' function from the stats package. In other words, Epicalc users do not need to change basic syntax or arguments. However, the naming system of the returned object is slightly different. In addition to the ability to provide more statistics in one command, another useful feature of 'aggregate.numeric' in Epicalc is the default values of FUN. Without typing such an argument, 'aggregate.numeric' gives commonly wanted statistics in a shorter line of command.

Note that 'na.rm', the additional argument, is forced to TRUE to allow computation of 'var' and 'sd', when they are in the FUN argument, and 'length' is computed with missing records included. In standard R functions, the equivalent argument is '"na.rm"=TRUE'. Default value of the argument 'length.warning' is TRUE. A condition where 'x' has any missing value will be noticed, which is useful during data exploration. In further analysis, after the missing problem has been recognized, users may change this into FALSE to increase efficiency of output. Both 'na.rm' and 'length.warning' will have no effect if there is not any missing x.
}
\author{Virasakdi Chongsuvivatwong
       \email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'aggregate', 'summ' and 'tapply'}
\examples{
data(Compaq)
use(Compaq)

## 'x' for default aggregate can be a data frame
aggregate(data.frame(id,year), by=list(HOSPITAL=hospital, STAGE=stage),
	FUN="mean")
# The two additional columns are means of 'id' and 'year'

## 'x' in for 'aggregate.numeric' is a numeric vector
aggregate(year, by = list(HOSPITAL = hospital, STAGE = stage), 
	FUN = mean)
# The above line is the same as the below standard command in R

aggregate.data.frame(year, by = list(HOSPITAL = hospital, 
	STAGE = stage), FUN = mean)
# Note the difference in the name of the last column of the returned 
# data frame.

# aggregate in Epicalc can handle multiple functions
aggregate(year, by = list(HOSPITAL = hospital, STAGE = stage), 
	FUN = c("mean", "sd", "length"))

## Handling of missing values
.data$year[8] <- NA
detach(.data); attach(.data)

aggregate(year, by = list(STAGE = stage), FUN = "length")

## 'mean's of subsets in the standard 'aggregrate.data.frame' 
## have 'na.rm' set to FALSE.
aggregate.data.frame(year, by = list(STAGE = stage), FUN = "mean")

## The default value of 'na.rm' is TRUE in aggregate.numeric of Epicalc.
aggregate(year, by = list(STAGE = stage), FUN = c("mean","median"))

## It can be set to FALSE though.
aggregate(year, by = list(STAGE = stage), FUN = c("mean","median"), 
	na.rm=FALSE)

# Omitted the FUN argument can save a lot of time.
aggregate(year, by = list(HOSPITAL = hospital, STAGE = stage))

# Warning of na.rm 
aggregate(year, by = list(HOSPITAL = hospital, STAGE = stage), length.warning=FALSE)
}
\keyword{database}

