\name{Sort data frame by variable(s)}
\alias{sortBy}
\title{Sort data frame by variable(s)}
\description{Sort the whole dataset by one or more variables}
\usage{
sortBy(..., dataFrame = .data)
}
\arguments{
	\item{...}{index variable(s) used for sorting}
	\item{dataFrame}{Destination data frame where all variables of the same length are sorted}
}
\details{The whole dataset, and the variables outside, can be sorted by an index variable(s) inside the (...).
}
\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'sort', 'order'}
\examples{

sbp <- c(120, 100, 110, 120, 140, 120,  NA,  NA) 
dbp <- c( 80,  80,  70,  80,  70,  NA,  70,  60)
.data <- data.frame(sbp, dbp)
use(.data)
age <- c(37, 32, 24, 33, 31, 30, 26, 25)
age2 <- age^2
sortBy(age)
pack()
des()
.data
sortBy(age, decreasing=TRUE)
.data

## Note that the argument of 'sortBy' must not be concatenated vectors
data(Familydata)
use(Familydata)
.data
sortBy(money, sex) # correct
.data
use(Familydata) # Read in the dataset afresh
sortBy(c(money, sex)) # errors.
.data
}
\keyword{database}