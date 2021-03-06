\name{Unclass factors in a dataframe}
\alias{unclassDataframe}
\title{Unclass factor(s) in the default data frame}
\description{This function unclasses factor(s) in the default data frame (.data). }
\usage{
unclassDataframe (vars, dataFrame = .data)    
}
\arguments{
	\item{vars}{a vector of variables in the data frame, usually factors, that will be unclassed}
  \item{dataFrame}{data frame containing the variables}
}
\details{This function 'unclass'es several variables of class factor to their corresponding integer values. This is useful in further summation of items.
}
\author{Virasakdi Chongsuvivatwong
	\email{ <cvirasak@medicine.psu.ac.th>}
}
\seealso{'unclass', 'alpha', 'tableStack'}
\examples{
expect1 <- c(3,4,3,2,5,3,2,5,2,4,4,3,2,4,4, 
   1,3,2,4,4,4,3,4,2,4,5,4,4,3,4)
expect2 <- c(3,2,4,3,5,3,4,5,4,4,5,5,3,4,4,
   3,4,2,3,5,3,4,4,2,4,5,4,4,3,5)
found1  <- c(1,3,4,3,4,3,3,2,2,4,5,4,3,4,3,
   1,1,2,3,4,4,1,1,3,4,5,4,1,4,2)
found2  <- c(1,1,2,1,3,1,1,2,2,4,3,3,1,1,3,
   3,1,1,2,1,1,1,1,1,3,5,4,4,1,1)
.data <- data.frame(expect1, expect2, found1, found2)
use(.data)
pack() # clean up
des()
level.lab <- list("Very poor"=1, "Poor"=2,
   "Fair"=3, "Good"=4, "Very good"=5)
for (i in 1:4) {
   .data[,i] <- factor(.data[,i])
   levels(.data[,i]) <- level.lab
}
des() # All variables are now factors
unclassDataframe(vars=c(1,4))
des() # Only variables #1 and #4 are 'unclass'ed
}
\keyword{aplot}
